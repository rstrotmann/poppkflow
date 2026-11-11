scm_candidates <- function() {
  tibble::tribble(
    ~param, ~covariate, ~form   , ~ref, ~init,
    "cl"  , "WT"      , "power" , 70  , 0.75 ,
    "cl"  , "CRCL"    , "power" , 100 , 0.1  ,
    "cl"  , "AGE"     , "linear", 50  , 0    ,
    "v2"  , "WT"      , "power" , 70  , 1
  )
}


test_that("pk_scm runs forward inclusion and backward elimination through the ledger", {
  proj <- local_project()
  base <- new_run(proj, pk_spec(ncmt = 2), note = "Base", stage = "stochastic")
  scm <- pk_scm(
    base, scm_candidates(),
    forward_p = 0.05, backward_p = 0.001, data = example_data(), est = pk_est(method = "focei"),
    fit_fun = stub_fit_fun(), quiet = TRUE
  )
  expect_s3_class(scm, "pk_scm")
  # forward: CL~WT (-30), then CL~CRCL (joint -36, gain 6 > 3.84); backward removes CL~CRCL (increase 6 < 10.83)
  expect_equal(scm$forward_selected, c("CL~WT", "CL~CRCL"))
  expect_equal(scm$selected, "CL~WT")

  steps <- scm$steps
  fwd1 <- steps[steps$Step == "forward", ][1:4, ]
  expect_equal(fwd1$Tested, c("CL~WT", "CL~CRCL", "CL~AGE", "V2~WT"))
  expect_equal(fwd1$dOFV, c(-30, -27, -2, -3))
  expect_equal(fwd1$Selected, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(unique(fwd1$Reference), "run001")
  expect_equal(steps$Tested[steps$Step == "backward" & steps$Selected], "CL~CRCL")

  runs <- pk_runs()
  expect_true(all(runs$stage[-1] == "scm"))
  expect_true(all(runs$parent_id[-1] %in% runs$run_id))
  # final run is a model with only CL~WT
  final_spec <- readRDS(file.path(proj$path, "models", scm$final$run_id, "spec.rds"))
  expect_equal(final_spec$covariates$id, "CL~WT")
  # the step table and the scm file are stored
  expect_true(file.exists(file.path(proj$path, "scm", "scm01.rds")))
  expect_true("scm_completed" %in% pk_audit()$event)
})


test_that("pk_scm marks non-selected trial runs as rejected with the dOFV as reason", {
  proj <- local_project()
  base <- new_run(proj, pk_spec(ncmt = 2))
  scm <- pk_scm(
    base, scm_candidates(),
    forward_p = 0.05, backward_p = 0.001, data = example_data(), est = pk_est(method = "focei"),
    fit_fun = stub_fit_fun(), quiet = TRUE
  )
  runs <- pk_runs()
  rej <- runs[runs$decision == "rejected", ]
  expect_true(nrow(rej) > 0)
  expect_true(all(grepl("^SCM (forward|backward) step", rej$decision_reason)))
  # selected path stays pending
  on_path <- scm$steps$Run[scm$steps$Selected]
  expect_true(all(runs$decision[runs$run_id %in% on_path] == "pending"))
})


test_that("pk_scm stops when no candidate meets the criterion", {
  proj <- local_project()
  base <- new_run(proj, pk_spec(ncmt = 2))
  cands <- tibble::tribble(
    ~param, ~covariate, ~form   , ~ref, ~init,
    "cl"  , "AGE"     , "linear", 50  , 0    ,
    "v2"  , "WT"      , "power" , 70  , 1
  )
  scm <- pk_scm(
    base, cands,
    data = example_data(), est = pk_est(method = "focei"), fit_fun = stub_fit_fun(), quiet = TRUE
  )
  expect_equal(scm$selected, character())
  expect_equal(scm$final$run_id, "run001")
  expect_equal(nrow(scm$steps), 2L)
})


test_that("pk_scm respects require_cov_step", {
  proj <- local_project()
  base <- new_run(proj, pk_spec(ncmt = 2), fit_fun = stub_fit_fun(cov_step = FALSE))
  cands <- scm_candidates()[1, ]
  scm <- pk_scm(
    base, cands,
    data = example_data(), est = pk_est(method = "focei"), fit_fun = stub_fit_fun(cov_step = FALSE), quiet = TRUE
  )
  expect_equal(scm$selected, character())
  scm2 <- pk_scm(
    base, cands,
    require_cov_step = FALSE, data = example_data(), est = pk_est(method = "focei"),
    fit_fun = stub_fit_fun(cov_step = FALSE), quiet = TRUE
  )
  expect_equal(scm2$selected, "CL~WT")
})


test_that("pk_scm warns about failing trials and continues", {
  proj <- local_project()
  base <- new_run(proj, pk_spec(ncmt = 2))
  cands <- tibble::tribble(
    ~param, ~covariate, ~form   , ~ref, ~init,
    "cl"  , "WT"      , "power" , 70  , 0.75 ,
    "v2"  , "WT"      , "power" , 70  , 1
  )
  warns <- character()
  scm <- withCallingHandlers(
    pk_scm(
      base, cands,
      data = example_data(), est = pk_est(method = "focei"),
      fit_fun = stub_fit_fun(fail_if = "v2_wt"), quiet = TRUE
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_true(any(grepl("failed", warns)))
  expect_equal(scm$selected, "CL~WT")
  expect_true("failed" %in% pk_runs()$status)
})


test_that("pk_scm rejects fixed or custom candidates", {
  proj <- local_project()
  base <- new_run(proj, pk_spec(ncmt = 2))
  fixed <- tibble::tribble(
    ~param, ~covariate, ~form   , ~ref, ~init, ~fix,
    "cl"  , "WT"      , "power" , 70  , 0.75 , TRUE
  )
  expect_error(pk_scm(base, fixed, data = example_data(), fit_fun = stub_fit_fun(), quiet = TRUE), "fixed")
})


test_that("pk_multistart runs every start and marks the best one", {
  proj <- local_project()
  starts <- tibble::tribble(
    ~set, ~ka, ~v2, ~q,
    "S1", 1.0, 30 , 2 ,
    "S2", 2.0, 100, 5 ,
    "S3", 0.5, 25 , 1
  )
  ms <- pk_multistart(
    pk_spec(ncmt = 2), starts,
    data = example_data(), est = pk_est(method = "focei"), fit_fun = stub_fit_fun(), quiet = TRUE
  )
  expect_equal(ms$set, c("S1", "S2", "S3"))
  expect_equal(ms$ka, c(1, 2, 0.5))
  expect_equal(ms$v2, c(30, 100, 25))
  expect_true(all(pk_runs()$stage == "multistart"))
  expect_equal(nrow(pk_runs()), 3L)
  expect_s3_class(attr(ms, "best_run"), "pk_run_ref")
})


test_that("pk_repro repeats the fit with other seeds and perturbed starts", {
  proj <- local_project()
  r1 <- pk_run(pk_spec(ncmt = 2), data = example_data(), est = pk_est(method = "saem_focei", seed = 2026), fit_fun = stub_fit_fun(), quiet = TRUE)
  rep <- pk_repro(
    r1,
    seeds = c(2026, 7, 99), perturb_starts = 2, data = example_data(), fit_fun = stub_fit_fun(), quiet = TRUE
  )
  expect_equal(nrow(rep), 5L)
  expect_equal(rep$method, c("saem_focei", "saem_focei", "saem_focei", "focei", "focei"))
  expect_equal(rep$run_id[1], "run001")
  runs <- pk_runs()
  expect_equal(sum(runs$stage %in% "repro"), 4L)
  expect_true(all(runs$parent_id[runs$stage %in% "repro"] == "run001"))
  expect_equal(attr(rep, "ofv_range"), c(1000, 1000))
  # perturbed starts differ
  expect_true(length(unique(rep$ka[4:5])) == 2L)
})


test_that("pk_screen_covariates tests ETAs against covariates", {
  proj <- local_project()
  pk_covariates(tibble::tribble(
    ~name , ~type         , ~ref,
    "WT"  , "continuous"  , 70  ,
    "SEXF", "categorical" , NA
  ))
  n <- 30
  set.seed(1)
  d <- data.frame(ID = rep(seq_len(n), each = 2), TIME = rep(c(0, 1), n), DV = NA_real_, AMT = 0, EVID = 0L)
  d$WT <- rep(round(rnorm(n, 70, 10)), each = 2)
  d$SEXF <- rep(rbinom(n, 1, 0.5), each = 2)
  ff <- function(model, data, est) {
    fit <- stub_fit_fun()(model, data, est)
    wt <- data$WT[!duplicated(data$ID)]
    fit$eta$eta.cl <- 0.02 * (wt - 70) + rnorm(length(wt), 0, 0.05)
    fit
  }
  r1 <- pk_run(pk_spec(ncmt = 2), data = d, est = pk_est(method = "focei"), fit_fun = ff, quiet = TRUE)
  scr <- pk_screen_covariates(r1, data = d)
  expect_equal(sort(unique(scr$ETA)), c("eta.cl", "eta.v2"))
  expect_equal(nrow(scr), 4L)
  wt_cl <- scr[scr$ETA == "eta.cl" & scr$Covariate == "WT", ]
  expect_equal(wt_cl$Test, "Pearson r")
  expect_gt(wt_cl$Statistic, 0.9)
  expect_lt(wt_cl$p, 0.001)
  sex <- scr[scr$Covariate == "SEXF", ]
  expect_equal(sex$Test, c("Wilcoxon", "Wilcoxon"))
  expect_true(all(sex$`Shrinkage (%)` == 10))
  p <- pk_plot_eta_cov(r1, data = d)
  expect_type(p, "list")
})
