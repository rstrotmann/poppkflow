# Integration tests with real nlmixr2 fits on the simulated example data (about 1-3 minutes).
# Run with: POPPKFLOW_SLOW=true Rscript -e 'devtools::test()'

skip_if_not(identical(Sys.getenv("POPPKFLOW_SLOW"), "true"), "set POPPKFLOW_SLOW=true to run the slow tests")


setup_real_project <- function(env = parent.frame()) {
  proj <- local_project(env)
  pk_covariates(tibble::tribble(
    ~name , ~type         , ~ref,
    "WT"  , "continuous"  , 70  ,
    "CRCL", "continuous"  , 100 ,
    "AGE" , "continuous"  , 50  ,
    "SEXF", "categorical" , NA
  ))
  d <- pk_data(system.file("extdata", "example_pk.csv", package = "poppkflow"))
  list(proj = proj, data = d)
}


test_that("the iterative loop works with real fits: base, covariate, compare, decide", {
  s <- setup_real_project()
  est <- pk_est(method = "focei")
  base_spec <- pk_spec(
    ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"),
    inits = list(ka = 1, cl = 5, v2 = 30, q = 4, v3 = 50, tlag = 0.25)
  )
  r1 <- pk_run(base_spec, data = s$data, est = est, note = "Base", stage = "structural", quiet = TRUE)
  expect_true(is.finite(pk_runs()$ofv[1]))
  expect_true(pk_runs()$cov_step[1])

  # covariate model started from the final estimates of the parent
  cand <- r1 |>
    pk_add_cov("cl", "WT", form = "power", init = 0.75) |>
    pk_add_cov("v2", "WT", form = "power", init = 1) |>
    pk_inits_from(r1)
  r2 <- pk_run(cand, data = s$data, est = est, note = "WT on CL and V2", stage = "covariate", quiet = TRUE)
  cmp <- pk_compare(r1, r2)
  expect_equal(cmp$reference[2], "run001")
  expect_equal(cmp$df[2], 2L)
  expect_lt(cmp$dofv[2], -6.63)

  pt <- pk_params_table(r2)
  cl_wt <- pt$Estimate[pt$Code == "cl_wt"]
  expect_gt(cl_wt, 0.3)
  expect_lt(cl_wt, 1.3)

  pk_decide(r2, "accepted", reason = "WT effect on CL and V2 is significant", quiet = TRUE)
  expect_equal(pk_runs()$decision, c("pending", "accepted"))

  # diagnostics and plots
  dg <- pk_diagnostics(r2, plots = TRUE)
  expect_s3_class(dg, "pk_diagnostics")
  expect_true(file.exists(file.path(s$proj$path, "models", "run002", "diag", "gof.png")))
  expect_s3_class(pk_plot_eta(r2), "ggplot")

  # screening of the ETAs of the base model
  scr <- pk_screen_covariates(r1, data = s$data)
  expect_true(any(scr$ETA == "eta.cl" & scr$Covariate == "WT"))
  wt_cl <- scr[scr$ETA == "eta.cl" & scr$Covariate == "WT", ]
  expect_equal(wt_cl$Test, "Pearson r")
  expect_gt(wt_cl$Statistic, 0)

  # cache
  again <- pk_run(cand, data = s$data, est = est, quiet = TRUE)
  expect_equal(again$run_id, "run002")

  # exposure
  expo <- pk_exposure(r2, dose = 100, ii = 24, n_doses = 10, data = s$data)
  expect_equal(nrow(expo), 40L)
  expect_true(all(c("Cmax_ss", "Cmin_ss", "AUC_ss", "t_half_beta", "Vss", "WT") %in% names(expo)))
  expect_true(all(expo$Cmax_ss > expo$Cmin_ss))
  summ <- pk_exposure_summary(expo)
  expect_true(all(c("Geometric mean", "Geometric CV (%)") %in% names(summ)))
})


test_that("SAEM followed by FOCEI works, and SAEM alone gives an OFV", {
  s <- setup_real_project()
  spec <- pk_spec(
    ncmt = 2, lag = TRUE, iiv = c("cl", "v2"),
    inits = list(ka = 1, cl = 5, v2 = 30, q = 4, v3 = 50, tlag = 0.25)
  )
  r <- pk_run(spec, data = s$data, est = pk_est(method = "saem_focei", nBurn = 100, nEm = 100), quiet = TRUE)
  res <- pk_result(r)
  expect_true(is.finite(res$ofv))
  expect_true(res$cov_step)

  r2 <- pk_run(spec, data = s$data, est = pk_est(method = "saem", nBurn = 100, nEm = 100), quiet = TRUE)
  expect_true(is.finite(pk_result(r2)$ofv))
  expect_equal(pk_runs()$est_method, c("saem_focei", "saem"))
})


test_that("pk_scm selects the WT effect on CL with real fits", {
  s <- setup_real_project()
  est <- pk_est(method = "focei")
  spec <- pk_spec(
    ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"),
    inits = list(ka = 1, cl = 5, v2 = 30, q = 4, v3 = 50, tlag = 0.25)
  )
  base <- pk_run(spec, data = s$data, est = est, stage = "stochastic", quiet = TRUE)
  cands <- tibble::tribble(
    ~param, ~covariate, ~form   , ~ref, ~init,
    "cl"  , "WT"      , "power" , 70  , 0.75 ,
    "cl"  , "SEXF"    , "cat"   , NA  , 0
  )
  scm <- pk_scm(base, cands, data = s$data, est = est, require_cov_step = FALSE, quiet = TRUE)
  expect_true("CL~WT" %in% scm$forward_selected)
  expect_false("CL~SEXF" %in% scm$selected)
  expect_true(all(pk_runs()$stage[-1] == "scm"))
})


test_that("pk_bootstrap and pk_bootstrap_summary run on a few replicates", {
  s <- setup_real_project()
  spec <- pk_spec(
    ncmt = 2, lag = TRUE, iiv = c("cl", "v2"),
    inits = list(ka = 1, cl = 5, v2 = 30, q = 4, v3 = 50, tlag = 0.25)
  )
  r <- pk_run(spec, data = s$data, est = pk_est(method = "focei"), quiet = TRUE)
  b <- pk_bootstrap(r, n = 2, data = s$data, est = pk_est(method = "saem", nBurn = 60, nEm = 60), quiet = TRUE)
  expect_equal(nrow(b), 2L)
  expect_true(any(b$ok))
  sm <- pk_bootstrap_summary(r)
  expect_true("lcl" %in% sm$Code)
  expect_true(all(sm$`n total` == 2L))
  b3 <- pk_bootstrap(r, n = 3, data = s$data, est = pk_est(method = "saem", nBurn = 60, nEm = 60), quiet = TRUE)
  expect_equal(sort(b3$sample), 1:3)
})


test_that("pk_vpc returns a plot", {
  skip_if_not_installed("nlmixr2plot")
  skip_if_not_installed("vpc")
  s <- setup_real_project()
  spec <- pk_spec(
    ncmt = 2, lag = TRUE, iiv = c("cl", "v2"),
    inits = list(ka = 1, cl = 5, v2 = 30, q = 4, v3 = 50, tlag = 0.25)
  )
  r <- pk_run(spec, data = s$data, est = pk_est(method = "focei"), quiet = TRUE)
  p <- pk_vpc(r, n = 30)
  expect_s3_class(p, "ggplot")
  expect_true(file.exists(file.path(s$proj$path, "models", "run001", "eval", "vpc.rds")))
})
