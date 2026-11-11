make_result <- function(...) {
  over <- list(...)
  res <- list(
    ofv = 100, cov_step = TRUE, cond_no = 50, eta_shrink = c(eta.cl = 10, eta.v2 = 12), eps_shrink = 5,
    parameters = tibble::tribble(
      ~Type                      , ~Code    , ~Estimate, ~`RSE (%)`,
      "Fixed effect"             , "lcl"    , 5        , 8         ,
      "Fixed effect"             , "lv2"    , 30       , 12        ,
      "Random effect (variance)" , "eta.cl" , 0.1      , NA        ,
      "Random effect (variance)" , "eta.v2" , 0.12     , NA
    ),
    cwres = list(n = 100, mean = 0.02, sd = 1, pct_gt2 = 4, pct_gt3 = 0.5)
  )
  for (nm in names(over)) res[[nm]] <- over[[nm]]
  res
}


test_that("a clean result has no flags", {
  expect_equal(nrow(flags_from_result(make_result())), 0L)
})


test_that("each rule triggers its flag", {
  bad <- make_result(
    cov_step = FALSE, cond_no = 5000, eta_shrink = c(eta.cl = 10, eta.ka = 55), eps_shrink = 40,
    parameters = tibble::tribble(
      ~Type                      , ~Code    , ~Estimate, ~`RSE (%)`,
      "Fixed effect"             , "lcl"    , 5        , 80        ,
      "Random effect (variance)" , "eta.cl" , 0.00001  , NA
    ),
    cwres = list(n = 100, mean = 0.8, sd = 1, pct_gt2 = 8, pct_gt3 = 5)
  )
  fl <- flags_from_result(bad)
  expect_setequal(
    fl$flag,
    c("cov_step_failed", "condition_number", "eta_shrinkage", "eps_shrinkage", "high_rse", "omega_near_zero", "cwres_bias", "cwres_tails")
  )
  expect_equal(fl$parameter[fl$flag == "eta_shrinkage"], "eta.ka")
  expect_equal(fl$value[fl$flag == "high_rse"], 80)
})


test_that("a missing OFV is an error flag", {
  fl <- flags_from_result(make_result(ofv = NA_real_))
  expect_equal(fl$severity[fl$flag == "no_ofv"], "error")
})


test_that("thresholds can be overridden", {
  res <- make_result(cond_no = 500)
  expect_equal(nrow(flags_from_result(res)), 0L)
  fl <- flags_from_result(res, utils::modifyList(default_thresholds(), list(max_condition_number = 100)))
  expect_equal(fl$flag, "condition_number")
})


test_that("pk_flags and pk_suggest read the stored result", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2, iiv = c("ka", "cl", "v2")), fit_fun = stub_fit_fun(shrink_eta = 60))
  fl <- pk_flags(r1)
  expect_true("eta_shrinkage" %in% fl$flag)
  expect_equal(pk_runs()$n_flags, nrow(fl))
  hints <- pk_suggest(r1)
  expect_true(any(grepl("removing the random effect", hints)))
  # thresholds from the project config are used
  fl2 <- pk_flags(r1, thresholds = list(max_shrinkage_eta = 90, max_shrinkage_eps = 90))
  expect_false("eta_shrinkage" %in% fl2$flag)
})


test_that("pk_diagnostics returns summary and flags and stores them", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  d <- pk_diagnostics(r1)
  expect_s3_class(d, "pk_diagnostics")
  expect_equal(d$summary$ofv, 1000)
  expect_equal(d$summary$n_flags, nrow(d$flags))
  expect_equal(readRDS(file.path(proj$path, "models", "run001", "diag.rds"))$summary$ofv, 1000)
})


test_that("pk_compare gives dOFV, df, LRT p-value and the reference", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  r2 <- new_run(proj, pk_add_iiv(r1, "ka"))
  r3 <- new_run(proj, pk_set_error(r1, "comb"))
  cmp <- pk_compare(r1, r2, r3)
  expect_equal(cmp$run_id, c("run001", "run002", "run003"))
  expect_equal(cmp$reference, c(NA, "run001", "run001"))
  expect_equal(cmp$dofv, c(NA, -20, -5))
  expect_equal(cmp$df, c(NA, 1L, 1L))
  expect_equal(cmp$p_lrt[2], stats::pchisq(20, 1, lower.tail = FALSE))
  expect_true(cmp$comparable[2])

  first <- pk_compare(r1, r2, ref = "first")
  expect_equal(first$reference, c(NA, "run001"))
})


test_that("pk_compare shows no p-value for a worse model and flags different data", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2, iiv = c("ka", "cl", "v2")))
  r2 <- new_run(proj, pk_drop_iiv(r1, "ka"))
  cmp <- pk_compare(r1, r2)
  expect_equal(cmp$dofv[2], 20)
  expect_true(is.na(cmp$p_lrt[2]))

  d2 <- example_data()
  d2$DV[2] <- 9
  r3 <- pk_run(pk_add_iiv(r1, "q"), data = d2, est = pk_est(method = "focei"), fit_fun = stub_fit_fun(), quiet = TRUE)
  expect_true(pk_compare(r1, r3, ref = "first")$comparable[2] %in% FALSE)
})


test_that("pk_compare_params puts estimates side by side with the relative change", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2, inits = list(cl = 10)))
  r2 <- new_run(proj, pk_spec(ncmt = 2, inits = list(cl = 12)))
  out <- pk_compare_params(r1, r2)
  row <- out[out$Code == "lcl", ]
  expect_equal(row$run001, 10)
  expect_equal(row$run002, 12)
  expect_equal(row$`change_run002 (%)`, 20)
})
