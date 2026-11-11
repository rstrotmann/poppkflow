test_that("pk_bootstrap stores replicates, resumes, and summarises", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2, inits = list(cl = 10)))
  d <- example_data()
  est <- pk_est(method = "saem")
  b <- pk_bootstrap(r1, n = 3, strata = "SEXF", data = d, est = est, fit_fun = stub_fit_fun(), quiet = TRUE)
  expect_equal(nrow(b), 3L)
  expect_true(all(b$ok))
  expect_true(all(c("sample", "ok", "ofv", "lcl", "lv2", "prop.sd", "om.cl", "om.v2") %in% names(b)))

  b5 <- pk_bootstrap(r1, n = 5, strata = "SEXF", data = d, est = est, fit_fun = stub_fit_fun(), quiet = TRUE)
  expect_equal(b5$sample, 1:5)
  expect_equal(pk_audit(run = "run001")$event[length(pk_audit(run = "run001")$event)], "bootstrap")

  sm <- pk_bootstrap_summary(r1)
  expect_equal(sm$Final[sm$Code == "lcl"], 10)
  expect_equal(sm$`Bootstrap median`[sm$Code == "lcl"], 10)
  expect_equal(sm$`Bias (%)`[sm$Code == "lcl"], 0)
  expect_equal(unique(sm$`n successful`), 5L)
  expect_true("om.cl" %in% sm$Code)
})


test_that("pk_bootstrap records failed replicates", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  calls <- 0
  flaky <- function(model, data, est) {
    calls <<- calls + 1
    if (calls == 2) stop("no convergence")
    stub_fit_fun()(model, data, est)
  }
  b <- pk_bootstrap(r1, n = 3, data = example_data(), est = pk_est(method = "saem"), fit_fun = flaky, quiet = TRUE)
  expect_equal(b$ok, c(TRUE, FALSE, TRUE))
  expect_equal(unique(pk_bootstrap_summary(r1)$`n total`), 3L)
  expect_equal(unique(pk_bootstrap_summary(r1)$`n successful`), 2L)
})


test_that("the resampling keeps the stratum sizes", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  d <- as.data.frame(tibble::tribble(
    ~ID, ~TIME, ~DV , ~AMT, ~EVID, ~G,
    1  , 0    , NA  , 100 , 1    , "a",
    1  , 1    , 1.0 , 0   , 0    , "a",
    2  , 0    , NA  , 100 , 1    , "a",
    2  , 1    , 1.1 , 0   , 0    , "a",
    3  , 0    , NA  , 100 , 1    , "b",
    3  , 1    , 1.2 , 0   , 0    , "b",
    4  , 0    , NA  , 100 , 1    , "b",
    4  , 1    , 1.3 , 0   , 0    , "b"
  ))
  seen <- list()
  spy <- function(model, data, est) {
    seen[[length(seen) + 1L]] <<- data
    stub_fit_fun()(model, data, est)
  }
  pk_bootstrap(r1, n = 4, strata = "G", data = d, est = pk_est(method = "saem"), fit_fun = spy, quiet = TRUE)
  for (bs in seen) {
    expect_equal(length(unique(bs$ID)), 4L)
    expect_equal(sum(!duplicated(bs$ID) & bs$G == "a"), 2L)
    expect_equal(nrow(bs), 8L)
  }
})


test_that("pk_import_legacy imports fits, code and parents without touching the source", {
  proj <- local_project()
  src <- withr::local_tempdir()
  spec1 <- pk_spec(ncmt = 2)
  spec2 <- pk_add_iiv(spec1, "ka")
  code1 <- pk_code(spec1)
  code2 <- pk_code(spec2)
  f1 <- stub_fit_fun()(pk_compile(spec1), example_data(), NULL)
  f2 <- stub_fit_fun()(pk_compile(spec2), example_data(), NULL)
  saveRDS(f1, file.path(src, "run001.rds"))
  saveRDS(f2, file.path(src, "run002.rds"))
  writeLines(code1, file.path(src, "run001.txt"))
  writeLines(code2, file.path(src, "run002.txt"))
  saveRDS(list(not = "a fit"), file.path(src, "run003.rds"))
  hist <- file.path(src, "history.csv")
  readr::write_csv(
    tibble::tribble(
      ~Run   , ~Reference, ~Description,
      "run001", NA        , "Base"       ,
      "run002", "run001"  , "Add IIV KA"
    ),
    hist
  )
  before <- list.files(src)

  ids <- pk_import_legacy(src, history = hist, quiet = TRUE)
  expect_equal(ids, c("run001", "run002"))
  expect_equal(list.files(src), before)

  runs <- pk_runs()
  expect_equal(runs$run_id, c("run001", "run002"))
  expect_equal(runs$parent_id, c(NA, "run001"))
  expect_equal(runs$description, c("Base", "Add IIV KA"))
  expect_equal(runs$stage, c("legacy", "legacy"))
  expect_equal(runs$dofv_parent, c(NA, -20))
  expect_equal(pk_result("run002")$ofv, 980)
  expect_true(file.exists(file.path(proj$path, "models", "run001", "model.txt")))

  # imported runs can be used as parent (code is stored as a custom specification)
  expect_s3_class(readRDS(file.path(proj$path, "models", "run001", "spec.rds")), "pk_spec")

  # a second import does not duplicate runs
  expect_equal(length(pk_import_legacy(src, history = hist, quiet = TRUE)), 0L)
  expect_equal(nrow(pk_runs()), 2L)
})


test_that("pk_report renders when pandoc is available", {
  skip_if_not_installed("rmarkdown")
  skip_if_not_installed("knitr")
  skip_if(inherits(try(ensure_pandoc(), silent = TRUE), "try-error"), "pandoc not available")
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2), note = "Base model", stage = "structural")
  r2 <- new_run(proj, pk_add_iiv(r1, "ka"), note = "IIV on KA", stage = "stochastic")
  pk_decide(r2, "accepted", reason = "Better fit", quiet = TRUE)
  out <- pk_report(r2, output_dir = file.path(proj$path, "report"))
  expect_true(file.exists(out))
  html <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(html, "Model development history")
  expect_match(html, "Better fit")
})
