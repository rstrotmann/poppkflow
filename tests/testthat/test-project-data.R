test_that("pk_project creates the folder structure and can be re-opened", {
  proj <- local_project()
  expect_true(all(dir.exists(file.path(proj$path, c("data", "ledger", "models", "scm", "output", "report")))))
  expect_true(file.exists(file.path(proj$path, "config.yml")))
  again <- pk_open(proj$path)
  expect_equal(again$name, "test")
  expect_equal(nrow(pk_runs()), 0L)
})


test_that("pk_open fails without a config", {
  expect_error(pk_open(withr::local_tempdir()), "config.yml")
})


test_that("the covariate dictionary round-trips", {
  proj <- local_project()
  dict <- tibble::tribble(
    ~name , ~type         , ~unit , ~ref, ~min, ~max,
    "WT"  , "continuous"  , "kg"  , 70  , 30  , 200 ,
    "SEXF", "categorical" , NA    , NA  , NA  , NA
  )
  pk_covariates(dict)
  out <- pk_covariates()
  expect_equal(out$name, c("WT", "SEXF"))
  expect_equal(out$ref, c(70, NA))
  expect_equal(out$max, c(200, NA))
  expect_equal(out$type, c("continuous", "categorical"))
})


test_that("pk_data registers, copies and reads a dataset with a hash", {
  proj <- local_project()
  file <- file.path(withr::local_tempdir(), "d.csv")
  tbl <- tibble::tribble(
    ~ID, ~TIME, ~DV , ~AMT, ~EVID,
    1  , 0    , NA  , 100 , 1    ,
    1  , 1    , 2.5 , 0   , 0
  )
  readr::write_csv(tbl, file, na = ".")
  d <- pk_data(file)
  expect_equal(nrow(d), 2L)
  expect_true(is.na(d$DV[1]))
  expect_true(file.exists(file.path(proj$path, "data", "d.csv")))
  expect_equal(pk_data()$DV, d$DV)
  expect_equal(attr(d, "data_hash"), attr(pk_data(), "data_hash"))
})


test_that("pk_check finds no problem in a clean dataset", {
  proj <- local_project()
  expect_equal(nrow(pk_check(example_data())), 0L)
})


test_that("pk_check reports data problems", {
  proj <- local_project()
  pk_covariates(tibble::tribble(
    ~name, ~type        , ~ref, ~min, ~max,
    "WT" , "continuous" , 70  , 30  , 150
  ))
  d <- as.data.frame(tibble::tribble(
    ~ID, ~TIME, ~DV , ~AMT, ~EVID, ~WT,
    1  , 1    , 2.0 , 0   , 0    , 70 ,
    1  , 0    , NA  , 100 , 1    , 70 ,
    1  , 2    , 0   , 0   , 0    , 70 ,
    1  , 2    , 1.0 , 0   , 0    , 70 ,
    2  , 0    , NA  , 100 , 1    , NA ,
    2  , 1    , 1.5 , 0   , 0    , 400,
    3  , 0    , NA  , 100 , 1    , 70
  ))
  out <- pk_check(d)
  expect_true(all(c("time_order", "dv_non_positive", "duplicated_records", "covariate_range", "covariate_missing_values", "subject_without_obs") %in% out$check))
  expect_equal(out$severity[out$check == "time_order"], "error")
})


test_that("pk_check reports missing required columns", {
  proj <- local_project()
  out <- pk_check(data.frame(ID = 1, TIME = 0))
  expect_equal(out$check, "required_columns")
  expect_equal(out$n, 3L)
})


test_that("pk_check reports a missing EVID or AMT column as an error", {
  proj <- local_project()
  d <- as.data.frame(tibble::tribble(
    ~ID, ~TIME, ~DV ,
    1  , 0    , NA  ,
    1  , 1    , 2.5
  ))
  out <- pk_check(d)
  expect_equal(out$check, "required_columns")
  expect_equal(out$severity, "error")
  expect_equal(out$n, 2L)
  expect_match(out$detail, "EVID")
  expect_match(out$detail, "AMT")
})


test_that("pk_data rejects a file without the required columns and registers nothing", {
  proj <- local_project()
  file <- file.path(withr::local_tempdir(), "bad.csv")
  tbl <- tibble::tribble(
    ~ID, ~TIME, ~DV ,
    1  , 0    , NA  ,
    1  , 1    , 2.5
  )
  readr::write_csv(tbl, file, na = ".")
  expect_error(pk_data(file), "EVID")
  expect_error(pk_data(file), "AMT")
  expect_false(file.exists(file.path(proj$path, "data", "bad.csv")))
  expect_null(yaml::read_yaml(file.path(proj$path, "config.yml"))$data$file)
  expect_error(pk_data(), "No dataset registered")
})


test_that("the error lists every missing column and points out a wrong case", {
  proj <- local_project()
  file <- file.path(withr::local_tempdir(), "lower.csv")
  tbl <- tibble::tribble(
    ~id, ~time, ~DV , ~AMT, ~EVID,
    1  , 0    , NA  , 100 , 1    ,
    1  , 1    , 2.5 , 0   , 0
  )
  readr::write_csv(tbl, file, na = ".")
  err <- expect_error(pk_data(file))
  msg <- conditionMessage(err)
  expect_match(msg, "missing 2 required columns")
  expect_match(msg, "ID")
  expect_match(msg, "TIME")
  expect_match(msg, "case-sensitive")
})


test_that("the required column names follow the data section of the config", {
  proj <- local_project()
  pk_project(proj$path, config = list(data = list(id = "SUBJ")))
  dir <- withr::local_tempdir()
  with_id <- file.path(dir, "with_id.csv")
  with_subj <- file.path(dir, "with_subj.csv")
  tbl <- tibble::tribble(
    ~ID, ~TIME, ~DV , ~AMT, ~EVID,
    1  , 0    , NA  , 100 , 1    ,
    1  , 1    , 2.5 , 0   , 0
  )
  readr::write_csv(tbl, with_id, na = ".")
  readr::write_csv(dplyr::rename(tbl, SUBJ = ID), with_subj, na = ".")
  expect_error(pk_data(with_id), "SUBJ")
  expect_equal(nrow(pk_data(with_subj)), 2L)
})
