test_that("pk_run registers a run with files, ledger row and events", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2), note = "Base model", stage = "structural")
  expect_s3_class(r1, "pk_run_ref")
  expect_equal(r1$run_id, "run001")
  dir <- file.path(proj$path, "models", "run001")
  expect_true(all(file.exists(file.path(dir, c("spec.rds", "model.txt", "fit.rds", "result.rds", "diag.rds", "settings.rds")))))

  runs <- pk_runs()
  expect_equal(nrow(runs), 1L)
  expect_equal(runs$status, "completed")
  expect_equal(runs$stage, "structural")
  expect_equal(runs$description, "Base model")
  expect_equal(runs$ofv, 1000)
  expect_true(is.na(runs$parent_id))
  expect_equal(runs$decision, "pending")
  expect_equal(runs$model_dir, file.path("models", "run001"))

  ev <- pk_audit()
  expect_equal(ev$event, c("created", "completed"))
})


test_that("candidates record the parent and dOFV against the parent", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  r2 <- new_run(proj, pk_add_iiv(r1, "ka"), note = "Add IIV on KA", stage = "stochastic")
  runs <- pk_runs()
  expect_equal(runs$parent_id, c(NA, "run001"))
  expect_equal(runs$dofv_parent, c(NA, -20))
  expect_equal(runs$n_par[2] - runs$n_par[1], 1L)
})


test_that("an identical run is taken from the cache, and refit creates a new run", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  calls <- 0
  counting <- function(model, data, est) {
    calls <<- calls + 1
    stub_fit_fun()(model, data, est)
  }
  r1b <- new_run(proj, pk_spec(ncmt = 2), fit_fun = counting)
  expect_equal(r1b$run_id, r1$run_id)
  expect_equal(calls, 0)
  expect_equal(nrow(pk_runs()), 1L)

  r1c <- new_run(proj, pk_spec(ncmt = 2), fit_fun = counting, refit = TRUE)
  expect_equal(calls, 1)
  expect_equal(r1c$run_id, "run002")
  expect_equal(nrow(pk_runs()), 2L)
})


test_that("a different seed or dataset gives a different run key", {
  proj <- local_project()
  spec <- pk_spec(ncmt = 2)
  a <- pk_run(spec, data = example_data(), est = pk_est(method = "focei", seed = 1), fit_fun = stub_fit_fun(), quiet = TRUE)
  b <- pk_run(spec, data = example_data(), est = pk_est(method = "focei", seed = 2), fit_fun = stub_fit_fun(), quiet = TRUE)
  d2 <- example_data()
  d2$DV[2] <- 2.2
  c <- pk_run(spec, data = d2, est = pk_est(method = "focei", seed = 1), fit_fun = stub_fit_fun(), quiet = TRUE)
  runs <- pk_runs()
  expect_equal(length(unique(runs$run_key)), 3L)
  expect_equal(nrow(runs), 3L)
})


test_that("a failed fit is recorded and the error is re-thrown", {
  proj <- local_project()
  expect_error(
    new_run(proj, pk_spec(ncmt = 2), fit_fun = stub_fit_fun(fail_if = "lka")),
    "failed"
  )
  runs <- pk_runs()
  expect_equal(runs$status, "failed")
  expect_match(runs$error_message, "stub failure")
  expect_equal(pk_audit()$event, c("created", "failed"))

  # failed runs are not used as cache
  r <- new_run(proj, pk_spec(ncmt = 2))
  expect_equal(r$run_id, "run002")
})


test_that("an error after the fit marks the run as failed instead of leaving it running", {
  proj <- local_project()
  with_mocked_bindings(
    expect_error(new_run(proj, pk_spec(ncmt = 2)), "boom"),
    compute_diagnostics = function(...) stop("boom"),
    .package = "poppkflow"
  )
  runs <- pk_runs()
  expect_equal(runs$status, "failed")
  expect_match(runs$error_message, "boom")
  expect_false(is.na(runs$finished))
  expect_equal(pk_audit()$event, c("created", "failed"))

  # a failed run is neither used as cache nor blocks the next run
  r <- new_run(proj, pk_spec(ncmt = 2))
  expect_equal(r$run_id, "run002")
  expect_equal(pk_runs()$status, c("failed", "completed"))
})


test_that("an interrupt during the fit marks the run as failed", {
  proj <- local_project()
  interrupting <- function(model, data, est) {
    stop(structure(class = c("test_interrupt", "condition"), list(message = "stop", call = NULL)))
  }
  # an exiting handler unwinds the stack like a real interrupt does
  out <- tryCatch(new_run(proj, pk_spec(ncmt = 2), fit_fun = interrupting), test_interrupt = function(cnd) "interrupted")
  expect_equal(out, "interrupted")
  runs <- pk_runs()
  expect_equal(runs$status, "failed")
  expect_match(runs$error_message, "Interrupted")
  expect_equal(pk_audit()$event, c("created", "failed"))
})


test_that("an identical model under a different parent is fitted again by default", {
  proj <- local_project()
  new_run(proj, pk_spec(ncmt = 2))
  new_run(proj, pk_spec(ncmt = 2, iiv = c("cl", "v2", "ka")))
  calls <- 0
  counting <- function(model, data, est) {
    calls <<- calls + 1
    stub_fit_fun()(model, data, est)
  }

  # run002 is the same model, but not derived from run001
  r3 <- new_run(proj, pk_add_iiv("run001", "ka"), fit_fun = counting)
  expect_equal(calls, 1)
  expect_equal(r3$run_id, "run003")
  runs <- pk_runs()
  expect_equal(runs$parent_id, c(NA, NA, "run001"))
  expect_equal(runs$run_key[2], runs$run_key[3])

  # the same derivation again is found under its own parent
  r3b <- new_run(proj, pk_add_iiv("run001", "ka"), fit_fun = counting)
  expect_equal(r3b$run_id, "run003")
  expect_equal(calls, 1)
  expect_equal(nrow(pk_runs()), 3L)
})


test_that("cache = 'any' returns the earlier run and logs the requested parent", {
  proj <- local_project()
  new_run(proj, pk_spec(ncmt = 2))
  new_run(proj, pk_spec(ncmt = 2, iiv = c("cl", "v2", "ka")))
  r3 <- new_run(proj, pk_add_iiv("run001", "ka"), cache = "any")
  expect_equal(r3$run_id, "run002")
  expect_equal(nrow(pk_runs()), 2L)
  ev <- pk_audit(run = "run002")
  expect_equal(ev$event, c("created", "completed", "cache_hit"))
  expect_match(ev$detail[3], "requested parent=run001; stored parent=NA")

  # a hit under the same parent is not logged as a different derivation
  new_run(proj, pk_spec(ncmt = 2), cache = "any")
  expect_equal(pk_audit(run = "run001")$event, c("created", "completed"))
})


test_that("a run that is re-run from itself is returned from the cache", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  again <- new_run(proj, r1)
  expect_equal(again$run_id, "run001")
  expect_equal(nrow(pk_runs()), 1L)
  expect_equal(pk_audit()$event, c("created", "completed"))
})


test_that("pk_run rejects data without the required columns before registering a run", {
  proj <- local_project()
  d <- example_data()
  d$EVID <- NULL
  d$AMT <- NULL
  expect_error(
    pk_run(pk_spec(ncmt = 2), data = d, est = pk_est(method = "focei"), fit_fun = stub_fit_fun(), quiet = TRUE),
    "EVID"
  )
  expect_equal(nrow(pk_runs()), 0L)
  expect_equal(nrow(pk_audit()), 0L)
  expect_false(dir.exists(file.path(proj$path, "models", "run001")))

  # the next valid run still gets the first id
  r <- new_run(proj, pk_spec(ncmt = 2))
  expect_equal(r$run_id, "run001")
})


test_that("explicit ids are respected and must be unique", {
  proj <- local_project()
  r <- new_run(proj, pk_spec(ncmt = 2), id = "base")
  expect_equal(r$run_id, "base")
  expect_error(new_run(proj, pk_spec(ncmt = 1), id = "base"), "already exists")
})


test_that("runs can be used by id as parent", {
  proj <- local_project()
  new_run(proj, pk_spec(ncmt = 2))
  r2 <- new_run(proj, pk_add_iiv("run001", "ka"))
  expect_equal(pk_runs()$parent_id[2], "run001")
  expect_equal(r2$run_id, "run002")
})


test_that("pk_inits_from takes the estimates of a run", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2, inits = list(cl = 7, v2 = 33)))
  cand <- pk_inits_from(pk_add_iiv(r1, "ka"), r1)
  expect_equal(cand$spec$inits$cl, 7)
  expect_equal(cand$spec$inits$v2, 33)
  expect_equal(cand$spec$inits$om[["cl"]], 0.1)
})


test_that("pk_result, pk_get and pk_params_table read the stored files", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  res <- pk_result(r1)
  expect_equal(res$ofv, 1000)
  expect_s3_class(pk_get("run001"), "stubfit")
  tab <- pk_params_table(r1)
  expect_true(all(c("Type", "Code", "Estimate", "RSE (%)") %in% names(tab)))
  expect_equal(tab$Estimate[tab$Code == "lcl"], 10)
  expect_equal(tab$Estimate[tab$Code == "eta.cl"], 0.1)
})


test_that("pk_decide requires a reason and logs the change", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2))
  expect_error(pk_decide(r1, "accepted"), "reason")
  pk_decide(r1, "accepted", reason = "Adequate fit")
  pk_decide(r1, "rejected", reason = "Changed my mind")
  runs <- pk_runs()
  expect_equal(runs$decision, "rejected")
  expect_equal(runs$decision_reason, "Changed my mind")
  ev <- pk_audit(run = "run001")
  expect_equal(ev$event, c("created", "completed", "decision:accepted", "decision:rejected"))
  pk_decide(r1, "pending")
  expect_equal(pk_runs()$decision, "pending")
})


test_that("failed runs cannot be decided", {
  proj <- local_project()
  try(new_run(proj, pk_spec(ncmt = 2), fit_fun = stub_fit_fun(fail_if = "lka")), silent = TRUE)
  expect_error(pk_decide("run001", "accepted", reason = "x"), "completed")
})


test_that("pk_tree shows the lineage and pk_history the development table", {
  proj <- local_project()
  r1 <- new_run(proj, pk_spec(ncmt = 2), note = "Base")
  r2 <- new_run(proj, pk_add_iiv(r1, "ka"), note = "IIV KA")
  r3 <- new_run(proj, pk_set_error(r1, "comb"), note = "Combined error")
  pk_decide(r2, "accepted", reason = "better")
  tree <- capture.output(lines <- pk_tree())
  expect_equal(length(lines), 3L)
  expect_match(lines[1], "run001")
  expect_match(lines[2], "run002")
  expect_match(lines[2], "[+]", fixed = TRUE)
  expect_match(lines[3], "run003")

  mm <- pk_tree(format = "mermaid")
  expect_equal(mm[1], "flowchart TD")
  expect_true("  run001 --> run002" %in% mm)

  h <- pk_history()
  expect_equal(h$run_id, c("run001", "run002", "run003"))
  expect_equal(h$decision, c("pending", "accepted", "pending"))
})


test_that("the ledger is written atomically and survives a re-open", {
  proj <- local_project()
  new_run(proj, pk_spec(ncmt = 2))
  proj2 <- pk_open(proj$path)
  expect_equal(nrow(pk_runs(proj = proj2)), 1L)
  expect_false(any(grepl("\\.lock$|\\.tmp_", list.files(file.path(proj$path, "ledger"), all.files = TRUE))))
})
