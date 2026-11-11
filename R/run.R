data_hash_of <- function(d) {
  d <- as.data.frame(d)
  attr(d, "data_hash") <- NULL
  rownames(d) <- NULL
  pk_hash(d)
}

#' Fit a model and register it in the ledger
#'
#' `pk_run()` is the central function of the workflow. It
#'
#' 1. compiles the specification to `nlmixr2` code,
#' 2. computes the run key from the specification (generated code), the data and
#'    the estimation settings, and returns the existing run if the same
#'    combination has already been completed (caching; use `refit = TRUE` to
#'    force a new fit),
#' 3. registers the run (with parent, stage and note) in the ledger,
#' 4. fits the model,
#' 5. stores `fit.rds`, `result.rds`, `spec.rds`, `model.txt`, `settings.rds` and
#'    `diag.rds` in `models/<run_id>/`, and
#' 6. updates the ledger with OFV, dOFV against the parent, AIC, condition
#'    number, covariance step status, shrinkage, and the number of flags.
#'
#' A failed fit is recorded with status `"failed"` and the error message, and
#' the error is re-thrown.
#'
#' @param x A `pk_spec`, a `pk_candidate` (result of the editing verbs, see
#'   [pk_edit]), a run reference, or a run id. For a run, its specification is
#'   re-used and the run becomes the parent.
#' @param data Data frame. Default: the registered dataset, see [pk_data()].
#' @param est Estimation settings, see [pk_est()].
#' @param note Free-text description of the run (what changed and why).
#' @param stage Development stage, e.g. `"structural"`, `"stochastic"`,
#'   `"covariate"`, `"final"`.
#' @param id Optional run id. Default: the next free `runNNN`.
#' @param refit Fit again even if an identical completed run exists?
#' @param fit_fun Function `f(model, data, est)` that returns a fit. Default:
#'   the `nlmixr2` fit with the strategy of [pk_est()]. Mainly useful for tests.
#' @param quiet Suppress progress messages?
#' @inheritParams pk_project
#'
#' @return A run reference (`pk_run_ref`). Use [pk_result()] and [pk_get()] to
#'   access results and fit.
#' @examples
#' \donttest{
#' p <- pk_project(file.path(tempdir(), "demo_run"))
#' d <- pk_data(system.file("extdata", "example_pk.csv", package = "poppkflow"))
#' r1 <- pk_run(pk_spec(ncmt = 2, iiv = c("cl", "v2")), data = d, est = pk_est(method = "focei"),
#'              note = "Base model", stage = "structural")
#' pk_history()
#' }
#' @export
pk_run <- function(x, data = NULL, est = NULL, note = NA_character_, stage = NA_character_,
                   id = NULL, refit = FALSE, fit_fun = NULL, quiet = FALSE, proj = NULL) {
  proj <- if (inherits(x, "pk_run_ref")) {
    structure(list(path = x$proj_path), class = "pk_project")
  } else {
    resolve_proj(proj)
  }
  proj <- pk_refresh_quiet(proj)
  cand <- as_candidate(x, proj)
  spec <- cand$spec
  model <- pk_compile(spec)
  code <- attr(model, "code")
  if (is.null(data)) data <- pk_data(proj = proj)
  est <- est %||% pk_est(proj = proj)
  if (is.null(fit_fun)) fit_fun <- fit_model

  key_parts <- list(spec = pk_hash(code), data = data_hash_of(data), settings = est_hash(est))
  run_key <- pk_hash(key_parts)

  d0 <- ledger_read(proj)
  if (!refit) {
    hit <- d0[!is.na(d0$run_key) & d0$run_key == run_key & d0$status %in% "completed", ]
    if (nrow(hit)) {
      if (!quiet) {
        cli::cli_alert_info("Identical run already completed: returning {.val {hit$run_id[1]}} (use {.code refit = TRUE} to fit again).")
      }
      return(new_run_ref(proj, hit$run_id[1]))
    }
  }
  parent <- cand$parent_id
  if (!is.na(parent) && !parent %in% d0$run_id) {
    cli::cli_abort("Parent run {.val {parent}} is not in the ledger.")
  }
  run_id <- id %||% ledger_next_id(proj)
  if (run_id %in% d0$run_id) {
    cli::cli_abort("Run id {.val {run_id}} already exists in the ledger.")
  }

  dir <- run_dir(proj$path, run_id)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(spec, file.path(dir, "spec.rds"))
  saveRDS(est, file.path(dir, "settings.rds"))
  writeLines(code, file.path(dir, "model.txt"))

  ledger_upsert(proj, list(
    run_id = run_id, parent_id = parent, stage = stage, description = note,
    created = pk_now(), user = pk_user(),
    spec_hash = key_parts$spec, data_hash = key_parts$data, settings_hash = key_parts$settings, run_key = run_key,
    seed = est$seed, est_method = est$method,
    nlmixr2_version = as.character(utils::packageVersion("nlmixr2est")),
    rxode2_version = as.character(utils::packageVersion("rxode2")),
    package_version = as.character(utils::packageVersion("poppkflow")),
    r_version = paste(R.version$major, R.version$minor, sep = "."),
    status = "running", decision = "pending", model_dir = file.path("models", run_id)
  ))
  event_append(proj, run_id, "created", paste0("parent=", parent, "; stage=", stage))
  if (!quiet) cli::cli_alert("Fitting {.val {run_id}}{if (!is.na(note)) paste0(': ', note) else ''}")

  t0 <- Sys.time()
  fit <- tryCatch(fit_fun(model, data, est), error = function(e) e)
  if (inherits(fit, "error")) {
    msg <- conditionMessage(fit)
    ledger_upsert(proj, list(run_id = run_id, status = "failed", error_message = msg, finished = pk_now()))
    event_append(proj, run_id, "failed", msg)
    cli::cli_abort(c("Run {.val {run_id}} failed.", "x" = msg))
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  result <- extract_result(fit, elapsed)
  saveRDS(fit, file.path(dir, "fit.rds"))
  saveRDS(result, file.path(dir, "result.rds"))

  diag <- compute_diagnostics(proj, run_id)
  dofv <- NA_real_
  if (!is.na(parent)) {
    pofv <- d0$ofv[d0$run_id == parent]
    if (length(pofv) && !is.na(pofv)) dofv <- result$ofv - pofv
  }
  ledger_upsert(proj, list(
    run_id = run_id, status = "completed", finished = pk_now(),
    ofv = result$ofv, dofv_parent = dofv, aic = result$aic, bic = result$bic,
    n_theta = result$n_theta, n_eta = result$n_eta, n_par = result$n_par,
    cond_no = result$cond_no, cov_step = result$cov_step, runtime_s = round(result$runtime_s, 1),
    max_shrink_eta = result$max_shrink_eta, n_flags = nrow(diag$flags)
  ))
  event_append(proj, run_id, "completed", sprintf("OFV %.3f", result$ofv))
  if (!quiet) {
    cli::cli_alert_success(
      "{.val {run_id}}: OFV {format(result$ofv, nsmall = 2, digits = 7)}{if (!is.na(dofv)) paste0(' (dOFV ', format(round(dofv, 2), nsmall = 2), ' vs ', parent, ')') else ''}, {nrow(diag$flags)} flag{?s}"
    )
  }
  new_run_ref(proj, run_id)
}

# Re-read the project without changing the current-project option
pk_refresh_quiet <- function(proj) {
  cfg_file <- file.path(proj$path, "config.yml")
  if (!file.exists(cfg_file)) {
    return(proj)
  }
  cfg <- yaml::read_yaml(cfg_file)
  structure(list(path = proj$path, name = cfg$project$name, config = cfg), class = "pk_project")
}
