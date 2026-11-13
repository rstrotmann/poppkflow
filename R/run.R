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
#'    combination has already been completed under the same parent (caching,
#'    see `cache`; use `refit = TRUE` to force a new fit),
#' 3. registers the run (with parent, stage and note) in the ledger,
#' 4. fits the model,
#' 5. stores `fit.rds`, `result.rds`, `spec.rds`, `model.txt`, `settings.rds` and
#'    `diag.rds` in `models/<run_id>/`, and
#' 6. updates the ledger with OFV, dOFV against the parent, AIC, condition
#'    number, covariance step status, shrinkage, and the number of flags.
#'
#' A run that does not complete is recorded with status `"failed"` and the error
#' message, and the error is re-thrown. This covers an error in the fit, an
#' error while the results are extracted and stored, and an interrupt (the
#' message then says that the run was interrupted). A run is therefore never left
#' with the status `"running"` unless the R process itself is killed.
#'
#' The cache respects the lineage. An identical model that was fitted under a
#' different parent is a different step in the development history, so with
#' `cache = "same_parent"` (default) it is fitted again as a new run. A run
#' that is re-run from itself (`pk_run("run003")` without changes) returns that
#' run. With `cache = "any"` the earlier run is returned whatever its parent;
#' the event log of that run then records the `cache_hit` with the requested
#' parent, so the attempted derivation stays visible in [pk_audit()].
#'
#' @param x A `pk_spec`, a `pk_candidate` (result of the editing verbs, see
#'   [pk_edit]), a run reference, or a run id. For a run, its specification is
#'   re-used and the run becomes the parent.
#' @param data Data frame. Default: the registered dataset, see [pk_data()].
#'   It must contain the required columns (`ID`, `TIME`, `DV`, `EVID`, `AMT`);
#'   otherwise `pk_run()` stops before a run is registered.
#' @param est Estimation settings, see [pk_est()].
#' @param note Free-text description of the run (what changed and why).
#' @param stage Development stage, e.g. `"structural"`, `"stochastic"`,
#'   `"covariate"`, `"final"`.
#' @param id Optional run id. Default: the next free `runNNN`.
#' @param refit Fit again even if an identical completed run exists?
#' @param cache Which identical completed run may be returned instead of
#'   fitting: `"same_parent"` (default) only one with the same parent as the
#'   requested run, `"any"` one with any parent (a `cache_hit` event records a
#'   different requested parent). Ignored if `refit = TRUE`.
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
                   id = NULL, refit = FALSE, cache = c("same_parent", "any"),
                   fit_fun = NULL, quiet = FALSE, proj = NULL) {
  cache <- match.arg(cache)
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
  # before anything is written to the ledger
  assert_data_columns(names(data), proj$config$data)
  est <- est %||% pk_est(proj = proj)
  if (is.null(fit_fun)) fit_fun <- fit_model

  key_parts <- list(spec = pk_hash(code), data = data_hash_of(data), settings = est_hash(est))
  run_key <- pk_hash(key_parts)

  parent <- cand$parent_id
  d0 <- ledger_read(proj)
  if (!refit) {
    same_key <- d0[!is.na(d0$run_key) & d0$run_key == run_key & d0$status %in% "completed", ]
    # a run that is re-run from itself is that run; otherwise the parent decides
    fits <- same_parent(same_key$parent_id, parent) | same_key$run_id %in% parent
    hit <- if (cache == "any") same_key[order(!fits), ] else same_key[fits, ]
    if (nrow(hit)) {
      hit_id <- hit$run_id[1]
      if (!hit_id %in% parent && !same_parent(hit$parent_id[1], parent)) {
        # only possible with cache = "any": keep the attempted derivation in the audit trail
        event_append(proj, hit_id, "cache_hit", sprintf(
          "requested parent=%s; stored parent=%s; stage=%s", parent, hit$parent_id[1], stage
        ))
        if (!quiet) {
          cli::cli_alert_info("Identical run {.val {hit_id}} already completed under parent {.val {hit$parent_id[1]}}: returning it for the requested parent {.val {parent}} (cache = \"any\").")
        }
      } else if (!quiet) {
        cli::cli_alert_info("Identical run already completed: returning {.val {hit_id}} (use {.code refit = TRUE} to fit again).")
      }
      return(new_run_ref(proj, hit_id))
    }
    if (nrow(same_key) && !quiet) {
      cli::cli_alert_info("The same model exists as {.val {same_key$run_id[1]}} under parent {.val {same_key$parent_id[1]}}; fitting again under parent {.val {parent}} (use {.code cache = \"any\"} to reuse it).")
    }
  }
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
  # From here on the run exists in the ledger as "running". Whatever ends the function before the
  # run is completed (an error in the fit or in storing the results, or an interrupt) marks it as
  # failed, so that a run is never left behind as "running".
  completed <- FALSE
  fail_msg <- "Interrupted before the run was completed."
  on.exit(
    if (!completed) {
      # must not hide the original error, for example when the ledger lock cannot be obtained
      try(silent = TRUE, {
        ledger_upsert(proj, list(run_id = run_id, status = "failed", error_message = fail_msg, finished = pk_now()))
        event_append(proj, run_id, "failed", fail_msg)
      })
    },
    add = TRUE
  )
  event_append(proj, run_id, "created", paste0("parent=", parent, "; stage=", stage))
  if (!quiet) cli::cli_alert("Fitting {.val {run_id}}{if (!is.na(note)) paste0(': ', note) else ''}")

  t0 <- Sys.time()
  fit <- tryCatch(fit_fun(model, data, est), error = function(e) e)
  if (inherits(fit, "error")) {
    fail_msg <- conditionMessage(fit)
    cli::cli_abort(c("Run {.val {run_id}} failed.", "x" = fail_msg))
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  withCallingHandlers(
    {
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
      # the completed row is written: from here the run counts as completed
      completed <- TRUE
    },
    error = function(e) fail_msg <<- paste("Failed after the fit while storing the results:", conditionMessage(e))
  )
  event_append(proj, run_id, "completed", sprintf("OFV %.3f", result$ofv))
  if (!quiet) {
    cli::cli_alert_success(
      "{.val {run_id}}: OFV {format(result$ofv, nsmall = 2, digits = 7)}{if (!is.na(dofv)) paste0(' (dOFV ', format(round(dofv, 2), nsmall = 2), ' vs ', parent, ')') else ''}, {nrow(diag$flags)} flag{?s}"
    )
  }
  new_run_ref(proj, run_id)
}

# Is the stored parent the requested one? Two missing parents (no parent) are the same.
same_parent <- function(stored, requested) {
  (is.na(stored) & is.na(requested)) | (!is.na(stored) & !is.na(requested) & stored == requested)
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
