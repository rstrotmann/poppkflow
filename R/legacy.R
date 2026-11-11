#' Import existing runs into the ledger
#'
#' Imports fits that were saved as `<run>.rds` (and optionally the generated
#' model code `<run>.txt`) by script-based workflows into the ledger of the
#' current project. The parent of each run is taken from run history tables
#' that have the columns `Run` and `Reference` (for example the development
#' tables written by a numbered-script workflow). Imported runs get the stage
#' `"legacy"`, the decision `"pending"`, and no run key (they are never
#' returned by the cache). The model code, if available, is stored as a custom
#' specification so that the run can be used as a parent.
#'
#' The source folder is only read, never modified.
#'
#' @param models_dir Folder with `<run>.rds` files.
#' @param history Character vector of csv files with the columns `Run` and
#'   `Reference` (and optionally `Description`).
#' @param pattern Regular expression for the file names to import.
#' @param quiet Suppress progress messages?
#' @inheritParams pk_project
#' @return The imported run ids (invisibly).
#' @export
pk_import_legacy <- function(models_dir, history = character(), pattern = "^run[0-9A-Za-z_]+\\.rds$", quiet = FALSE, proj = NULL) {
  proj <- pk_refresh_quiet(resolve_proj(proj))
  files <- list.files(models_dir, pattern = pattern, full.names = TRUE)
  hist <- dplyr::bind_rows(lapply(history, function(f) {
    h <- readr::read_csv(f, show_col_types = FALSE, progress = FALSE)
    if (!all(c("Run", "Reference") %in% names(h))) {
      return(NULL)
    }
    if (!"Description" %in% names(h)) h$Description <- NA_character_
    h[, c("Run", "Reference", "Description")]
  }))
  if (nrow(hist)) hist <- hist[!duplicated(hist$Run), ]
  existing <- ledger_read(proj)$run_id
  ids <- character()
  for (f in files) {
    id <- sub("\\.rds$", "", basename(f))
    if (id %in% existing) next
    fit <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(fit) || is.null(safe(fit$objf))) {
      if (!quiet) cli::cli_alert_warning("Skipped {.file {basename(f)}}: not an nlmixr2 fit.")
      next
    }
    dir <- run_dir(proj$path, id)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(f, file.path(dir, "fit.rds"), overwrite = TRUE)
    result <- extract_result(fit)
    saveRDS(result, file.path(dir, "result.rds"))
    code_file <- sub("\\.rds$", ".txt", f)
    code <- NA_character_
    if (file.exists(code_file)) {
      code <- paste(readLines(code_file), collapse = "\n")
      file.copy(code_file, file.path(dir, "model.txt"), overwrite = TRUE)
      fn <- tryCatch(eval(parse(text = code), envir = new.env(parent = baseenv())), error = function(e) NULL)
      if (is.function(fn)) {
        spec <- structure(list(custom_fn = fn, covariates = empty_cov_tbl()), class = "pk_spec")
        saveRDS(spec, file.path(dir, "spec.rds"))
      }
    }
    h <- if (nrow(hist)) hist[hist$Run == id, ] else NULL
    parent <- if (!is.null(h) && nrow(h) && !is.na(h$Reference[1])) h$Reference[1] else NA_character_
    compute_diagnostics(proj, id)
    ledger_upsert(proj, list(
      run_id = id, parent_id = parent, stage = "legacy",
      description = if (!is.null(h) && nrow(h)) h$Description[1] else NA_character_,
      created = pk_now(), finished = pk_now(), user = pk_user(),
      spec_hash = if (is.na(code)) NA_character_ else pk_hash(code),
      est_method = "legacy", status = "completed",
      ofv = result$ofv, aic = result$aic, bic = result$bic, n_theta = result$n_theta, n_eta = result$n_eta,
      n_par = result$n_par, cond_no = result$cond_no, cov_step = result$cov_step,
      runtime_s = result$runtime_s, max_shrink_eta = result$max_shrink_eta,
      n_flags = nrow(flags_from_result(result, effective_thresholds(proj))),
      decision = "pending", model_dir = file.path("models", id)
    ))
    event_append(proj, id, "imported", paste("from", basename(f)))
    ids <- c(ids, id)
  }
  d <- ledger_read(proj)
  for (id in ids) {
    i <- match(id, d$run_id)
    p <- d$parent_id[i]
    if (!is.na(p) && p %in% d$run_id && !is.na(d$ofv[match(p, d$run_id)])) {
      ledger_upsert(proj, list(run_id = id, dofv_parent = d$ofv[i] - d$ofv[match(p, d$run_id)]))
    }
  }
  if (!quiet) cli::cli_alert_success("Imported {length(ids)} run{?s}.")
  invisible(ids)
}
