# The run ledger: ledger/runs.csv (current state, one row per run) and
# ledger/events.csv (append-only audit trail). Fits and other rich objects are
# stored as RDS files in models/<run_id>/.

ledger_schema <- c(
  run_id = "c", parent_id = "c", stage = "c", description = "c", created = "c", finished = "c", user = "c",
  spec_hash = "c", data_hash = "c", settings_hash = "c", run_key = "c", seed = "d", est_method = "c",
  nlmixr2_version = "c", rxode2_version = "c", package_version = "c", r_version = "c",
  status = "c", error_message = "c",
  ofv = "d", dofv_parent = "d", aic = "d", bic = "d", n_theta = "i", n_eta = "i", n_par = "i",
  cond_no = "d", cov_step = "l", runtime_s = "d", max_shrink_eta = "d", n_flags = "i",
  decision = "c", decision_reason = "c", decision_by = "c", decision_time = "c", model_dir = "c"
)

events_schema <- c(time = "c", user = "c", run_id = "c", event = "c", detail = "c")

collector <- function(code) {
  switch(code,
    c = readr::col_character(),
    d = readr::col_double(),
    i = readr::col_integer(),
    l = readr::col_logical()
  )
}

col_spec <- function(schema) {
  do.call(readr::cols, lapply(schema, collector))
}

empty_tbl <- function(schema) {
  tibble::as_tibble(lapply(schema, function(code) {
    switch(code, c = character(), d = numeric(), i = integer(), l = logical())
  }))
}

ledger_file <- function(proj) proj_path(proj, "ledger", "runs.csv")
events_file <- function(proj) proj_path(proj, "ledger", "events.csv")

ledger_read <- function(proj) {
  f <- ledger_file(proj)
  if (!file.exists(f) || file.size(f) == 0L) {
    return(empty_tbl(ledger_schema))
  }
  d <- readr::read_csv(f, col_types = col_spec(ledger_schema), na = c("", "NA"), progress = FALSE)
  d[, names(ledger_schema)]
}

coerce_to_schema <- function(d, schema) {
  for (nm in names(schema)) {
    if (!nm %in% names(d)) d[[nm]] <- NA
    d[[nm]] <- switch(schema[[nm]],
      c = as.character(d[[nm]]),
      d = as.numeric(d[[nm]]),
      i = as.integer(d[[nm]]),
      l = as.logical(d[[nm]])
    )
  }
  d[, names(schema)]
}

ledger_write <- function(proj, d) {
  d <- coerce_to_schema(d, ledger_schema)
  atomic_write(ledger_file(proj), function(tmp) readr::write_csv(d, tmp, na = ""))
  invisible(d)
}

# Insert or replace the row with the same run_id; `fields` is a named list
ledger_upsert <- function(proj, fields) {
  f <- ledger_file(proj)
  with_lock(f, {
    d <- ledger_read(proj)
    new <- coerce_to_schema(tibble::as_tibble(lapply(fields, function(v) if (is.null(v)) NA else v)), ledger_schema)
    i <- match(new$run_id, d$run_id)
    if (is.na(i)) {
      d <- dplyr::bind_rows(d, new)
    } else {
      old <- d[i, ]
      for (nm in names(fields)) {
        old[[nm]] <- new[[nm]]
      }
      d[i, ] <- old
    }
    ledger_write(proj, d)
  })
  invisible(TRUE)
}

ledger_get <- function(proj, run_id) {
  d <- ledger_read(proj)
  row <- d[d$run_id == run_id, ]
  if (nrow(row) == 0L) {
    cli::cli_abort("Run {.val {run_id}} is not in the ledger.")
  }
  row
}

ledger_next_id <- function(proj) {
  d <- ledger_read(proj)
  n <- suppressWarnings(as.integer(sub("^run", "", d$run_id[grepl("^run[0-9]+$", d$run_id)])))
  n <- n[!is.na(n)]
  format_run_id(if (length(n)) max(n) + 1L else 1L)
}

event_append <- function(proj, run_id, event, detail = "") {
  f <- events_file(proj)
  row <- tibble::tibble(time = pk_now(), user = pk_user(), run_id = run_id %||% NA_character_, event = event, detail = detail)
  with_lock(f, {
    old <- if (file.exists(f) && file.size(f) > 0L) {
      readr::read_csv(f, col_types = col_spec(events_schema), na = c("", "NA"), progress = FALSE)
    } else {
      empty_tbl(events_schema)
    }
    d <- dplyr::bind_rows(old, coerce_to_schema(row, events_schema))
    atomic_write(f, function(tmp) readr::write_csv(d, tmp, na = ""))
  })
  invisible(row)
}

run_dir <- function(proj_path, run_id) {
  file.path(proj_path, "models", run_id)
}

read_run_file <- function(proj_path, run_id, file) {
  f <- file.path(run_dir(proj_path, run_id), file)
  if (!file.exists(f)) {
    cli::cli_abort("File {.file {file}} not found for run {.val {run_id}}.")
  }
  readRDS(f)
}

#' Runs, history, audit trail and lineage
#'
#' * `pk_runs()` returns the complete ledger (`runs.csv`) as a tibble.
#' * `pk_history()` returns a compact development history: one row per run with
#'   OFV, dOFV against the parent, number of flags, and the decision.
#' * `pk_audit()` returns the append-only event log (`events.csv`).
#' * `pk_tree()` prints the lineage tree of the runs (parent to child) and
#'   returns it invisibly as text; `format = "mermaid"` returns a mermaid
#'   flowchart definition instead.
#'
#' @param stage Optional character vector to filter on the `stage` column.
#' @param run Optional run id to filter the audit trail.
#' @param root Optional run id: show only the sub-tree below this run.
#' @param format `"text"` or `"mermaid"`.
#' @inheritParams pk_project
#' @return A tibble, or the tree as a character vector.
#' @examples
#' p <- pk_project(file.path(tempdir(), "demo_runs"))
#' pk_runs()
#' @export
pk_runs <- function(proj = NULL) {
  ledger_read(resolve_proj(proj))
}

#' @rdname pk_runs
#' @export
pk_history <- function(stage = NULL, proj = NULL) {
  d <- ledger_read(resolve_proj(proj))
  if (!is.null(stage)) d <- d[d$stage %in% stage, ]
  d[, c(
    "run_id", "parent_id", "stage", "description", "status", "ofv", "dofv_parent", "n_par",
    "aic", "cov_step", "cond_no", "n_flags", "decision", "decision_reason"
  )]
}

#' @rdname pk_runs
#' @export
pk_audit <- function(run = NULL, proj = NULL) {
  proj <- resolve_proj(proj)
  f <- events_file(proj)
  if (!file.exists(f) || file.size(f) == 0L) {
    return(empty_tbl(events_schema))
  }
  d <- readr::read_csv(f, col_types = col_spec(events_schema), na = c("", "NA"), progress = FALSE)
  if (!is.null(run)) d <- d[d$run_id %in% run, ]
  d
}

#' @rdname pk_runs
#' @export
pk_tree <- function(root = NULL, format = c("text", "mermaid"), proj = NULL) {
  format <- match.arg(format)
  d <- ledger_read(resolve_proj(proj))
  if (nrow(d) == 0L) {
    cli::cli_text("No runs yet.")
    return(invisible(character()))
  }
  tag <- function(i) {
    dec <- d$decision[i]
    mark <- switch(if (is.na(dec)) "pending" else dec, accepted = "[+]", rejected = "[x]", pending = "[ ]")
    ofv <- if (is.na(d$ofv[i])) "" else sprintf("  OFV %.1f", d$ofv[i])
    dofv <- if (is.na(d$dofv_parent[i])) "" else sprintf(" (dOFV %+.1f)", d$dofv_parent[i])
    desc <- if (is.na(d$description[i])) "" else paste0("  ", d$description[i])
    paste0(mark, " ", d$run_id[i], ofv, dofv, desc)
  }
  if (format == "mermaid") {
    lines <- "flowchart TD"
    for (i in seq_len(nrow(d))) {
      lab <- gsub("\"", "'", sub("^\\[.\\] ", "", tag(i)))
      lines <- c(lines, sprintf("  %s[\"%s\"]", d$run_id[i], lab))
      if (!is.na(d$parent_id[i]) && d$parent_id[i] %in% d$run_id) {
        lines <- c(lines, sprintf("  %s --> %s", d$parent_id[i], d$run_id[i]))
      }
    }
    return(lines)
  }
  roots <- if (is.null(root)) {
    d$run_id[is.na(d$parent_id) | !d$parent_id %in% d$run_id]
  } else {
    root
  }
  lines <- character()
  walk <- function(id, indent, is_last, is_root) {
    i <- match(id, d$run_id)
    connector <- if (is_root) "" else if (is_last) "`- " else "|- "
    lines <<- c(lines, paste0(indent, connector, tag(i)))
    kids <- d$run_id[!is.na(d$parent_id) & d$parent_id == id]
    child_indent <- if (is_root) indent else paste0(indent, if (is_last) "   " else "|  ")
    for (k in seq_along(kids)) walk(kids[k], child_indent, k == length(kids), FALSE)
  }
  for (r in roots) walk(r, "", TRUE, TRUE)
  cat(lines, sep = "\n")
  invisible(lines)
}

# ---- run references -------------------------------------------------------------------------

new_run_ref <- function(proj, run_id) {
  structure(list(run_id = run_id, proj_path = proj$path), class = "pk_run_ref")
}

#' @export
print.pk_run_ref <- function(x, ...) {
  proj <- structure(list(path = x$proj_path), class = "pk_project")
  row <- tryCatch(ledger_get(proj, x$run_id), error = function(e) NULL)
  if (is.null(row)) {
    cli::cli_text("{.cls pk_run_ref} {.val {x$run_id}} (not in ledger)")
  } else {
    cli::cli_text("{.cls pk_run_ref} {.val {x$run_id}}: {row$status}, OFV {format(row$ofv, digits = 8)}, decision {row$decision}")
    if (!is.na(row$description)) cli::cli_text("{row$description}")
  }
  invisible(x)
}

# Resolve a run reference or id to (project, run_id)
resolve_run <- function(run, proj = NULL) {
  if (inherits(run, "pk_run_ref")) {
    p <- pk_refresh_quiet(structure(list(path = run$proj_path), class = "pk_project"))
    return(list(proj = p, run_id = run$run_id))
  }
  if (is_string(run)) {
    return(list(proj = pk_refresh_quiet(resolve_proj(proj)), run_id = run))
  }
  cli::cli_abort("Expected a run reference or a run id.")
}

#' Access a run
#'
#' * `pk_run_ref()` creates a run reference from a run id.
#' * `pk_get()` loads the full `nlmixr2` fit of a run (`fit.rds`).
#' * `pk_result()` loads the slim result (`result.rds`): OFV, parameter table,
#'   omega, shrinkage, ETAs. It does not need the fit.
#' @param run A run reference or a run id.
#' @inheritParams pk_project
#' @return `pk_get()`: the fit; `pk_result()`: a list; `pk_run_ref()`: a
#'   `pk_run_ref`.
#' @export
pk_get <- function(run, proj = NULL) {
  r <- resolve_run(run, proj)
  read_run_file(r$proj$path, r$run_id, "fit.rds")
}

#' @rdname pk_get
#' @export
pk_result <- function(run, proj = NULL) {
  r <- resolve_run(run, proj)
  read_run_file(r$proj$path, r$run_id, "result.rds")
}

#' @rdname pk_get
#' @export
pk_run_ref <- function(run, proj = NULL) {
  r <- resolve_run(run, proj)
  ledger_get(r$proj, r$run_id)
  new_run_ref(r$proj, r$run_id)
}
