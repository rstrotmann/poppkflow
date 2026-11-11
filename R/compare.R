#' Compare runs
#'
#' `pk_compare()` returns one row per run with OFV, the difference in OFV (dOFV),
#' the difference in the number of estimated parameters (df), the p-value of the
#' likelihood-ratio test, AIC, BIC, condition number, covariance step status,
#' number of flags, and the decision. The reference is the parent of each run
#' (`ref = "parent"`), the first run in the list (`ref = "first"`), or a given
#' run id.
#'
#' The likelihood-ratio test assumes nested models; the p-value is only given
#' for a decrease in OFV with a positive increase in the number of parameters.
#' The OFV is only comparable between runs estimated on the same data with the
#' same method. Runs with different data hashes or estimation methods are
#' flagged in the column `comparable`.
#'
#' `pk_compare_params()` shows the parameter estimates of several runs side by
#' side, with the relative change against the first run.
#'
#' @param ... Runs (references or ids).
#' @param ref `"parent"`, `"first"`, or a run id.
#' @inheritParams pk_project
#' @return A tibble.
#' @export
pk_compare <- function(..., ref = c("parent", "first"), proj = NULL) {
  runs <- list(...)
  if (length(runs) && inherits(runs[[1]], "pk_run_ref")) {
    proj <- resolve_run(runs[[1]])$proj
  }
  proj <- resolve_proj(proj)
  ids <- vapply(runs, function(r) resolve_run(r, proj)$run_id, character(1))
  d <- ledger_read(proj)
  ref <- if (length(ref) > 1L) ref[1] else ref
  rows <- d[match(ids, d$run_id), ]
  if (anyNA(rows$run_id)) {
    cli::cli_abort("Some runs are not in the ledger.")
  }
  ref_id <- vapply(seq_len(nrow(rows)), function(i) {
    if (ref == "parent") {
      rows$parent_id[i]
    } else if (ref == "first") {
      if (i == 1L) NA_character_ else rows$run_id[1]
    } else {
      if (rows$run_id[i] == ref) NA_character_ else ref
    }
  }, character(1))
  rr <- d[match(ref_id, d$run_id), ]
  df <- rows$n_par - rr$n_par
  dofv <- rows$ofv - rr$ofv
  p <- ifelse(!is.na(dofv) & !is.na(df) & dofv < 0 & df > 0, stats::pchisq(-dofv, df, lower.tail = FALSE), NA_real_)
  comparable <- ifelse(is.na(ref_id), NA, rows$data_hash == rr$data_hash & rows$est_method == rr$est_method)
  tibble::tibble(
    run_id = rows$run_id, reference = ref_id, ofv = rows$ofv, dofv = dofv, df = as.integer(df), p_lrt = p,
    aic = rows$aic, daic = rows$aic - rr$aic, bic = rows$bic, cond_no = rows$cond_no, cov_step = rows$cov_step,
    n_flags = rows$n_flags, comparable = comparable, decision = rows$decision, description = rows$description
  )
}

#' @rdname pk_compare
#' @export
pk_compare_params <- function(..., proj = NULL) {
  runs <- list(...)
  ids <- vapply(runs, function(r) resolve_run(r, proj)$run_id, character(1))
  tabs <- lapply(seq_along(runs), function(i) {
    t <- pk_params_table(runs[[i]], proj)
    t[, c("Type", "Code", "Estimate")] |> stats::setNames(c("Type", "Code", ids[i]))
  })
  out <- Reduce(function(a, b) dplyr::full_join(a, b, by = c("Type", "Code")), tabs)
  if (length(ids) > 1L) {
    base <- out[[ids[1]]]
    for (id in ids[-1]) {
      out[[paste0("change_", id, " (%)")]] <- 100 * (out[[id]] - base) / abs(base)
    }
  }
  out
}
