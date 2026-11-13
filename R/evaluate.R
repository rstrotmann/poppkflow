eval_dir <- function(proj_path, run_id) {
  d <- file.path(run_dir(proj_path, run_id), "eval")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Omega matrix -> named list of variances (om.<par>) and covariances (om.<a>.<b>)
omega_vec <- function(omega) {
  nm <- sub("^eta\\.", "", rownames(omega))
  out <- as.list(diag(omega))
  names(out) <- paste0("om.", nm)
  for (i in seq_len(nrow(omega))[-1]) {
    for (j in seq_len(i - 1L)) {
      if (omega[i, j] != 0) out[[paste0("om.", nm[j], ".", nm[i])]] <- omega[i, j]
    }
  }
  out
}

#' Non-parametric bootstrap
#'
#' Resamples subjects with replacement (optionally stratified, for example by
#' sampling design), re-estimates the model from the final estimates of the run
#' and stores the estimates of every replicate in
#' `models/<run>/eval/bootstrap.rds`. Replicates are saved after each fit (or
#' after each batch if `parallel = TRUE`), and a repeated call continues from the
#' stored replicates (`resume = TRUE`).
#'
#' By default the replicates are estimated with SAEM, which is robust to the
#' starting values and does not need an OFV. For parallel execution install
#' `future.apply` and set a plan, for example `future::plan("multisession")`.
#' Only the main process writes to the project; the workers return results.
#'
#' @param run A completed run (reference or id).
#' @param n Total number of replicates wanted.
#' @param strata Name of a subject-level column to stratify the resampling by.
#' @param est Estimation settings for the replicates. Default: SAEM with 250
#'   burn-in and 250 EM iterations.
#' @param seed Base seed; replicate `b` uses `seed + b`.
#' @param parallel Use `future.apply::future_lapply()`?
#' @param resume Continue from stored replicates?
#' @param data,fit_fun,quiet,proj See [pk_run()].
#' @return A tibble with one row per replicate (invisibly).
#' @export
pk_bootstrap <- function(run, n = 200, strata = NULL, data = NULL, est = NULL, seed = 1,
                         parallel = FALSE, resume = TRUE, fit_fun = NULL, quiet = FALSE, proj = NULL) {
  r <- resolve_run(run, proj)
  proj <- pk_refresh_quiet(r$proj)
  ref <- new_run_ref(proj, r$run_id)
  if (is.null(data)) data <- pk_data(proj = proj)
  est <- est %||% pk_est(method = "saem", nBurn = 250, nEm = 250, proj = proj)
  fit_fun <- fit_fun %||% fit_model
  idc <- proj$config$data$id %||% "ID"
  model <- pk_compile(pk_inits_from(ref, ref, proj = proj))

  file <- file.path(eval_dir(proj$path, r$run_id), "bootstrap.rds")
  done <- if (resume && file.exists(file)) readRDS(file) else NULL
  first <- if (is.null(done)) 1L else max(done$sample) + 1L
  todo <- seq.int(first, length.out = max(0L, n - (first - 1L)))
  if (!length(todo)) {
    if (!quiet) cli::cli_alert_info("{.val {nrow(done)}} replicates already stored.")
    return(invisible(done))
  }

  subj <- data[!duplicated(data[[idc]]), c(idc, strata), drop = FALSE]
  one <- function(b) {
    set.seed(seed + b)
    grp <- if (is.null(strata)) rep(1L, nrow(subj)) else as.character(subj[[strata]])
    ids <- unlist(lapply(split(subj[[idc]], grp), function(v) v[sample.int(length(v), replace = TRUE)]), use.names = FALSE)
    parts <- lapply(seq_along(ids), function(i) {
      x <- data[data[[idc]] == ids[i], , drop = FALSE]
      x[[idc]] <- i
      x
    })
    bs <- do.call(rbind, parts)
    est_b <- est
    est_b$seed <- seed + b
    t0 <- Sys.time()
    fit <- tryCatch(fit_fun(model, bs, est_b), error = function(e) NULL)
    if (is.null(fit)) {
      return(tibble::tibble(sample = b, ok = FALSE))
    }
    th <- as.list(fit$theta)
    tibble::as_tibble(c(
      list(sample = b, ok = TRUE, ofv = num1(fit$objf), time_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))),
      th, omega_vec(fit$omega)
    ))
  }

  collected <- done
  save_now <- function(x) {
    collected <<- dplyr::bind_rows(collected, x)
    atomic_write(file, function(tmp) saveRDS(collected, tmp))
  }
  if (parallel && requireNamespace("future.apply", quietly = TRUE)) {
    res <- future.apply::future_lapply(todo, one, future.seed = TRUE)
    save_now(dplyr::bind_rows(res))
  } else {
    for (b in todo) {
      save_now(one(b))
      if (!quiet) cli::cli_alert("Bootstrap replicate {b}/{n}")
    }
  }
  event_append(proj, r$run_id, "bootstrap", sprintf("%d replicates (%d successful)", nrow(collected), sum(collected$ok)))
  invisible(collected)
}

#' Summary of a bootstrap
#'
#' Median and 95% percentile interval of the bootstrap estimates, and the bias
#' relative to the final estimate. Log-scale parameters are back-transformed.
#'
#' @param run A run (reference or id) with a stored bootstrap.
#' @inheritParams pk_project
#' @return A tibble with `Code`, `Final`, `Bootstrap median`, `2.5th pct`,
#'   `97.5th pct`, `Bias (%)`, `n successful`, `n total`.
#' @export
pk_bootstrap_summary <- function(run, proj = NULL) {
  r <- resolve_run(run, proj)
  file <- file.path(run_dir(r$proj$path, r$run_id), "eval", "bootstrap.rds")
  if (!file.exists(file)) cli::cli_abort("No bootstrap stored for {.val {r$run_id}}. Run {.fn pk_bootstrap} first.")
  boot <- readRDS(file)
  res <- pk_result(run, proj)
  final <- c(res$theta, unlist(omega_vec(res$omega)))
  ok <- boot[boot$ok, ]
  codes <- intersect(names(final), names(ok))
  logp <- res$parameters$Code[res$parameters$LogScale %in% TRUE]
  rows <- lapply(codes, function(cd) {
    v <- ok[[cd]]
    fv <- final[[cd]]
    if (cd %in% logp) {
      v <- exp(v)
      fv <- exp(fv)
    }
    tibble::tibble(
      Code = cd, Final = fv, `Bootstrap median` = stats::median(v, na.rm = TRUE),
      `2.5th pct` = unname(stats::quantile(v, 0.025, na.rm = TRUE)),
      `97.5th pct` = unname(stats::quantile(v, 0.975, na.rm = TRUE))
    )
  })
  out <- dplyr::bind_rows(rows)
  out$`Bias (%)` <- 100 * (out$`Bootstrap median` - out$Final) / abs(out$Final)
  out$`n successful` <- nrow(ok)
  out$`n total` <- nrow(boot)
  out
}

#' Visual predictive check
#'
#' Thin wrapper around [nlmixr2plot::vpcPlot()] (which uses the vpc package).
#' The plot is stored in `models/<run>/eval/`.
#'
#' @param run A run (reference or id).
#' @param n Number of simulated replicates.
#' @param pred_corr Prediction-corrected VPC?
#' @param stratify Optional character vector of stratification columns.
#' @param log_y Log-scale y axis?
#' @param ... Further arguments for [nlmixr2plot::vpcPlot()].
#' @inheritParams pk_project
#' @return A ggplot object.
#' @export
pk_vpc <- function(run, n = 300, pred_corr = FALSE, stratify = NULL, log_y = FALSE, ..., proj = NULL) {
  r <- resolve_run(run, proj)
  fit <- pk_get(run, proj)
  args <- list(fit, n = n, pred_corr = pred_corr, log_y = log_y, ...)
  if (!is.null(stratify)) args$stratify <- stratify
  if (!requireNamespace("nlmixr2plot", quietly = TRUE)) {
    cli::cli_abort("The package {.pkg nlmixr2plot} is needed for {.fn pk_vpc}.")
  }
  p <- do.call(nlmixr2plot::vpcPlot, args) +
    theme_bw()
  saveRDS(p, file.path(eval_dir(r$proj$path, r$run_id), paste0("vpc", if (pred_corr) "_pc" else "", ".rds")))
  event_append(r$proj, r$run_id, "vpc", sprintf("n=%d, pred_corr=%s", n, pred_corr))
  p
}
