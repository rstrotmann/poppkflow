#' Standard diagnostics bundle for a run
#'
#' Computes the same set of numerical diagnostics for every run: OFV, AIC, BIC,
#' number of parameters, condition number, covariance step status, maximum ETA
#' shrinkage, epsilon shrinkage, CWRES summary, and the rule-based flags of
#' [pk_flags()]. The bundle is stored as `diag.rds` in the run folder.
#' Optionally the standard plots are created ([pk_plot_gof()],
#' [pk_plot_eta()], [pk_plot_ind()]) and saved as png files in
#' `models/<run>/diag/`.
#'
#' @param run A run reference or a run id.
#' @param plots Create and save the standard plots? Requires the full fit.
#' @param thresholds Named list overriding flag thresholds, see [pk_flags()].
#' @inheritParams pk_project
#' @return A `pk_diagnostics` object: a list with `summary` (one-row tibble),
#'   `flags`, and (if `plots = TRUE`) `plots`.
#' @export
pk_diagnostics <- function(run, plots = FALSE, thresholds = list(), proj = NULL) {
  r <- resolve_run(run, proj)
  d <- compute_diagnostics(r$proj, r$run_id, thresholds)
  if (plots) {
    out_dir <- file.path(run_dir(r$proj$path, r$run_id), "diag")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    ref <- new_run_ref(r$proj, r$run_id)
    pl <- list(gof = pk_plot_gof(ref), eta = pk_plot_eta(ref), individual = pk_plot_ind(ref))
    for (nm in names(pl)) {
      p <- pl[[nm]]
      if (inherits(p, "ggplot") || inherits(p, "patchwork")) {
        ggplot2::ggsave(file.path(out_dir, paste0(nm, ".png")), p, width = 9, height = 7, dpi = 120)
      }
    }
    d$plots <- pl
  }
  d
}

compute_diagnostics <- function(proj, run_id, thresholds = list()) {
  res <- read_run_file(proj$path, run_id, "result.rds")
  fl <- flags_from_result(res, effective_thresholds(proj, thresholds))
  cw <- res$cwres
  summ <- tibble::tibble(
    run_id = run_id, ofv = res$ofv, aic = res$aic, bic = res$bic, n_par = res$n_par,
    cond_no = res$cond_no, cov_step = res$cov_step, max_shrink_eta = res$max_shrink_eta,
    eps_shrink = res$eps_shrink,
    cwres_mean = num1(cw$mean), cwres_sd = num1(cw$sd),
    cwres_pct_gt2 = num1(cw$pct_gt2), cwres_pct_gt3 = num1(cw$pct_gt3), n_flags = nrow(fl)
  )
  d <- structure(list(summary = summ, flags = fl), class = "pk_diagnostics")
  saveRDS(d, file.path(run_dir(proj$path, run_id), "diag.rds"))
  d
}

#' @export
print.pk_diagnostics <- function(x, ...) {
  s <- x$summary
  cli::cli_text("{.cls pk_diagnostics} for {.val {s$run_id}}")
  cli::cli_text("OFV {format(s$ofv, digits = 8)}, {s$n_par} parameters, covariance step {if (isTRUE(s$cov_step)) 'ok' else 'failed'}")
  cli::cli_text("Max ETA shrinkage {format(s$max_shrink_eta, digits = 3)} %, mean CWRES {format(s$cwres_mean, digits = 2)}")
  if (nrow(x$flags)) {
    cli::cli_text("{nrow(x$flags)} flag{?s}:")
    for (m in x$flags$message) cli::cli_bullets(c("!" = m))
  } else {
    cli::cli_text("No flags.")
  }
  invisible(x)
}

# ---- plots ----------------------------------------------------------------------------------

diag_df <- function(run, proj = NULL) {
  df <- as.data.frame(pk_get(run, proj))
  df$ID <- as.numeric(as.character(df$ID))
  df
}

combine_plots <- function(plots, ncol = 2) {
  if (requireNamespace("patchwork", quietly = TRUE)) {
    patchwork::wrap_plots(plots, ncol = ncol)
  } else {
    plots
  }
}

smoother <- function() {
  ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "firebrick", linewidth = 0.7)
}

#' Diagnostic plots
#'
#' * `pk_plot_gof()`: observed vs PRED, observed vs IPRED, CWRES vs time, CWRES
#'   vs PRED. Returns a combined plot if the patchwork package is installed,
#'   otherwise a list of ggplots.
#' * `pk_plot_eta()`: histograms and QQ plots of the ETAs.
#' * `pk_plot_eta_cov()`: ETAs vs covariates (scatter plots with smoother for
#'   continuous, boxplots for categorical covariates).
#' * `pk_plot_ind()`: individual fits (observed, IPRED, PRED).
#'
#' @param run A run reference or a run id.
#' @param log Use log axes for concentrations?
#' @param covariates Covariate names; default: the covariate dictionary.
#' @param data Dataset that contains the covariates. Default: registered data.
#' @param ids Subject ids for individual fits; default: the first `n`.
#' @param n Number of subjects to show.
#' @inheritParams pk_project
#' @return A ggplot (or patchwork), or a list of ggplots.
#' @name pk_plots
NULL

#' @rdname pk_plots
#' @export
pk_plot_gof <- function(run, log = TRUE, proj = NULL) {
  df <- diag_df(run, proj)
  g <- df[!is.na(df$DV), ]
  if (log) {
    # observations before the lag time have a prediction of zero and cannot be shown on a log scale
    g <- g[g$DV > 0, ]
  }
  gp <- g[g$PRED > 0, ]
  gi <- if (log) g[g$IPRED > 0, ] else g
  lim <- range(c(g$DV, gi$IPRED, gp$PRED), na.rm = TRUE)
  scale_xy <- function() {
    if (log) {
      list(
        ggplot2::scale_x_log10(limits = pmax(lim, 1e-8)),
        ggplot2::scale_y_log10(limits = pmax(lim, 1e-8))
      )
    } else {
      NULL
    }
  }
  p1 <- ggplot2::ggplot(gp, ggplot2::aes(.data$PRED, .data$DV)) +
    ggplot2::geom_point(alpha = 0.4) + ggplot2::geom_abline(linetype = 2) + smoother() + scale_xy() +
    ggplot2::labs(x = "Population prediction", y = "Observed")
  p2 <- ggplot2::ggplot(gi, ggplot2::aes(.data$IPRED, .data$DV)) +
    ggplot2::geom_point(alpha = 0.4) + ggplot2::geom_abline(linetype = 2) + smoother() + scale_xy() +
    ggplot2::labs(x = "Individual prediction", y = "Observed")
  p3 <- ggplot2::ggplot(g, ggplot2::aes(.data$TIME, .data$CWRES)) +
    ggplot2::geom_point(alpha = 0.4) + ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_hline(yintercept = c(-2, 2), linetype = 3) + smoother() +
    ggplot2::labs(x = "Time", y = "CWRES")
  p4 <- ggplot2::ggplot(gp, ggplot2::aes(.data$PRED, .data$CWRES)) +
    ggplot2::geom_point(alpha = 0.4) + ggplot2::geom_hline(yintercept = 0, linetype = 2) +
    ggplot2::geom_hline(yintercept = c(-2, 2), linetype = 3) + smoother() +
    (if (log) ggplot2::scale_x_log10() else NULL) +
    ggplot2::labs(x = "Population prediction", y = "CWRES")
  theme <- ggplot2::theme_bw(base_size = 10)
  combine_plots(list(p1 + theme, p2 + theme, p3 + theme, p4 + theme), 2)
}

#' @rdname pk_plots
#' @export
pk_plot_eta <- function(run, proj = NULL) {
  res <- pk_result(run, proj)
  e <- res$eta
  if (is.null(e) || nrow(e) == 0L) {
    cli::cli_abort("The run has no ETAs.")
  }
  long <- tidyr::pivot_longer(e, tidyr::starts_with("eta."), names_to = "ETA", values_to = "value")
  p_h <- ggplot2::ggplot(long, ggplot2::aes(.data$value)) +
    ggplot2::geom_histogram(bins = 15, fill = "steelblue", colour = "white") +
    ggplot2::facet_wrap(~ETA, scales = "free") + ggplot2::labs(x = "ETA", y = "Count") + ggplot2::theme_bw(base_size = 10)
  p_q <- ggplot2::ggplot(long, ggplot2::aes(sample = .data$value)) +
    ggplot2::stat_qq() + ggplot2::stat_qq_line() + ggplot2::facet_wrap(~ETA, scales = "free") +
    ggplot2::labs(x = "Theoretical quantiles", y = "Sample quantiles") + ggplot2::theme_bw(base_size = 10)
  combine_plots(list(p_h, p_q), 1)
}

#' @rdname pk_plots
#' @export
pk_plot_eta_cov <- function(run, covariates = NULL, data = NULL, proj = NULL) {
  tab <- eta_cov_table(run, covariates, data, proj)
  eta_cols <- grep("^eta\\.", names(tab$data), value = TRUE)
  plots <- list()
  cont <- tab$continuous
  cat <- tab$categorical
  if (length(cont)) {
    long <- tidyr::pivot_longer(tab$data, tidyr::all_of(eta_cols), names_to = "ETA", values_to = "value")
    long <- tidyr::pivot_longer(long, tidyr::all_of(cont), names_to = "Covariate", values_to = "cov")
    plots$continuous <- ggplot2::ggplot(long, ggplot2::aes(.data$cov, .data$value)) +
      ggplot2::geom_point(alpha = 0.5) + ggplot2::geom_hline(yintercept = 0, linetype = 2) + smoother() +
      ggplot2::facet_grid(ETA ~ Covariate, scales = "free") +
      ggplot2::labs(x = NULL, y = "ETA") + ggplot2::theme_bw(base_size = 10)
  }
  if (length(cat)) {
    d <- tab$data
    for (cn in cat) d[[cn]] <- as.character(d[[cn]])
    long <- tidyr::pivot_longer(d, tidyr::all_of(eta_cols), names_to = "ETA", values_to = "value")
    long <- tidyr::pivot_longer(long, tidyr::all_of(cat), names_to = "Covariate", values_to = "level")
    plots$categorical <- ggplot2::ggplot(long, ggplot2::aes(.data$level, .data$value)) +
      ggplot2::geom_boxplot(outlier.shape = NA, fill = "grey90") + ggplot2::geom_jitter(width = 0.15, alpha = 0.4) +
      ggplot2::geom_hline(yintercept = 0, linetype = 2) + ggplot2::facet_grid(ETA ~ Covariate, scales = "free") +
      ggplot2::labs(x = NULL, y = "ETA") + ggplot2::theme_bw(base_size = 10)
  }
  if (length(plots) == 1L) plots[[1]] else plots
}

#' @rdname pk_plots
#' @export
pk_plot_ind <- function(run, ids = NULL, n = 12, log = TRUE, proj = NULL) {
  df <- diag_df(run, proj)
  all_ids <- unique(df$ID)
  ids <- ids %||% utils::head(all_ids, n)
  g <- df[df$ID %in% ids, ]
  if (log) {
    g$PRED[!is.na(g$PRED) & g$PRED <= 0] <- NA
    g$IPRED[!is.na(g$IPRED) & g$IPRED <= 0] <- NA
    g$DV[!is.na(g$DV) & g$DV <= 0] <- NA
  }
  p <- ggplot2::ggplot(g, ggplot2::aes(.data$TIME)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$DV), na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$IPRED), colour = "firebrick", na.rm = TRUE) +
    ggplot2::geom_line(ggplot2::aes(y = .data$PRED), colour = "steelblue", linetype = 2, na.rm = TRUE) +
    ggplot2::facet_wrap(~ID, scales = "free_y") +
    ggplot2::labs(x = "Time", y = "Concentration", subtitle = "Points: observed; red: IPRED; blue dashed: PRED") +
    ggplot2::theme_bw(base_size = 10)
  if (log) p <- p + ggplot2::scale_y_log10()
  p
}

# ETAs joined with the first-record covariates of each subject
eta_cov_table <- function(run, covariates = NULL, data = NULL, proj = NULL) {
  r <- resolve_run(run, proj)
  res <- read_run_file(r$proj$path, r$run_id, "result.rds")
  if (is.null(res$eta) || nrow(res$eta) == 0L) {
    cli::cli_abort("The run has no ETAs.")
  }
  if (is.null(data)) data <- pk_data(proj = r$proj)
  dict <- cov_dict_to_tibble(safe(yaml::read_yaml(proj_path(r$proj, "config.yml"))$covariates, list()))
  covariates <- covariates %||% dict$name
  covariates <- intersect(covariates, names(data))
  if (!length(covariates)) {
    cli::cli_abort("No covariates found. Define them with {.fn pk_covariates} or pass {.arg covariates}.")
  }
  subj <- subject_table(data, r$proj, covariates)
  type_of <- function(cn) {
    t <- dict$type[match(cn, dict$name)]
    if (!is.na(t)) {
      return(t)
    }
    x <- subj[[cn]]
    if (is.numeric(x) && length(unique(x[!is.na(x)])) > 5L) "continuous" else "categorical"
  }
  types <- vapply(covariates, type_of, character(1))
  list(
    data = dplyr::left_join(res$eta, subj, by = "ID"),
    continuous = covariates[types == "continuous"],
    categorical = covariates[types == "categorical"]
  )
}
