default_thresholds <- function() {
  list(
    max_shrinkage_eta = 30, max_shrinkage_eps = 30, max_rse = 50,
    max_condition_number = 1000, min_omega = 1e-4, max_abs_mean_cwres = 0.5,
    max_pct_cwres_gt3 = 2
  )
}

empty_flags <- function() {
  tibble::tibble(flag = character(), severity = character(), parameter = character(), value = numeric(), threshold = numeric(), message = character())
}

#' Rule-based warnings for a run
#'
#' Evaluates a set of rules on the result of a run and returns the triggered
#' flags. Flags are hints for the analyst, not decisions. Rules and default
#' thresholds (configurable in the `flags` section of `config.yml`, or with
#' `thresholds`):
#'
#' * `cov_step_failed`: the covariance step did not run or failed.
#' * `condition_number`: condition number above `max_condition_number` (1000).
#' * `eta_shrinkage`: SD shrinkage of an ETA above `max_shrinkage_eta` (30 %).
#' * `eps_shrinkage`: IWRES shrinkage above `max_shrinkage_eps` (30 %).
#' * `high_rse`: relative standard error of a fixed effect above `max_rse` (50 %).
#' * `omega_near_zero`: IIV variance below `min_omega` (1e-4).
#' * `cwres_bias`: absolute mean CWRES above `max_abs_mean_cwres` (0.5).
#' * `cwres_tails`: more than `max_pct_cwres_gt3` (2 %) of |CWRES| above 3.
#' * `no_ofv`: the OFV is missing or not finite.
#'
#' @param run A run reference or a run id.
#' @param thresholds Named list overriding the thresholds.
#' @inheritParams pk_project
#' @return A tibble with the columns `flag`, `severity`, `parameter`, `value`,
#'   `threshold`, `message`; zero rows if nothing is flagged.
#' @export
pk_flags <- function(run, thresholds = list(), proj = NULL) {
  r <- resolve_run(run, proj)
  res <- read_run_file(r$proj$path, r$run_id, "result.rds")
  flags_from_result(res, effective_thresholds(r$proj, thresholds))
}

effective_thresholds <- function(proj, thresholds = list()) {
  cfg <- safe(proj$config$flags)
  if (is.null(cfg)) cfg <- safe(yaml::read_yaml(proj_path(proj, "config.yml"))$flags)
  utils::modifyList(utils::modifyList(default_thresholds(), cfg %||% list()), thresholds)
}

flags_from_result <- function(res, thr = default_thresholds()) {
  out <- list()
  add <- function(flag, severity, parameter, value, threshold, message) {
    out[[length(out) + 1L]] <<- tibble::tibble(
      flag = flag, severity = severity, parameter = parameter,
      value = as.numeric(value), threshold = as.numeric(threshold), message = message
    )
  }
  if (!is.finite(num1(res$ofv))) {
    add("no_ofv", "error", NA_character_, NA_real_, NA_real_, "The objective function value is missing or not finite.")
  }
  if (!isTRUE(res$cov_step)) {
    add("cov_step_failed", "warning", NA_character_, NA_real_, NA_real_, "Covariance step failed or was not run; RSEs are not available.")
  }
  cn <- num1(res$cond_no)
  if (is.finite(cn) && cn > thr$max_condition_number) {
    add("condition_number", "warning", NA_character_, cn, thr$max_condition_number, sprintf("Condition number %.0f is high (possible over-parameterisation).", cn))
  }
  es <- res$eta_shrink
  for (nm in names(es)) {
    if (is.finite(es[[nm]]) && es[[nm]] > thr$max_shrinkage_eta) {
      add("eta_shrinkage", "warning", nm, es[[nm]], thr$max_shrinkage_eta, sprintf("Shrinkage of %s is %.0f %%; individual estimates are uninformative for this parameter.", nm, es[[nm]]))
    }
  }
  eps <- num1(res$eps_shrink)
  if (is.finite(eps) && eps > thr$max_shrinkage_eps) {
    add("eps_shrinkage", "warning", "IWRES", eps, thr$max_shrinkage_eps, sprintf("Epsilon shrinkage %.0f %%; IPRED-based diagnostics are less reliable.", eps))
  }
  p <- res$parameters
  if (!is.null(p) && nrow(p)) {
    fx <- p[p$Type == "Fixed effect" & !is.na(p$`RSE (%)`), ]
    for (i in seq_len(nrow(fx))) {
      if (is.finite(fx$`RSE (%)`[i]) && fx$`RSE (%)`[i] > thr$max_rse) {
        add("high_rse", "warning", fx$Code[i], fx$`RSE (%)`[i], thr$max_rse, sprintf("RSE of %s is %.0f %%.", fx$Code[i], fx$`RSE (%)`[i]))
      }
    }
    om <- p[p$Type == "Random effect (variance)", ]
    for (i in seq_len(nrow(om))) {
      if (is.finite(om$Estimate[i]) && om$Estimate[i] < thr$min_omega) {
        add("omega_near_zero", "warning", om$Code[i], om$Estimate[i], thr$min_omega, sprintf("Variance of %s is practically zero; consider removing the random effect.", om$Code[i]))
      }
    }
  }
  cw <- res$cwres
  if (!is.null(cw)) {
    if (is.finite(cw$mean) && abs(cw$mean) > thr$max_abs_mean_cwres) {
      add("cwres_bias", "warning", "CWRES", cw$mean, thr$max_abs_mean_cwres, sprintf("Mean CWRES %.2f deviates from zero.", cw$mean))
    }
    if (is.finite(cw$pct_gt3) && cw$pct_gt3 > thr$max_pct_cwres_gt3) {
      add("cwres_tails", "warning", "CWRES", cw$pct_gt3, thr$max_pct_cwres_gt3, sprintf("%.1f %% of |CWRES| > 3 (heavy tails or outliers).", cw$pct_gt3))
    }
  }
  if (!length(out)) {
    return(empty_flags())
  }
  dplyr::bind_rows(out)
}

#' Non-binding suggestions for the next modelling step
#'
#' Translates the flags of a run into hints, for example "consider removing IIV
#' on KA" for a high ETA shrinkage. The analyst decides; the hints are derived
#' purely from rules.
#'
#' @param run A run reference or a run id.
#' @inheritParams pk_flags
#' @return A character vector of hints (empty if there is nothing to suggest).
#' @export
pk_suggest <- function(run, thresholds = list(), proj = NULL) {
  fl <- pk_flags(run, thresholds, proj)
  hint <- character()
  for (i in seq_len(nrow(fl))) {
    f <- fl[i, ]
    pname <- sub("^eta\\.", "", f$parameter)
    hint <- c(hint, switch(f$flag,
      eta_shrinkage = sprintf("Shrinkage of %s is %.0f %%: consider removing the random effect on %s, or do not use its EBEs for covariate screening.", f$parameter, f$value, pname),
      omega_near_zero = sprintf("Variance of %s is about zero: consider removing the random effect on %s.", f$parameter, pname),
      cov_step_failed = "Covariance step failed: simplify the model (fewer random effects or covariances), improve initial estimates, or try another estimation method.",
      condition_number = "High condition number: check for correlated parameters or over-parameterisation (e.g. remove a covariance or an IIV term).",
      high_rse = sprintf("Parameter %s is imprecise (RSE %.0f %%): consider fixing it, removing it, or simplifying the model.", f$parameter, f$value),
      cwres_bias = "CWRES are biased: review the structural model (e.g. lag time, additional compartment, absorption model).",
      cwres_tails = "Heavy CWRES tails: review the residual error model (e.g. combined error) and check outliers.",
      eps_shrinkage = "High epsilon shrinkage: use population-level (PRED, CWRES) rather than IPRED-based diagnostics.",
      no_ofv = "The run produced no valid OFV: check the model code and initial estimates.",
      f$message
    ))
  }
  unique(hint)
}
