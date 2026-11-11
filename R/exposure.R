#' Individual exposure and secondary parameters
#'
#' Derives individual secondary PK parameters from the empirical Bayes
#' estimates of a run (half-lives, volume of distribution at steady state) and
#' simulates the individual steady-state profile for a dosing regimen to obtain
#' Cmax, Tmax, Cmin and AUC over the dosing interval. `AUC_ss` is computed as
#' dose / CL (linear pharmacokinetics; `CL` is `CL/F` for oral data) and
#' `AUC_ss_sim` by the trapezoidal rule on the simulated profile.
#'
#' Dose and concentration units follow the data (for example dose in the unit
#' of `AMT`, concentration in dose unit per volume unit). The result is stored in
#' `models/<run>/eval/exposure.rds`.
#'
#' @param run A completed run (reference or id).
#' @param dose Dose amount per administration (same unit as `AMT` in the data).
#' @param ii Dosing interval (same time unit as the data).
#' @param n_doses Number of doses before the evaluated interval; the last
#'   interval is evaluated. Use a number large enough to reach steady state.
#' @param dt Time step of the simulated profile.
#' @param data Dataset with the subject covariates. Default: registered data.
#' @inheritParams pk_project
#' @return A tibble with one row per subject: individual parameters, secondary
#'   parameters, exposure metrics and the covariates of the dictionary.
#' @export
pk_exposure <- function(run, dose, ii = 24, n_doses = 14, dt = 0.1, data = NULL, proj = NULL) {
  r <- resolve_run(run, proj)
  spec <- read_run_file(r$proj$path, r$run_id, "spec.rds")
  if (!is.null(spec$custom_fn)) {
    cli::cli_abort("{.fn pk_exposure} needs a specification created with {.fn pk_spec}.")
  }
  fit <- pk_get(run, proj)
  res <- pk_result(run, proj)
  df <- as.data.frame(fit)
  df$ID <- as.numeric(as.character(df$ID))
  df <- df[!duplicated(df$ID), , drop = FALSE]
  params <- setdiff(pk_params(spec), "tlag")
  missing <- setdiff(params, names(df))
  if (length(missing)) {
    cli::cli_abort("Individual parameter(s) {.val {missing}} not found in the fit.")
  }
  ipar <- tibble::as_tibble(df[, c("ID", params)])
  if (spec$lag) {
    th <- res$theta
    ipar$tlag <- if ("eta.tlag" %in% names(df)) exp(th[["ltlag"]] + df$eta.tlag) else rep(exp(th[["ltlag"]]), nrow(df))
  }
  v1 <- if (spec$ncmt == 1) "v" else "v2"
  ipar$v1 <- ipar[[v1]]
  if (spec$ncmt == 2) {
    k20 <- ipar$cl / ipar$v2
    k23 <- ipar$q / ipar$v2
    k32 <- ipar$q / ipar$v3
    s <- k23 + k32 + k20
    beta <- 0.5 * (s - sqrt(s^2 - 4 * k32 * k20))
    alpha <- k32 * k20 / beta
    ipar$t_half_alpha <- log(2) / alpha
    ipar$t_half_beta <- log(2) / beta
    ipar$Vss <- ipar$v2 + ipar$v3
  } else {
    ipar$t_half_beta <- log(2) * ipar$v / ipar$cl
    ipar$Vss <- ipar$v
  }
  if (spec$route == "oral") ipar$t_half_abs <- log(2) / ipar$ka

  sim_mod <- exposure_model(spec)
  cmt <- if (spec$route == "oral") "depot" else "central"
  ev <- rxode2::et(amt = dose, ii = ii, addl = n_doses - 1, cmt = cmt)
  ev <- rxode2::et(ev, seq((n_doses - 1) * ii, n_doses * ii, by = dt))
  pars <- as.data.frame(ipar[, intersect(c("ka", "cl", "v", "v2", "q", "v3", "tlag"), names(ipar))])
  s <- rxode2::rxSolve(sim_mod, pars, ev, returnType = "data.frame")
  idcol <- intersect(c("sim.id", "id"), names(s))[1]
  s$.ID <- ipar$ID[as.integer(s[[idcol]])]
  t0 <- (n_doses - 1) * ii
  expo <- dplyr::summarise(
    dplyr::group_by(s, ID = .data$.ID),
    Cmax_ss = max(.data$cp), Tmax_ss = .data$time[which.max(.data$cp)] - t0, Cmin_ss = .data$cp[which.max(.data$time)],
    AUC_ss_sim = sum(diff(.data$time) * (utils::head(.data$cp, -1) + utils::tail(.data$cp, -1)) / 2),
    .groups = "drop"
  )
  out <- dplyr::left_join(ipar, expo, by = "ID")
  out$v1 <- NULL
  out$AUC_ss <- dose / out$cl
  out$Cavg_ss <- out$AUC_ss / ii
  if (is.null(data)) data <- safe(pk_data(proj = r$proj))
  if (!is.null(data)) {
    subj <- subject_table(data, r$proj)
    if (ncol(subj) > 1L) out <- dplyr::left_join(out, subj, by = "ID")
  }
  saveRDS(out, file.path(eval_dir(r$proj$path, r$run_id), "exposure.rds"))
  out
}

exposure_model <- function(spec) {
  v1 <- if (spec$ncmt == 1) "v" else "v2"
  lines <- c(
    if (spec$route == "oral") "d/dt(depot) <- -ka * depot",
    if (spec$route == "oral" && spec$lag) "alag(depot) <- tlag",
    if (spec$ncmt == 1) {
      c(
        sprintf("d/dt(central) <- %s - (cl / v) * central", if (spec$route == "oral") "ka * depot" else "0"),
        "cp <- central / v"
      )
    } else {
      c(
        sprintf("d/dt(central) <- %s - (cl / v2 + q / v2) * central + (q / v3) * periph", if (spec$route == "oral") "ka * depot" else "0"),
        "d/dt(periph) <- (q / v2) * central - (q / v3) * periph",
        "cp <- central / v2"
      )
    }
  )
  rxode2::rxode2(paste(lines, collapse = "\n"))
}

#' Summarise exposure metrics
#'
#' Geometric mean, geometric CV (%), median, 5th and 95th percentile of the
#' exposure and secondary parameters of [pk_exposure()], optionally by
#' subgroup.
#'
#' @param expo Result of [pk_exposure()].
#' @param by Optional column name(s) in `expo` defining subgroups (for
#'   continuous covariates use `cut()` first).
#' @param metrics Columns to summarise.
#' @return A tibble.
#' @export
pk_exposure_summary <- function(expo, by = NULL,
                                metrics = intersect(c("AUC_ss", "Cmax_ss", "Cmin_ss", "Cavg_ss", "Tmax_ss", "cl", "v", "v2", "t_half_beta", "Vss"), names(expo))) {
  long <- tidyr::pivot_longer(expo, tidyr::all_of(metrics), names_to = "Parameter", values_to = "value")
  long <- dplyr::group_by(long, dplyr::across(tidyr::all_of(c(by, "Parameter"))))
  out <- dplyr::summarise(
    long,
    n = sum(!is.na(.data$value)),
    `Geometric mean` = exp(mean(log(.data$value[.data$value > 0]), na.rm = TRUE)),
    `Geometric CV (%)` = 100 * sqrt(exp(stats::sd(log(.data$value[.data$value > 0]), na.rm = TRUE)^2) - 1),
    Median = stats::median(.data$value, na.rm = TRUE),
    `5th pct` = unname(stats::quantile(.data$value, 0.05, na.rm = TRUE)),
    `95th pct` = unname(stats::quantile(.data$value, 0.95, na.rm = TRUE)),
    .groups = "drop"
  )
  out$Parameter <- factor(out$Parameter, levels = metrics)
  dplyr::arrange(out, dplyr::across(tidyr::all_of(c(by, "Parameter"))))
}
