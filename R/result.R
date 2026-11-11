# Extraction of a slim, fit-independent result from an nlmixr2 fit. The result is stored
# as result.rds next to fit.rds so that ledger tools never need to load the full fit.

safe <- function(expr, default = NULL) {
  tryCatch(expr, error = function(e) default)
}

extract_result <- function(fit, runtime_s = NA_real_) {
  pf <- safe(as.data.frame(fit$parFixedDf))
  theta <- safe(fit$theta)
  omega <- safe(fit$omega)
  shrink <- safe(as.data.frame(fit$shrink))
  ini <- safe(as.data.frame(fit$iniDf))

  ofv <- num1(safe(fit$objf))
  objdf <- safe(fit$objDf)
  aic <- num1(safe(objdf$AIC))
  bic <- num1(safe(objdf$BIC))

  # numbers of estimated parameters
  if (!is.null(ini) && all(c("fix", "ntheta", "neta1", "neta2") %in% names(ini))) {
    est <- ini[!ini$fix, ]
    n_theta <- sum(!is.na(est$ntheta))
    n_omega <- sum(is.na(est$ntheta) & !is.na(est$neta1))
    n_par <- n_theta + n_omega
    n_eta <- sum(!is.na(ini$neta1) & ini$neta1 == ini$neta2 & is.na(ini$ntheta))
  } else {
    n_theta <- if (is.null(pf)) NA_integer_ else nrow(pf)
    n_eta <- if (is.null(omega)) NA_integer_ else nrow(omega)
    n_omega <- if (is.null(omega)) 0L else sum(diag(omega) != 0) + sum(omega[lower.tri(omega)] != 0)
    n_par <- n_theta + n_omega
  }
  if (is.na(aic) && !is.na(ofv)) aic <- ofv + 2 * n_par

  eta_shrink <- NULL
  eps_shrink <- NA_real_
  if (!is.null(shrink)) {
    row <- if ("sd shrinkage (%)" %in% rownames(shrink)) "sd shrinkage (%)" else rownames(shrink)[1]
    v <- unlist(shrink[row, , drop = TRUE])
    eps_shrink <- num1(v[["IWRES"]] %||% NA)
    eta_shrink <- v[grepl("^eta\\.|^iov\\.", names(v))]
  }

  eta_tbl <- safe({
    e <- as.data.frame(fit$eta)
    e$ID <- as.numeric(as.character(e$ID))
    tibble::as_tibble(e)
  })

  cw <- safe({
    df <- as.data.frame(fit)
    x <- df$CWRES[!is.na(df$CWRES)]
    list(
      n = length(x), mean = mean(x), sd = stats::sd(x),
      pct_gt2 = 100 * mean(abs(x) > 2), pct_gt3 = 100 * mean(abs(x) > 3)
    )
  })

  list(
    ofv = ofv, saem_ofv = num1(attr(fit, "saem_objf")), aic = aic, bic = bic,
    n_theta = as.integer(n_theta), n_eta = as.integer(n_eta), n_par = as.integer(n_par),
    cond_no = num1(safe(fit$conditionNumberCor)), cov_step = !is.null(safe(fit$cov)),
    theta = theta, omega = omega, shrink = shrink, eta_shrink = eta_shrink, eps_shrink = eps_shrink,
    parameters = parameter_table(pf, omega, shrink), eta = eta_tbl, cwres = cw,
    runtime_s = if (is.na(runtime_s)) num1(attr(fit, "elapsed_s")) else runtime_s,
    max_shrink_eta = if (length(eta_shrink)) max(eta_shrink, na.rm = TRUE) else NA_real_
  )
}

# Parameter table: fixed effects back-transformed, random effects as variances and CV(%),
# covariances with correlation, with RSE, CI and shrinkage.
parameter_table <- function(pf, omega, shrink) {
  out <- list()
  if (!is.null(pf) && nrow(pf)) {
    est <- pf$Estimate
    bt <- if ("Back-transformed" %in% names(pf)) pf$`Back-transformed` else rep(NA_real_, nrow(pf))
    is_log <- !is.na(bt) & !is.na(est) & abs(bt - exp(est)) < 1e-6 * pmax(1, abs(bt))
    se <- if ("SE" %in% names(pf)) pf$SE else rep(NA_real_, nrow(pf))
    ci_lo <- if ("CI Lower" %in% names(pf)) pf$`CI Lower` else rep(NA_real_, nrow(pf))
    ci_hi <- if ("CI Upper" %in% names(pf)) pf$`CI Upper` else rep(NA_real_, nrow(pf))
    out$theta <- tibble::tibble(
      Type = "Fixed effect", Code = rownames(pf), LogScale = is_log,
      Estimate = ifelse(is_log, bt, est),
      `RSE (%)` = ifelse(is_log, 100 * se, 100 * se / abs(est)),
      `95% CI lower` = ifelse(is_log, ci_lo, est - 1.96 * se),
      `95% CI upper` = ifelse(is_log, ci_hi, est + 1.96 * se),
      `Shrinkage (%)` = NA_real_, `CV (%)` = NA_real_, Correlation = NA_real_
    )
  }
  if (!is.null(omega) && nrow(omega)) {
    nm <- rownames(omega)
    sh_row <- if (!is.null(shrink) && "sd shrinkage (%)" %in% rownames(shrink)) "sd shrinkage (%)" else NULL
    sh <- vapply(nm, function(n) {
      if (!is.null(sh_row) && n %in% names(shrink)) num1(shrink[sh_row, n]) else NA_real_
    }, numeric(1))
    out$omega <- tibble::tibble(
      Type = "Random effect (variance)", Code = nm, LogScale = NA,
      Estimate = unname(diag(omega)), `RSE (%)` = NA_real_, `95% CI lower` = NA_real_,
      `95% CI upper` = NA_real_, `Shrinkage (%)` = unname(sh),
      `CV (%)` = 100 * sqrt(exp(unname(diag(omega))) - 1), Correlation = NA_real_
    )
    cov_rows <- list()
    for (i in seq_len(nrow(omega))[-1]) {
      for (j in seq_len(i - 1L)) {
        if (omega[i, j] != 0) {
          cov_rows[[length(cov_rows) + 1L]] <- tibble::tibble(
            Type = "Random effect (covariance)", Code = paste0("cov(", nm[j], ",", nm[i], ")"), LogScale = NA,
            Estimate = omega[i, j], `RSE (%)` = NA_real_, `95% CI lower` = NA_real_,
            `95% CI upper` = NA_real_, `Shrinkage (%)` = NA_real_, `CV (%)` = NA_real_,
            Correlation = omega[i, j] / sqrt(omega[i, i] * omega[j, j])
          )
        }
      }
    }
    out$cov <- dplyr::bind_rows(cov_rows)
  }
  dplyr::bind_rows(out)
}

#' Parameter table of a run
#'
#' Fixed effects (back-transformed from the log scale where applicable), random
#' effects as variances with CV (%), covariances with correlations, relative
#' standard errors, 95% confidence intervals and shrinkage.
#'
#' @param run A run reference or a run id.
#' @inheritParams pk_project
#' @return A tibble.
#' @export
pk_params_table <- function(run, proj = NULL) {
  pk_result(run, proj = proj)$parameters
}
