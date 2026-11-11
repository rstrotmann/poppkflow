#' Estimation settings
#'
#' Creates the settings object used by [pk_run()]. Unspecified arguments are
#' taken from the `estimation` section of the project config.
#'
#' The default method `"saem_focei"` estimates the model with SAEM and then
#' refines it with FOCEI started from the SAEM solution. With oral data only
#' and slow absorption (`ka` smaller than the elimination rate constant) the
#' individual likelihood surfaces can be multimodal. FOCEI alone can then
#' converge to different sets of individual modes depending on the starting
#' values, and the reported OFV of the same population model can differ by
#' several tens of points. SAEM locates the population optimum and consistent
#' individual modes robustly, and FOCEI started from it provides the FOCEI OFV
#' used for likelihood-ratio tests and the covariance step. The OFV
#' reproducibility across SAEM seeds can be checked with [pk_repro()].
#'
#' @param method `"saem_focei"` (default), `"focei"` or `"saem"`.
#' @param seed Seed for SAEM.
#' @param nBurn,nEm Number of SAEM burn-in and EM iterations.
#' @param maxOuterIterations Maximum outer iterations of FOCEI.
#' @param covMethod Covariance step method for FOCEI (e.g. `"r,s"`).
#' @param ... Further settings stored with the run (not used by the default
#'   fit function).
#' @param proj A `pk_project`; only used to read defaults.
#' @return A `pk_est` object (a list).
#' @examples
#' pk_est(method = "focei")
#' @export
pk_est <- function(method = NULL, seed = NULL, nBurn = NULL, nEm = NULL,
                   maxOuterIterations = NULL, covMethod = NULL, ..., proj = NULL) {
  defaults <- tryCatch(resolve_proj(proj)$config$estimation, error = function(e) NULL)
  defaults <- utils::modifyList(
    list(method = "saem_focei", seed = 2026, nBurn = 300, nEm = 300, maxOuterIterations = 2000, covMethod = "r,s"),
    defaults %||% list()
  )
  given <- Filter(Negate(is.null), list(
    method = method, seed = seed, nBurn = nBurn, nEm = nEm,
    maxOuterIterations = maxOuterIterations, covMethod = covMethod
  ))
  est <- utils::modifyList(defaults, c(given, list(...)))
  est$method <- match.arg(est$method, c("saem_focei", "focei", "saem"))
  structure(est, class = "pk_est")
}

#' @export
print.pk_est <- function(x, ...) {
  cli::cli_text("{.cls pk_est} {.val {x$method}}, seed {x$seed}")
  invisible(x)
}

# Default fit function: nlmixr2 estimation according to a pk_est object.
# Returns the fit with attributes elapsed_s and saem_objf.
fit_model <- function(model, data, est) {
  t0 <- Sys.time()
  tbl <- nlmixr2est::tableControl(cwres = TRUE, npde = FALSE)
  tbl_fast <- nlmixr2est::tableControl(cwres = FALSE, npde = FALSE)
  fit <- NULL
  saem_objf <- NA_real_
  if (est$method %in% c("saem", "saem_focei")) {
    ctrl <- nlmixr2est::saemControl(print = 0, nBurn = est$nBurn, nEm = est$nEm, seed = est$seed)
    # In the two-stage strategy the SAEM stage only provides the starting values for FOCEI, so the
    # (slow) table step is skipped; the SAEM objective function value is then not available.
    fit <- suppressMessages(nlmixr2est::nlmixr2(
      model, data,
      est = "saem", control = ctrl,
      table = if (est$method == "saem") tbl else tbl_fast
    ))
    saem_objf <- num1(fit$objf)
    if (!is.finite(saem_objf)) saem_objf <- NA_real_
  }
  if (est$method %in% c("focei", "saem_focei")) {
    ctrl <- nlmixr2est::foceiControl(
      print = 0, maxOuterIterations = est$maxOuterIterations,
      covMethod = est$covMethod, linCmtSensCarry = "none"
    )
    fit <- suppressMessages(nlmixr2est::nlmixr2(
      if (is.null(fit)) model else fit, data,
      est = "focei", control = ctrl, table = tbl
    ))
  }
  attr(fit, "elapsed_s") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  attr(fit, "saem_objf") <- saem_objf
  fit
}

est_hash <- function(est) {
  # the seed is part of the key; settings that do not affect results are excluded
  pk_hash(unclass(est)[sort(names(est))])
}
