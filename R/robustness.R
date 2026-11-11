#' Search for local minima: multiple starting values
#'
#' Fits the same model from several sets of initial estimates. With oral data
#' only, absorption and disposition rate constants can be confounded
#' ("flip-flop"), and the likelihood can have local minima. Every start is a
#' regular run in the ledger (stage `"multistart"`).
#'
#' @param x A `pk_spec`, `pk_candidate`, run reference, or run id.
#' @param starts Tibble with one row per start. Columns are initial values named
#'   as the parameters (`ka`, `v2`, `q`, `v3`, `tlag`, ...); an optional column
#'   `set` labels the start.
#' @param data,est,fit_fun,quiet,proj See [pk_run()].
#' @return A tibble with one row per start: `set`, `run_id`, `ofv`, the final
#'   estimates of the structural parameters, and `best` (lowest OFV). The best
#'   run is attached as attribute `best_run`.
#' @export
pk_multistart <- function(x, starts, data = NULL, est = NULL, fit_fun = NULL, quiet = FALSE, proj = NULL) {
  starts <- tibble::as_tibble(starts)
  if (!"set" %in% names(starts)) starts$set <- paste0("S", seq_len(nrow(starts)))
  rows <- list()
  for (i in seq_len(nrow(starts))) {
    vals <- as.list(starts[i, setdiff(names(starts), "set")])
    cand <- do.call(pk_set_inits, c(list(x), vals, list(proj = proj)))
    r <- tryCatch(
      pk_run(cand,
        data = data, est = est, note = paste("Multistart", starts$set[i]), stage = "multistart",
        fit_fun = fit_fun, quiet = quiet, proj = proj
      ),
      error = function(e) {
        cli::cli_warn("Start {.val {starts$set[i]}} failed: {conditionMessage(e)}")
        NULL
      }
    )
    if (is.null(r)) {
      rows[[i]] <- tibble::tibble(set = starts$set[i], run_id = NA_character_, ofv = NA_real_)
      next
    }
    rows[[i]] <- dplyr::bind_cols(
      tibble::tibble(set = starts$set[i], run_id = r$run_id, ofv = pk_result(r)$ofv),
      estimates_row(r)
    )
  }
  out <- dplyr::bind_rows(rows)
  out$best <- !is.na(out$ofv) & out$ofv == min(out$ofv, na.rm = TRUE)
  best <- out$run_id[which(out$best)[1]]
  attr(out, "best_run") <- if (is.na(best)) NULL else pk_run_ref(best, proj = proj)
  out
}

# One-row tibble with the final (natural scale) estimates of the fixed effects
estimates_row <- function(run) {
  p <- pk_params_table(run)
  fx <- p[p$Type == "Fixed effect", ]
  v <- stats::setNames(as.list(fx$Estimate), sub("^l(ka|cl|v2|v3|v|q|tlag)$", "\\1", fx$Code))
  tibble::as_tibble(v)
}

#' Reproducibility of the OFV
#'
#' Re-estimates a model with different SAEM seeds and/or from randomly
#' perturbed initial estimates (with FOCEI only). The OFV of the same
#' population model should be reproducible within a few points; larger
#' differences point to local minima or multimodal individual likelihoods. The
#' runs are regular ledger runs (stage `"repro"`) with the original run as
#' parent.
#'
#' @param run A completed run (reference or id).
#' @param seeds SAEM seeds for repeated SAEM-based estimations (the seed of the
#'   original run is skipped).
#' @param perturb_starts Number of FOCEI fits from perturbed initial estimates.
#' @param perturb_sd Standard deviation of the log-normal perturbation of the
#'   structural parameters.
#' @param seed Seed for the perturbation.
#' @param data,fit_fun,quiet,proj See [pk_run()].
#' @return A tibble with the columns `label`, `method`, `run_id`, `ofv` and the
#'   estimates; the attribute `ofv_range` holds the range of the OFV across the
#'   original and the repeated runs.
#' @export
pk_repro <- function(run, seeds = c(7, 99), perturb_starts = 0, perturb_sd = 0.3, seed = 1,
                     data = NULL, fit_fun = NULL, quiet = FALSE, proj = NULL) {
  r <- resolve_run(run, proj)
  proj <- pk_refresh_quiet(r$proj)
  est0 <- read_run_file(proj$path, r$run_id, "settings.rds")
  ref_ref <- new_run_ref(proj, r$run_id)
  rows <- list()
  rows[[1]] <- dplyr::bind_cols(
    tibble::tibble(label = paste("original", est0$method, "seed", est0$seed), method = est0$method, run_id = r$run_id, ofv = pk_result(ref_ref)$ofv),
    estimates_row(ref_ref)
  )
  for (s in setdiff(seeds, est0$seed)) {
    est <- est0
    est$seed <- s
    rr <- pk_run(ref_ref,
      data = data, est = est, note = paste("Reproducibility: seed", s), stage = "repro",
      fit_fun = fit_fun, quiet = quiet, proj = proj
    )
    rows[[length(rows) + 1L]] <- dplyr::bind_cols(
      tibble::tibble(label = paste("seed", s), method = est$method, run_id = rr$run_id, ofv = pk_result(rr)$ofv),
      estimates_row(rr)
    )
  }
  if (perturb_starts > 0) {
    set.seed(seed)
    spec <- read_run_file(proj$path, r$run_id, "spec.rds")
    est <- est0
    est$method <- "focei"
    for (i in seq_len(perturb_starts)) {
      fac <- stats::setNames(exp(stats::rnorm(length(pk_params(spec)), 0, perturb_sd)), pk_params(spec))
      vals <- lapply(names(fac), function(p) signif(spec$inits[[p]] * fac[[p]], 4))
      names(vals) <- names(fac)
      cand <- do.call(pk_set_inits, c(list(ref_ref), vals, list(proj = proj)))
      rr <- pk_run(cand,
        data = data, est = est, note = paste("Reproducibility: perturbed start", i), stage = "repro",
        fit_fun = fit_fun, quiet = quiet, proj = proj
      )
      rows[[length(rows) + 1L]] <- dplyr::bind_cols(
        tibble::tibble(label = paste("perturbed start", i), method = "focei", run_id = rr$run_id, ofv = pk_result(rr)$ofv),
        estimates_row(rr)
      )
    }
  }
  out <- dplyr::bind_rows(rows)
  attr(out, "ofv_range") <- range(out$ofv, na.rm = TRUE)
  out
}
