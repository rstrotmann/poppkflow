#' Stepwise covariate modelling (SCM)
#'
#' Forward inclusion followed by backward elimination, starting from a base
#' run. Every model that is fitted is a regular run in the ledger (stage
#' `"scm"`), with the current model as parent, so the complete search is
#' documented in the run tree. The step table is stored in `scm/<scm_id>.rds`.
#'
#' Forward step: all remaining candidates are added one at a time to the
#' current model; the candidate with the largest drop in OFV is included if the
#' drop exceeds the chi-square criterion with 1 df at `forward_p` (default 0.01,
#' dOFV > 6.63). Backward step: each included candidate is removed one at a
#' time; the one with the smallest increase in OFV is removed if the increase
#' is below the criterion at `backward_p` (default 0.001, dOFV > 10.83).
#'
#' Runs that were not selected in a step are marked as rejected (with the
#' dOFV in the reason) when `decide_trials = TRUE`; runs on the selected path
#' stay pending so that the analyst records the decision.
#'
#' @param base Base run (reference or id), completed.
#' @param candidates Tibble with one row per covariate relationship and the
#'   columns `param`, `covariate`, `form`, `ref` (for `power` and `linear`) and
#'   `init`. Relationships with a fixed parameter are not supported (a
#'   likelihood-ratio test needs a free parameter).
#' @param forward_p,backward_p Significance levels (1 df). Defaults from the
#'   `scm` section of `config.yml`.
#' @param data Data frame. Default: the registered dataset.
#' @param est Estimation settings, see [pk_est()].
#' @param require_cov_step Only select candidates whose model has a successful
#'   covariance step in the forward step?
#' @param inits_from_parent Start each trial model from the final estimates of
#'   the current model?
#' @param decide_trials Mark non-selected trial runs as rejected?
#' @param fit_fun,quiet,proj See [pk_run()].
#'
#' @return A `pk_scm` object: a list with `steps` (tibble), `final` (run
#'   reference of the final model), `selected`, `forward_selected`, `criteria`,
#'   and `scm_id`.
#' @examples
#' \donttest{
#' cands <- tibble::tribble(
#'   ~param, ~covariate, ~form   , ~ref, ~init,
#'   "cl"  , "WT"      , "power" , 70  , 0.75 ,
#'   "cl"  , "CRCL"    , "power" , 100 , 0.1  ,
#'   "v2"  , "WT"      , "power" , 70  , 1
#' )
#' # scm <- pk_scm(base_run, cands)
#' }
#' @export
pk_scm <- function(base, candidates, forward_p = NULL, backward_p = NULL, data = NULL, est = NULL,
                   require_cov_step = TRUE, inits_from_parent = TRUE, decide_trials = TRUE,
                   fit_fun = NULL, quiet = FALSE, proj = NULL) {
  b <- resolve_run(base, proj)
  proj <- pk_refresh_quiet(b$proj)
  cfg <- proj$config$scm
  forward_p <- forward_p %||% cfg$forward_p %||% 0.01
  backward_p <- backward_p %||% cfg$backward_p %||% 0.001
  crit_fwd <- stats::qchisq(1 - forward_p, 1)
  crit_bwd <- stats::qchisq(1 - backward_p, 1)

  candidates <- tibble::as_tibble(candidates)
  for (col in c("param", "covariate", "form")) {
    if (!col %in% names(candidates)) cli::cli_abort("{.arg candidates} needs the column {.field {col}}.")
  }
  if (!"ref" %in% names(candidates)) candidates$ref <- NA_real_
  if (!"init" %in% names(candidates)) candidates$init <- 0
  if ("fix" %in% names(candidates) && any(candidates$fix, na.rm = TRUE)) {
    cli::cli_abort("Candidates with fixed parameters cannot be tested in SCM.")
  }
  if (any(candidates$form == "custom")) {
    cli::cli_abort("Custom covariate forms are not supported in {.fn pk_scm}.")
  }
  candidates$id <- paste0(toupper(candidates$param), "~", toupper(candidates$covariate))

  base_ref <- new_run_ref(proj, b$run_id)
  base_row <- ledger_get(proj, b$run_id)
  if (!identical(base_row$status, "completed")) {
    cli::cli_abort("The base run {.val {b$run_id}} is not completed.")
  }
  base_spec <- read_run_file(proj$path, b$run_id, "spec.rds")
  selected <- setdiff(base_spec$covariates$id, character())
  already <- selected
  scm_files <- list.files(proj_path(proj, "scm"), pattern = "^scm[0-9]+\\.rds$")
  scm_id <- sprintf("scm%02d", length(scm_files) + 1L)
  event_append(proj, b$run_id, "scm_started", paste0(scm_id, "; candidates=", paste(candidates$id, collapse = ",")))

  steps <- list()
  current <- base_ref
  cur_ofv <- base_row$ofv

  run_trial <- function(cand, step, tested) {
    if (inits_from_parent) cand <- pk_inits_from(cand, current, proj = proj)
    r <- tryCatch(
      pk_run(cand,
        data = data, est = est, note = sprintf("SCM %s: %s %s", step, if (step == "forward") "add" else "remove", tested),
        stage = "scm", fit_fun = fit_fun, quiet = quiet, proj = proj, refit = FALSE
      ),
      error = function(e) {
        cli::cli_warn("SCM trial {.val {tested}} ({step}) failed: {conditionMessage(e)}")
        NULL
      }
    )
    r
  }
  step_row <- function(r, step, tested, model_ids, theta = NA_character_) {
    if (is.null(r)) {
      return(tibble::tibble(
        Step = step, Tested = tested, Run = NA_character_, Reference = current$run_id, OFV = NA_real_,
        dOFV = NA_real_, Estimate = NA_real_, RSE = NA_real_, covStep = NA, Selected = FALSE, Model = paste(model_ids, collapse = " + ")
      ))
    }
    res <- pk_result(r)
    pt <- res$parameters
    est_v <- if (!is.na(theta)) num1(pt$Estimate[pt$Code == theta]) else NA_real_
    rse_v <- if (!is.na(theta)) num1(pt$`RSE (%)`[pt$Code == theta]) else NA_real_
    tibble::tibble(
      Step = step, Tested = tested, Run = r$run_id, Reference = current$run_id, OFV = res$ofv,
      dOFV = res$ofv - cur_ofv, Estimate = est_v, RSE = rse_v, covStep = res$cov_step, Selected = FALSE,
      Model = paste(model_ids, collapse = " + ")
    )
  }

  # ---- forward inclusion
  repeat {
    remaining <- setdiff(candidates$id, selected)
    if (!length(remaining)) break
    res_step <- list()
    for (cid in remaining) {
      cr <- candidates[candidates$id == cid, ]
      cand <- pk_add_cov(current, cr$param, cr$covariate, form = cr$form, ref = if (is.na(cr$ref)) NULL else cr$ref,
        init = cr$init, proj = proj
      )
      theta <- cand$spec$covariates$theta[cand$spec$covariates$id == cid]
      r <- run_trial(cand, "forward", cid)
      res_step[[length(res_step) + 1L]] <- step_row(r, "forward", cid, c(selected, cid), theta)
    }
    st <- dplyr::bind_rows(res_step)
    ok <- !is.na(st$dOFV) & (!require_cov_step | isTRUE_vec(st$covStep))
    best_i <- if (any(ok)) which(ok)[which.min(st$dOFV[ok])] else NA_integer_
    pick <- !is.na(best_i) && st$dOFV[best_i] < -crit_fwd
    if (pick) st$Selected[best_i] <- TRUE
    steps[[length(steps) + 1L]] <- st
    if (!pick) break
    selected <- c(selected, st$Tested[best_i])
    current <- new_run_ref(proj, st$Run[best_i])
    cur_ofv <- st$OFV[best_i]
  }
  forward_selected <- selected

  # ---- backward elimination
  removable <- setdiff(selected, already)
  repeat {
    removable <- intersect(removable, selected)
    if (!length(removable)) break
    res_step <- list()
    for (cid in removable) {
      cand <- pk_drop_cov(current, cid, proj = proj)
      r <- run_trial(cand, "backward", cid)
      res_step[[length(res_step) + 1L]] <- step_row(r, "backward", cid, setdiff(selected, cid))
    }
    st <- dplyr::bind_rows(res_step)
    ok <- !is.na(st$dOFV)
    worst_i <- if (any(ok)) which(ok)[which.min(st$dOFV[ok])] else NA_integer_
    drop <- !is.na(worst_i) && st$dOFV[worst_i] < crit_bwd
    if (drop) st$Selected[worst_i] <- TRUE
    steps[[length(steps) + 1L]] <- st
    if (!drop) break
    selected <- setdiff(selected, st$Tested[worst_i])
    current <- new_run_ref(proj, st$Run[worst_i])
    cur_ofv <- st$OFV[worst_i]
  }

  steps <- dplyr::bind_rows(steps)
  if (decide_trials && nrow(steps)) {
    on_path <- c(base_ref$run_id, steps$Run[steps$Selected])
    for (i in seq_len(nrow(steps))) {
      if (!is.na(steps$Run[i]) && !steps$Selected[i] && !steps$Run[i] %in% on_path) {
        row <- ledger_get(proj, steps$Run[i])
        if (identical(row$status, "completed") && identical(row$decision, "pending")) {
          pk_decide(steps$Run[i], "rejected",
            reason = sprintf("SCM %s step, %s: dOFV %.2f, not selected (criterion %.2f)", steps$Step[i], steps$Tested[i], steps$dOFV[i],
              if (steps$Step[i] == "forward") -crit_fwd else crit_bwd
            ),
            quiet = TRUE, proj = proj
          )
        }
      }
    }
  }
  out <- structure(
    list(
      steps = steps, final = current, selected = selected, forward_selected = forward_selected,
      criteria = c(forward = crit_fwd, backward = crit_bwd), scm_id = scm_id, base = b$run_id
    ),
    class = "pk_scm"
  )
  saveRDS(out, proj_path(proj, "scm", paste0(scm_id, ".rds")))
  event_append(proj, current$run_id, "scm_completed", paste0(scm_id, "; selected=", paste(selected, collapse = ",")))
  if (!quiet) {
    cli::cli_alert_success("SCM {.val {scm_id}} finished: final model {.val {current$run_id}} with {if (length(selected)) paste(selected, collapse = ' + ') else 'no covariates'}")
  }
  out
}

isTRUE_vec <- function(x) {
  !is.na(x) & x
}

#' @export
print.pk_scm <- function(x, ...) {
  cli::cli_text("{.cls pk_scm} {.val {x$scm_id}}: base {.val {x$base}}, final {.val {x$final$run_id}}")
  cli::cli_text("Selected: {if (length(x$selected)) paste(x$selected, collapse = ', ') else 'none'}")
  invisible(x)
}
