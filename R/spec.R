#' Declarative model specification
#'
#' A model is described as data, not as code: structure (number of
#' compartments, route, lag time), inter-individual variability (IIV), omega
#' blocks, inter-occasion variability (IOV), residual error model, covariate
#' relationships, and initial estimates. [pk_compile()] turns the specification
#' into an `nlmixr2` model function.
#'
#' Parameters are named `ka`, `cl`, `v` (one compartment) or `v2`, `q`, `v3` (two
#' compartments), and `tlag`. All structural parameters are log-normal:
#' `P = exp(lP + covariate terms + eta.P)`.
#'
#' @param ncmt Number of compartments (1 or 2).
#' @param route `"oral"` (first-order absorption) or `"iv"`.
#' @param lag Include an absorption lag time (oral only)?
#' @param iiv Parameters with IIV. Default: clearance and the central volume.
#' @param blocks List of character vectors; each defines an omega block, e.g.
#'   `list(c("cl", "v2"))`.
#' @param iov `NULL` or a list with `param`, `occasion` (column name) and
#'   `levels`, see [pk_add_iov()].
#' @param error Residual error model: `"prop"`, `"add"` or `"comb"`.
#' @param covariates Covariate relationships (a tibble, see [pk_add_cov()]).
#' @param extra_thetas Named list of additional fixed-effect parameters (name =
#'   initial value) that are written to the `ini` block.
#' @param inits Named list with initial estimates (natural scale):
#'   structural parameters, `prop.sd`, `add.sd`, `om` (named vector of IIV
#'   variances) and `om_cov` (named vector of block covariances, e.g.
#'   `c(cl.v2 = 0.01)`). Missing entries get defaults.
#' @param custom_fn Escape hatch: a ready-made `nlmixr2` model function. When
#'   given, it is used as is and the other arguments are ignored (except for
#'   bookkeeping), so the editing verbs such as [pk_add_iiv()] do not apply.
#'
#' @return A `pk_spec` object.
#' @examples
#' spec <- pk_spec(ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"), error = "prop")
#' spec
#' @export
pk_spec <- function(ncmt = 2, route = c("oral", "iv"), lag = FALSE, iiv = NULL,
                    blocks = list(), iov = NULL, error = c("prop", "add", "comb"),
                    covariates = NULL, extra_thetas = list(), inits = list(),
                    custom_fn = NULL) {
  route <- match.arg(route)
  error <- match.arg(error)
  if (!ncmt %in% c(1, 2)) {
    cli::cli_abort("Only 1- and 2-compartment models are supported (got {.val {ncmt}}).")
  }
  if (route == "iv") lag <- FALSE
  if (is.null(iiv)) {
    iiv <- c("cl", if (ncmt == 1) "v" else "v2")
  }
  spec <- structure(
    list(
      ncmt = as.integer(ncmt), route = route, lag = isTRUE(lag), iiv = unique(iiv),
      blocks = blocks, iov = iov, error = error,
      covariates = covariates %||% empty_cov_tbl(),
      extra_thetas = extra_thetas, inits = list(), custom_fn = custom_fn
    ),
    class = "pk_spec"
  )
  spec$inits <- complete_inits(spec, inits)
  validate_spec(spec)
}

empty_cov_tbl <- function() {
  tibble::tibble(
    id = character(), param = character(), covariate = character(), form = character(),
    ref = numeric(), theta = character(), init = numeric(), fix = logical(), term = character()
  )
}

#' Structural parameter names of a specification
#' @param spec A `pk_spec`.
#' @return Character vector of parameter names.
#' @examples
#' pk_params(pk_spec(ncmt = 1, lag = TRUE))
#' @export
pk_params <- function(spec) {
  c(
    if (spec$route == "oral") "ka",
    "cl",
    if (spec$ncmt == 1) "v" else c("v2", "q", "v3"),
    if (spec$lag) "tlag"
  )
}

default_inits <- function() {
  list(
    ka = 1, cl = 10, v = 50, v2 = 30, q = 5, v3 = 40, tlag = 0.3,
    prop.sd = 0.2, add.sd = 0.1,
    om = c(ka = 0.1, cl = 0.1, v = 0.1, v2 = 0.1, q = 0.1, v3 = 0.1, tlag = 0.05),
    om_cov = numeric()
  )
}

complete_inits <- function(spec, inits) {
  d <- default_inits()
  out <- d
  for (nm in setdiff(names(inits), c("om", "om_cov"))) out[[nm]] <- inits[[nm]]
  out$om <- d$om
  out$om[names(inits$om)] <- inits$om
  out$om_cov <- inits$om_cov %||% numeric()
  out
}

validate_spec <- function(spec) {
  if (!is.null(spec$custom_fn)) {
    return(spec)
  }
  params <- pk_params(spec)
  bad <- setdiff(spec$iiv, params)
  if (length(bad)) {
    cli::cli_abort("IIV requested for parameter(s) not in the model: {.val {bad}}. Available: {.val {params}}.")
  }
  for (b in spec$blocks) {
    bad <- setdiff(b, spec$iiv)
    if (length(bad)) {
      cli::cli_abort("Block parameter(s) without IIV: {.val {bad}}. Add IIV first.")
    }
    if (length(b) < 2L) cli::cli_abort("A block needs at least two parameters.")
  }
  bl <- unlist(spec$blocks)
  if (anyDuplicated(bl)) {
    cli::cli_abort("A parameter can only be in one omega block.")
  }
  if (nrow(spec$covariates)) {
    bad <- setdiff(spec$covariates$param, params)
    if (length(bad)) {
      cli::cli_abort("Covariate on parameter(s) not in the model: {.val {bad}}.")
    }
    if (anyDuplicated(spec$covariates$id)) {
      cli::cli_abort("Duplicated covariate relationship id(s).")
    }
  }
  if (!is.null(spec$iov)) {
    if (!spec$iov$param %in% params) {
      cli::cli_abort("IOV on parameter not in the model: {.val {spec$iov$param}}.")
    }
  }
  spec
}

#' @export
print.pk_spec <- function(x, ...) {
  if (!is.null(x$custom_fn)) {
    cli::cli_text("{.cls pk_spec} custom model function")
    return(invisible(x))
  }
  cli::cli_text("{.cls pk_spec}: {x$ncmt}-compartment, {x$route}{if (x$lag) ', lag time' else ''}")
  cli::cli_text("IIV: {.val {x$iiv}}; blocks: {length(x$blocks)}; IOV: {if (is.null(x$iov)) 'none' else x$iov$param}")
  cli::cli_text("Residual error: {.val {x$error}}")
  cli::cli_text("Covariates: {if (nrow(x$covariates)) paste(x$covariates$id, collapse = ', ') else 'none'}")
  invisible(x)
}

# ---- candidates ---------------------------------------------------------------------------

# A candidate is a specification together with the id of the run it was derived from.
new_candidate <- function(spec, parent_id = NA_character_) {
  structure(list(spec = spec, parent_id = parent_id), class = "pk_candidate")
}

#' @export
print.pk_candidate <- function(x, ...) {
  cli::cli_text("{.cls pk_candidate} derived from {.val {x$parent_id}}")
  print(x$spec)
  invisible(x)
}

as_candidate <- function(x, proj = NULL) {
  if (inherits(x, "pk_candidate")) {
    return(x)
  }
  if (inherits(x, "pk_spec")) {
    return(new_candidate(x))
  }
  if (inherits(x, "pk_run_ref")) {
    return(new_candidate(read_run_file(x$proj_path, x$run_id, "spec.rds"), x$run_id))
  }
  if (is_string(x)) {
    proj <- resolve_proj(proj)
    return(new_candidate(read_run_file(proj$path, x, "spec.rds"), x))
  }
  cli::cli_abort("Cannot use an object of class {.cls {class(x)[1]}} as a model specification.")
}

modify_spec <- function(x, f, proj = NULL) {
  cand <- as_candidate(x, proj)
  if (!is.null(cand$spec$custom_fn)) {
    cli::cli_abort("Specification verbs do not apply to a custom model function.")
  }
  cand$spec <- validate_spec(f(cand$spec))
  cand
}

# ---- editing verbs --------------------------------------------------------------------------

#' Edit a model specification
#'
#' Each verb takes a specification, a candidate, a run, or a run id as first
#' argument and returns a *candidate*: the edited specification together with
#' the parent run. A candidate is fitted with [pk_run()], which records the
#' lineage in the ledger.
#'
#' @param x A `pk_spec`, `pk_candidate`, `pk_run_ref`, or a run id.
#' @param ... Parameter names (character), or named initial values for
#'   `pk_set_inits()`.
#' @param ncmt,route,lag Model structure, see [pk_spec()].
#' @param params Character vector of parameters for an omega block.
#' @param param Parameter name.
#' @param occasion Name of the occasion column (IOV).
#' @param levels Occasion levels (IOV).
#' @param init Initial value.
#' @param error Residual error model.
#' @param covariate Covariate (column) name.
#' @param form Functional form: `"power"` (`theta * log(COV / ref)`), `"linear"`
#'   (`theta * (COV - ref)`), `"cat"` (`theta * COV`, for 0/1 coded
#'   covariates), or `"custom"` (give `term` and `theta`).
#' @param ref Reference value for centering (`power` and `linear`). If `NULL`,
#'   the value from the covariate dictionary is used, see [pk_covariates()].
#' @param fix Fix the covariate parameter at `init`?
#' @param term,theta,id Custom term (e.g. `"ss_cl * (OCC - 1)"`), parameter name,
#'   and relationship id for `form = "custom"`.
#' @param proj A `pk_project`.
#'
#' @return A `pk_candidate`.
#' @name pk_edit
#' @examples
#' base <- pk_spec(ncmt = 2, iiv = c("cl", "v2"))
#' base |>
#'   pk_add_iiv("ka") |>
#'   pk_set_block(c("cl", "v2")) |>
#'   pk_set_error("comb") |>
#'   pk_add_cov("cl", "WT", form = "power", ref = 70, init = 0.75)
NULL

#' @rdname pk_edit
#' @export
pk_set_structure <- function(x, ncmt = NULL, route = NULL, lag = NULL, proj = NULL) {
  modify_spec(x, function(s) {
    old <- pk_params(s)
    if (!is.null(ncmt)) s$ncmt <- as.integer(ncmt)
    if (!is.null(route)) s$route <- match.arg(route, c("oral", "iv"))
    if (!is.null(lag)) s$lag <- isTRUE(lag)
    if (s$route == "iv") s$lag <- FALSE
    keep <- pk_params(s)
    s$iiv <- intersect(s$iiv, keep)
    s$blocks <- Filter(function(b) all(b %in% s$iiv), s$blocks)
    s$covariates <- s$covariates[s$covariates$param %in% keep | s$covariates$form == "custom", ]
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_add_iiv <- function(x, ..., proj = NULL) {
  p <- c(...)
  modify_spec(x, function(s) {
    s$iiv <- unique(c(s$iiv, p))
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_drop_iiv <- function(x, ..., proj = NULL) {
  p <- c(...)
  modify_spec(x, function(s) {
    s$iiv <- setdiff(s$iiv, p)
    s$blocks <- Filter(function(b) !any(b %in% p), s$blocks)
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_set_block <- function(x, params, proj = NULL) {
  modify_spec(x, function(s) {
    s$iiv <- unique(c(s$iiv, params))
    s$blocks <- c(Filter(function(b) !any(b %in% params), s$blocks), list(params))
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_drop_block <- function(x, params, proj = NULL) {
  modify_spec(x, function(s) {
    s$blocks <- Filter(function(b) !all(params %in% b), s$blocks)
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_add_iov <- function(x, param, occasion = "OCC", levels = c(1, 2), init = 0.01, proj = NULL) {
  modify_spec(x, function(s) {
    s$iov <- list(param = param, occasion = occasion, levels = levels, init = init)
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_set_error <- function(x, error = c("prop", "add", "comb"), proj = NULL) {
  error <- match.arg(error)
  modify_spec(x, function(s) {
    s$error <- error
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_add_cov <- function(x, param, covariate = NA_character_, form = c("power", "linear", "cat", "custom"),
                       ref = NULL, init = 0, fix = FALSE, term = NULL, theta = NULL, id = NULL,
                       proj = NULL) {
  form <- match.arg(form)
  modify_spec(x, function(s) {
    if (form == "custom") {
      if (is.null(term) || is.null(theta)) {
        cli::cli_abort("{.code form = \"custom\"} needs {.arg term} and {.arg theta}.")
      }
      id_new <- id %||% paste0(toupper(param), "~", toupper(theta))
      row <- tibble::tibble(
        id = id_new, param = param, covariate = covariate, form = "custom", ref = NA_real_,
        theta = theta, init = init, fix = fix, term = term
      )
    } else {
      if (is.null(ref) && form %in% c("power", "linear")) {
        ref <- tryCatch(
          {
            d <- cov_dict_to_tibble(resolve_proj(proj)$config$covariates)
            d$ref[d$name == covariate][1]
          },
          error = function(e) NA_real_
        )
      }
      if (form %in% c("power", "linear") && (is.null(ref) || is.na(ref))) {
        cli::cli_abort("A reference value is needed for {.val {covariate}}: give {.arg ref} or set it in {.fn pk_covariates}.")
      }
      th <- theta %||% paste0(tolower(param), "_", tolower(covariate))
      tm <- switch(form,
        power  = sprintf("%s * log(%s/%s)", th, covariate, format(ref, trim = TRUE)),
        linear = sprintf("%s * (%s - %s)", th, covariate, format(ref, trim = TRUE)),
        cat    = sprintf("%s * %s", th, covariate)
      )
      row <- tibble::tibble(
        id = id %||% paste0(toupper(param), "~", toupper(covariate)), param = param,
        covariate = covariate, form = form, ref = if (form == "cat") NA_real_ else as.numeric(ref),
        theta = th, init = init, fix = fix, term = tm
      )
    }
    s$covariates <- dplyr::bind_rows(s$covariates[s$covariates$id != row$id, ], row)
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_drop_cov <- function(x, id, proj = NULL) {
  modify_spec(x, function(s) {
    s$covariates <- s$covariates[!s$covariates$id %in% id, ]
    s
  }, proj)
}

#' @rdname pk_edit
#' @export
pk_set_inits <- function(x, ..., proj = NULL) {
  new <- list(...)
  modify_spec(x, function(s) {
    for (nm in setdiff(names(new), c("om", "om_cov"))) s$inits[[nm]] <- new[[nm]]
    if (!is.null(new$om)) s$inits$om[names(new$om)] <- new$om
    if (!is.null(new$om_cov)) s$inits$om_cov[names(new$om_cov)] <- new$om_cov
    cv <- s$covariates
    for (i in seq_len(nrow(cv))) {
      if (!is.null(new[[cv$theta[i]]])) s$covariates$init[i] <- new[[cv$theta[i]]]
    }
    s
  }, proj)
}

#' Use the final estimates of a run as initial estimates
#'
#' Replaces the initial estimates of the specification (structural parameters,
#' IIV variances, block covariances, residual error, covariate parameters) with
#' the estimates of a completed run. Typical use: start the next model from the
#' final estimates of its parent.
#'
#' @param x A `pk_spec`, `pk_candidate`, `pk_run_ref`, or a run id; the
#'   specification to update.
#' @param run The run (reference or id) providing the estimates. Defaults to the
#'   parent of `x` if `x` is a run or candidate.
#' @param digits Number of significant digits.
#' @inheritParams pk_edit
#' @return A `pk_candidate`.
#' @export
pk_inits_from <- function(x, run = NULL, digits = 4, proj = NULL) {
  cand <- as_candidate(x, proj)
  src <- run %||% cand$parent_id
  if (is.null(src) || (is.character(src) && is.na(src))) {
    cli::cli_abort("Give {.arg run}: there is no parent run to take estimates from.")
  }
  res <- pk_result(src, proj = proj)
  th <- res$theta
  om <- res$omega
  modify_spec(cand, function(s) {
    for (p in pk_params(s)) {
      nm <- paste0("l", p)
      if (nm %in% names(th)) s$inits[[p]] <- signif(exp(th[[nm]]), digits)
    }
    for (nm in c("prop.sd", "add.sd")) {
      if (nm %in% names(th)) s$inits[[nm]] <- signif(th[[nm]], digits)
    }
    if (!is.null(om)) {
      etas <- sub("^eta\\.", "", rownames(om))
      for (p in intersect(etas, s$iiv)) {
        s$inits$om[[p]] <- signif(om[paste0("eta.", p), paste0("eta.", p)], digits)
      }
      for (b in s$blocks) {
        for (i in seq_along(b)[-1]) {
          for (j in seq_len(i - 1L)) {
            a <- paste0("eta.", b[i])
            c2 <- paste0("eta.", b[j])
            if (all(c(a, c2) %in% rownames(om))) {
              s$inits$om_cov[[paste0(b[j], ".", b[i])]] <- signif(om[a, c2], digits)
            }
          }
        }
      }
    }
    cv <- s$covariates
    for (i in seq_len(nrow(cv))) {
      if (cv$theta[i] %in% names(th) && !isTRUE(cv$fix[i])) s$covariates$init[i] <- signif(th[[cv$theta[i]]], digits)
    }
    for (nm in names(s$extra_thetas)) {
      if (nm %in% names(th)) s$extra_thetas[[nm]] <- signif(th[[nm]], digits)
    }
    s
  }, proj)
}
