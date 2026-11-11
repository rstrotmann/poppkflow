#' Compile a specification into an nlmixr2 model
#'
#' Generates the code of an `nlmixr2` model function (with `linCmt()`) from a
#' [pk_spec()]. The code text is available as `attr(model, "code")` and is what
#' is stored with every run (`model.txt`) and hashed for caching.
#'
#' @param spec A `pk_spec`, `pk_candidate`, or run reference.
#' @return A model function with attribute `code`.
#' @examples
#' spec <- pk_spec(ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"), blocks = list(c("cl", "v2")))
#' cat(pk_code(spec))
#' @export
pk_compile <- function(spec) {
  if (inherits(spec, "pk_candidate")) spec <- spec$spec
  if (!inherits(spec, "pk_spec")) spec <- as_candidate(spec)$spec
  if (!is.null(spec$custom_fn)) {
    f <- spec$custom_fn
    attr(f, "code") <- paste(deparse(f), collapse = "\n")
    return(f)
  }
  txt <- compile_code(spec)
  f <- eval(parse(text = txt), envir = new.env(parent = baseenv()))
  attr(f, "code") <- txt
  f
}

#' @rdname pk_compile
#' @export
pk_code <- function(spec) {
  attr(pk_compile(spec), "code")
}

compile_code <- function(spec) {
  params <- pk_params(spec)
  inits <- spec$inits
  cv <- spec$covariates
  eta <- function(p) if (p %in% spec$iiv) paste0(" + eta.", p) else ""
  iov_term <- function(p) {
    if (!is.null(spec$iov) && identical(spec$iov$param, p)) {
      lv <- spec$iov$levels
      paste0(sprintf(" + iov.%s%d * (%s == %s)", p, seq_along(lv), spec$iov$occasion, lv), collapse = "")
    } else {
      ""
    }
  }
  cov_terms <- function(p) {
    x <- cv$term[cv$param == p]
    if (length(x)) paste0(" + ", paste(x, collapse = " + ")) else ""
  }
  num <- function(x) format(x, digits = 10, trim = TRUE, scientific = FALSE)

  # --- ini block
  ini <- vapply(params, function(p) sprintf("l%s <- log(%s)", p, num(inits[[p]])), character(1))
  if (nrow(cv)) {
    ini <- c(ini, vapply(seq_len(nrow(cv)), function(i) {
      if (isTRUE(cv$fix[i])) {
        sprintf("%s <- fix(%s)", cv$theta[i], num(cv$init[i]))
      } else {
        sprintf("%s <- %s", cv$theta[i], num(cv$init[i]))
      }
    }, character(1)))
  }
  for (nm in names(spec$extra_thetas)) {
    ini <- c(ini, sprintf("%s <- %s", nm, num(spec$extra_thetas[[nm]])))
  }
  pick <- function(v, nm) if (nm %in% names(v)) v[[nm]] else NULL
  om_get <- function(p) pick(inits$om, p) %||% 0.1
  cov_get <- function(a, b) {
    v <- pick(inits$om_cov, paste0(a, ".", b)) %||% pick(inits$om_cov, paste0(b, ".", a))
    if (is.null(v)) 0.1 * sqrt(om_get(a) * om_get(b)) else v
  }
  in_block <- unlist(spec$blocks)
  for (b in spec$blocks) {
    vals <- c()
    for (i in seq_along(b)) {
      for (j in seq_len(i)) {
        vals <- c(vals, if (i == j) om_get(b[i]) else cov_get(b[j], b[i]))
      }
    }
    ini <- c(ini, sprintf("%s ~ c(%s)", paste0("eta.", b, collapse = " + "), paste(num(vals), collapse = ", ")))
  }
  for (p in setdiff(spec$iiv, in_block)) {
    ini <- c(ini, sprintf("eta.%s ~ %s", p, num(om_get(p))))
  }
  if (!is.null(spec$iov)) {
    for (k in seq_along(spec$iov$levels)) {
      ini <- c(ini, sprintf("iov.%s%d ~ %s", spec$iov$param, k, num(spec$iov$init %||% 0.01)))
    }
  }
  if (spec$error %in% c("prop", "comb")) ini <- c(ini, sprintf("prop.sd <- %s", num(inits$prop.sd)))
  if (spec$error %in% c("add", "comb")) ini <- c(ini, sprintf("add.sd <- %s", num(inits$add.sd)))

  # --- model block
  mod <- character()
  for (p in setdiff(params, "tlag")) {
    mod <- c(mod, sprintf("%s <- exp(l%s%s%s%s)", p, p, cov_terms(p), eta(p), iov_term(p)))
  }
  if (spec$lag) {
    mod <- c(mod, sprintf("alag(depot) <- exp(ltlag%s%s%s)", cov_terms("tlag"), eta("tlag"), iov_term("tlag")))
  }
  mod <- c(mod, switch(spec$error,
    prop = "linCmt() ~ prop(prop.sd)",
    add  = "linCmt() ~ add(add.sd)",
    comb = "linCmt() ~ prop(prop.sd) + add(add.sd)"
  ))
  paste0(
    "function() {\n  ini({\n    ", paste(ini, collapse = "\n    "),
    "\n  })\n  model({\n    ", paste(mod, collapse = "\n    "), "\n  })\n}"
  )
}

# Hash of a specification based on its generated code
spec_hash <- function(spec) {
  pk_hash(pk_code(spec))
}
