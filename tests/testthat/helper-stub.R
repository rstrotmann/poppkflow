# Helpers for fast tests: a stub fit function that returns an object shaped like the parts of an
# nlmixr2 fit that poppkflow reads. The OFV is a deterministic function of the model code.

local_project <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  proj <- pk_project(file.path(dir, "proj"), name = "test")
  withr::defer(options(poppkflow.project = NULL), envir = env)
  proj
}

example_data <- function() {
  as.data.frame(tibble::tribble(
    ~ID, ~TIME, ~DV , ~AMT, ~EVID, ~WT, ~SEXF,
    1  , 0    , NA  , 100 , 1    , 70 , 0    ,
    1  , 1    , 2.1 , 0   , 0    , 70 , 0    ,
    1  , 4    , 1.2 , 0   , 0    , 70 , 0    ,
    2  , 0    , NA  , 100 , 1    , 90 , 1    ,
    2  , 1    , 1.5 , 0   , 0    , 90 , 1    ,
    2  , 4    , 0.9 , 0   , 0    , 90 , 1
  ))
}

# parse the ini block of generated model code
stub_theta <- function(code) {
  lines <- trimws(strsplit(code, "\n")[[1]])
  th <- numeric()
  for (l in lines) {
    m <- regmatches(l, regexec("^([A-Za-z][A-Za-z0-9_.]*) <- log\\(([^)]+)\\)$", l))[[1]]
    if (length(m)) {
      th[m[2]] <- log(as.numeric(m[3]))
      next
    }
    m <- regmatches(l, regexec("^([A-Za-z][A-Za-z0-9_.]*) <- (fix\\()?(-?[0-9.e-]+)\\)?$", l))[[1]]
    if (length(m)) th[m[2]] <- as.numeric(m[4])
  }
  th
}

stub_omega <- function(code) {
  lines <- trimws(strsplit(code, "\n")[[1]])
  eta_lines <- lines[grepl("^(eta|iov)\\.[A-Za-z0-9. +]* ~", lines)]
  nm <- unique(unlist(regmatches(eta_lines, gregexpr("(eta|iov)\\.[A-Za-z0-9]+", eta_lines))))
  om <- diag(0.1, length(nm))
  dimnames(om) <- list(nm, nm)
  for (l in eta_lines[grepl("\\+", eta_lines)]) {
    nms <- unlist(regmatches(l, gregexpr("(eta|iov)\\.[A-Za-z0-9]+", l)))
    om[nms[2], nms[1]] <- om[nms[1], nms[2]] <- 0.01
  }
  om
}

# effects on the OFV (negative = improvement); `both` is the joint effect of cl_wt and cl_crcl
stub_ofv <- function(code, effects = c(cl_wt = -30, cl_crcl = -27, v2_wt = -3, cl_age = -2), both = -36, ka_iiv = -20) {
  th <- names(stub_theta(code))
  ofv <- 1000
  have <- intersect(names(effects), th)
  ofv <- ofv + sum(effects[have])
  if (all(c("cl_wt", "cl_crcl") %in% have)) ofv <- 1000 + both + sum(effects[setdiff(have, c("cl_wt", "cl_crcl"))])
  if (grepl("eta\\.ka", code)) ofv <- ofv + ka_iiv
  if (grepl("add\\(add.sd\\)", code)) ofv <- ofv - 5
  ofv
}

stub_fit_fun <- function(ofv_fun = stub_ofv, cov_step = TRUE, fail_if = NULL, shrink_eta = 10) {
  function(model, data, est) {
    code <- attr(model, "code")
    if (!is.null(fail_if) && grepl(fail_if, code)) stop("stub failure")
    th <- stub_theta(code)
    om <- stub_omega(code)
    est_v <- th
    bt <- ifelse(grepl("^l(ka|cl|v|v2|q|v3|tlag)$", names(th)), exp(th), th)
    pf <- data.frame(
      Estimate = unname(est_v), SE = 0.05 * abs(unname(est_v)) + 0.01, `%RSE` = 5,
      `Back-transformed` = unname(bt), `CI Lower` = unname(bt) * 0.9, `CI Upper` = unname(bt) * 1.1,
      check.names = FALSE, row.names = names(th)
    )
    ids <- unique(data$ID)
    eta <- data.frame(ID = ids)
    for (nm in rownames(om)) eta[[nm]] <- stats::rnorm(length(ids), 0, 0.3)
    sh <- as.data.frame(matrix(shrink_eta, nrow = 1, ncol = nrow(om) + 1, dimnames = list("sd shrinkage (%)", c(rownames(om), "IWRES"))))
    structure(
      list(
        objf = ofv_fun(code), theta = th, omega = om, parFixedDf = pf, shrink = sh, eta = eta,
        conditionNumberCor = 50, cov = if (cov_step) list(1) else NULL
      ),
      class = "stubfit"
    )
  }
}

new_run <- function(proj, spec, fit_fun = stub_fit_fun(), ...) {
  pk_run(spec, data = example_data(), est = pk_est(method = "focei"), fit_fun = fit_fun, quiet = TRUE, proj = proj, ...)
}
