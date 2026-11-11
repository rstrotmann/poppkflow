#' Simulate a small example dataset
#'
#' Simulates a single-dose oral study from a two-compartment model with first-order
#' absorption and lag time. Clearance depends on body weight (allometric,
#' exponent 0.75) and creatinine clearance (exponent 0.3), the central volume on
#' body weight (exponent 1). Residual error is combined proportional and
#' additive. The result is used in the examples, tests and the vignette
#' (`system.file("extdata", "example_pk.csv", package = "poppkflow")`).
#'
#' @param n Number of subjects.
#' @param seed Random seed.
#' @param dose Dose per subject (dose unit; concentrations are in dose
#'   unit per litre).
#' @return A data frame in nlmixr2 format with the columns `ID`, `TIME`, `DV`,
#'   `AMT`, `EVID`, `WT`, `AGE`, `SEXF` and `CRCL`.
#' @examples
#' d <- pk_simulate_example(n = 10)
#' head(d)
#' @export
pk_simulate_example <- function(n = 40, seed = 2026, dose = 100) {
  set.seed(seed)
  subj <- tibble::tibble(
    ID = seq_len(n),
    WT = round(stats::rlnorm(n, log(75), 0.2), 1),
    AGE = round(stats::runif(n, 20, 80)),
    SEXF = stats::rbinom(n, 1, 0.5),
    CRCL = round(pmax(30, stats::rnorm(n, 100, 25)))
  )
  om_cl <- 0.08
  om_v <- 0.12
  rho <- 0.5
  e1 <- stats::rnorm(n)
  e2 <- rho * e1 + sqrt(1 - rho^2) * stats::rnorm(n)
  subj$eta_cl <- sqrt(om_cl) * e1
  subj$eta_v2 <- sqrt(om_v) * e2
  subj$eta_ka <- sqrt(0.15) * stats::rnorm(n)
  subj$ka <- 1.2 * exp(subj$eta_ka)
  subj$cl <- 5 * (subj$WT / 70)^0.75 * (subj$CRCL / 100)^0.3 * exp(subj$eta_cl)
  subj$v2 <- 30 * (subj$WT / 70) * exp(subj$eta_v2)
  subj$q <- 4
  subj$v3 <- 50
  subj$tlag <- 0.25

  mod <- rxode2::rxode2("
    d/dt(depot)   <- -ka * depot
    alag(depot)   <- tlag
    d/dt(central) <- ka * depot - (cl / v2 + q / v2) * central + (q / v3) * periph
    d/dt(periph)  <- (q / v2) * central - (q / v3) * periph
    cp <- central / v2
  ")
  base_times <- c(0.25, 0.5, 1, 2, 3, 4, 6, 8, 12, 24)
  rows <- lapply(seq_len(n), function(i) {
    times <- sort(unique(round(base_times * exp(stats::rnorm(length(base_times), 0, 0.05)), 2)))
    ev <- rxode2::et(amt = dose, cmt = "depot")
    ev <- rxode2::et(ev, times)
    p <- as.data.frame(subj[i, c("ka", "cl", "v2", "q", "v3", "tlag")])
    s <- rxode2::rxSolve(mod, p, ev, returnType = "data.frame")
    s <- s[s$time %in% times, ]
    ipred <- s$cp
    dv <- ipred * (1 + 0.15 * stats::rnorm(length(ipred))) + 0.02 * stats::rnorm(length(ipred))
    dv <- pmax(round(dv, 4), 0.01)
    rbind(
      data.frame(ID = i, TIME = 0, DV = NA_real_, AMT = dose, EVID = 1L),
      data.frame(ID = i, TIME = s$time, DV = dv, AMT = 0, EVID = 0L)
    )
  })
  d <- do.call(rbind, rows)
  d <- merge(d, as.data.frame(subj[, c("ID", "WT", "AGE", "SEXF", "CRCL")]), by = "ID", sort = FALSE)
  d <- d[order(d$ID, d$TIME, -d$EVID), ]
  rownames(d) <- NULL
  d[, c("ID", "TIME", "DV", "AMT", "EVID", "WT", "AGE", "SEXF", "CRCL")]
}
