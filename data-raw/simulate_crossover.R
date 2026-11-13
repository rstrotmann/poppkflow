# Generates inst/extdata/crossover_pk.csv: a simulated oral crossover study for testing.
# Run from the package root: Rscript data-raw/simulate_crossover.R
# Needs only rxode2 (and lotri, which rxode2 depends on); poppkflow is not used.
#
# Design
#   * 20 virtual subjects (10 male, 10 female), every subject receives all four treatments
#     (FOOD 1 or 2 x dose 200 or 500) in four periods (OCC 1 to 4), one dose per period.
#   * The order of the treatments follows a Williams design (4 sequences, 5 subjects each),
#     which balances period and first-order carry-over effects.
#   * Periods start every 168 h (washout of 7 days). TIME is the time since the first dose of
#     the subject. Samples are taken 0.25 to 48 h after each dose.
#   * FOOD is coded 1 (reference) and 2. DOSE is the dose level of the period (same as AMT on
#     the dosing record).
#
# Model (two-compartment, first-order absorption, simulated with rxode2)
#   CL = TVCL * (WT / 70)^0.75 * exp(TH_SEXF * SEXF + TH_FOOD * (FOOD - 1) + eta.cl)
#   V2 = TVV2 * (WT / 70)      * exp(eta.v2)
#   Q  = TVQ  * (WT / 70)^0.75
#   V3 = TVV3 * (WT / 70)
#   KA = TVKA                  * exp(eta.ka)
#   Weight scales the clearance terms (CL, Q) with exponent 0.75 and the volumes (V2, V3) with
#   exponent 1. Sex and food act on clearance only. FOOD changes between the periods of a
#   subject, so it is a time-varying covariate of the model.
#   Residual error: log-normal, 15 % (approximately proportional); concentrations are always
#   positive.
#
# True values
#   TVKA 1.2 1/h, TVCL 8 L/h, TVV2 35 L, TVQ 6 L/h, TVV3 70 L, all at 70 kg, male, FOOD 1
#   TH_SEXF -0.4 (female clearance about 33 % lower), TH_FOOD 0.2 (clearance about 22 % higher
#   with FOOD 2)
#   IIV (variances): eta.cl 0.05, eta.v2 0.09, eta.ka 0.16; no inter-occasion variability
#   (the column OCC allows adding it to the model).
#   Sex is a between-subject covariate, so with 10 subjects per sex its effect is only
#   detectable if it is large compared with the IIV of clearance. The effect size and the IIV
#   of clearance are chosen for that; FOOD is estimated within subjects and is precise.
#   Fitting the true model with FOCEI recovers the effects (SEXF: dOFV about 16 when it is
#   removed, FOOD: dOFV about 380). Give the covariate effects non-zero initial estimates, for
#   example -0.2 for TH_SEXF: from exactly 0 the optimizer leaves TH_SEXF at 0, the sex
#   difference ends up in the etas, and the covariance step returns absurd standard errors.
#   Units: dose in mg, time in h, concentration in mg/L.
suppressPackageStartupMessages(library(rxode2))
stopifnot(file.exists("DESCRIPTION"))

seed <- 2026
n <- 20
washout_start <- 168
sample_times <- c(0.25, 0.5, 1, 1.5, 2, 3, 4, 6, 8, 12, 24, 36, 48)
resid_sd <- 0.15

theta <- c(
  tvka = 1.2, tvcl = 8, tvv2 = 35, tvq = 6, tvv3 = 70,
  th_sexf = -0.4, th_food = 0.2
)
omega <- lotri::lotri({
  eta.cl ~ 0.05
  eta.v2 ~ 0.09
  eta.ka ~ 0.16
})

mod <- rxode2({
  cl <- tvcl * (WT / 70)^0.75 * exp(th_sexf * SEXF + th_food * (FOOD - 1) + eta.cl)
  v2 <- tvv2 * (WT / 70) * exp(eta.v2)
  q <- tvq * (WT / 70)^0.75
  v3 <- tvv3 * (WT / 70)
  ka <- tvka * exp(eta.ka)
  d/dt(depot) <- -ka * depot
  d/dt(central) <- ka * depot - (cl / v2 + q / v2) * central + (q / v3) * periph
  d/dt(periph) <- (q / v2) * central - (q / v3) * periph
  cp <- central / v2
})

# ---- virtual subjects and treatment sequences ------------------------------------------------
set.seed(seed)
subj <- data.frame(
  ID = seq_len(n),
  SEXF = sample(rep(0:1, each = n / 2))
)
subj$WT <- round(stats::rlnorm(n, log(ifelse(subj$SEXF == 1, 66, 82)), 0.14), 1)

# four treatments: FOOD x dose
trt <- data.frame(FOOD = c(1, 2, 1, 2), DOSE = c(200, 200, 500, 500))
# Williams design for four treatments: rows are sequences, columns are periods
williams <- rbind(c(1, 2, 4, 3), c(2, 3, 1, 4), c(3, 4, 2, 1), c(4, 1, 3, 2))
subj$SEQ <- sample(rep(seq_len(nrow(williams)), length.out = n))

# ---- event table: one dose and the samples for every period -----------------------------------
events <- do.call(rbind, lapply(seq_len(n), function(i) {
  do.call(rbind, lapply(1:4, function(p) {
    tr <- trt[williams[subj$SEQ[i], p], ]
    t0 <- (p - 1) * washout_start
    data.frame(
      id = subj$ID[i],
      time = c(t0, t0 + sample_times),
      evid = c(1L, rep(0L, length(sample_times))),
      amt = c(tr$DOSE, rep(0, length(sample_times))),
      cmt = c(1L, rep(2L, length(sample_times))),
      OCC = p, DOSE = tr$DOSE, FOOD = tr$FOOD,
      WT = subj$WT[i], SEXF = subj$SEXF[i]
    )
  }))
}))
events <- events[order(events$id, events$time, -events$evid), ]
rownames(events) <- NULL

# ---- simulate: individual etas are drawn by rxode2 --------------------------------------------
sim <- rxSolve(
  mod, theta, events,
  omega = omega, seed = seed, addDosing = FALSE, returnType = "data.frame"
)

obs <- events$evid == 0
key <- function(id, time) paste(id, format(time, nsmall = 2))
ipred <- sim$cp[match(key(events$id[obs], events$time[obs]), key(sim$id, sim$time))]
stopifnot(!anyNA(ipred), sum(obs) == nrow(sim))

dv <- rep(NA_real_, nrow(events))
dv[obs] <- round(ipred * exp(stats::rnorm(sum(obs), 0, resid_sd)), 4)

d <- data.frame(
  ID = events$id, TIME = events$time, DV = dv, AMT = events$amt, EVID = events$evid,
  CMT = events$cmt, OCC = events$OCC, DOSE = events$DOSE, FOOD = events$FOOD,
  WT = events$WT, SEXF = events$SEXF
)

# ---- checks of the design ---------------------------------------------------------------------
doses <- d[d$EVID == 1, ]
stopifnot(
  nrow(doses) == 4 * n,
  all(tapply(paste(doses$FOOD, doses$DOSE), doses$ID, function(x) length(unique(x))) == 4),
  all(d$DV[d$EVID == 0] > 0)
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
write.csv(d, "inst/extdata/crossover_pk.csv", row.names = FALSE, na = ".")
cat(
  "Wrote", nrow(d), "records for", n, "subjects:", sum(obs), "observations,",
  nrow(doses), "doses\n"
)
