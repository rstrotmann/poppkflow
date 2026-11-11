# Regression check: reproduce key results of the RS2023 analysis (popPK/) with poppkflow.
#
# Run from the package folder (poppkflow/):  Rscript dev/regression_RS2023.R
#
# The existing analysis in ../popPK is only READ (data and the saved fits run100.rds and
# final_model.rds). The new project is created in poppkflow/dev/regression_project/.
#
# Pass criteria (see section 9 of the plan):
#   * OFV of the base model re-estimated with poppkflow within 5 points of run100
#     (range of the FOCEI OFV across three SAEM seeds in the RS2023 analysis, T09b: 3.2 points)
#   * OFV of the final covariate model within 5 points of final_model
#   * the same covariate relationships as final_model_spec.rds
#   * fixed-effect estimates within 10 % of the legacy final model
suppressMessages(pkgload::load_all(".", quiet = TRUE))

legacy <- normalizePath("../popPK")
stopifnot(dir.exists(legacy))
proj_dir <- "dev/regression_project"
unlink(proj_dir, recursive = TRUE)
proj <- pk_project(proj_dir, name = "RS2023 regression")

pk_covariates(tibble::tribble(
  ~name , ~type         , ~unit    , ~ref,
  "WT"  , "continuous"  , "kg"     , 80  ,
  "AGE" , "continuous"  , "y"      , 65  ,
  "CRCL", "continuous"  , "mL/min" , 80  ,
  "SEXF", "categorical" , NA       , NA
))
dat <- pk_data(file.path(legacy, "data", "rs2023_popPK_analysis_dataset.csv"))
cat("Data checks:\n")
print(pk_check(dat))

# ---- legacy results (read-only) ----------------------------------------------------------------
legacy_base <- readRDS(file.path(legacy, "models", "run100.rds"))
legacy_final <- readRDS(file.path(legacy, "models", "final_model.rds"))
legacy_spec <- readRDS(file.path(legacy, "models", "final_model_spec.rds"))
bt <- function(fit) {
  x <- fit$parFixedDf$`Back-transformed`
  names(x) <- rownames(fit$parFixedDf)
  x
}

# ---- 1. base model: 2-CMT, lag, IIV on KA/CL/V2 with CL-V2 covariance, proportional error ---------
b <- bt(legacy_base)
om <- legacy_base$omega
base_spec <- pk_spec(
  ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"), blocks = list(c("cl", "v2")), error = "prop",
  inits = list(
    ka = round(b[["lka"]], 3), cl = round(b[["lcl"]], 1), v2 = round(b[["lv2"]], 1), q = round(b[["lq"]], 2),
    v3 = round(b[["lv3"]], 1), tlag = round(b[["ltlag"]], 3), prop.sd = round(b[["prop.sd"]], 4),
    om = c(ka = round(om["eta.ka", "eta.ka"], 4), cl = round(om["eta.cl", "eta.cl"], 4), v2 = round(om["eta.v2", "eta.v2"], 4)),
    om_cov = c(cl.v2 = round(om["eta.cl", "eta.v2"], 4))
  )
)
est <- pk_est(method = "saem_focei", seed = 2026, nBurn = 300, nEm = 300)
r_base <- pk_run(base_spec, data = dat, est = est, note = "Base model (regression of run100)", stage = "stochastic")
ofv_diff_base <- pk_result(r_base)$ofv - legacy_base$objf
cat(sprintf("\nBase: poppkflow OFV %.2f, legacy run100 OFV %.2f, difference %.2f\n", pk_result(r_base)$ofv, legacy_base$objf, ofv_diff_base))

# ---- 2. final covariate model ----------------------------------------------------------------------
sel <- legacy_spec$selected
cat("Legacy final model covariates:", paste(sel, collapse = " + "), "\n")
cand <- r_base
for (cid in sel) {
  r <- legacy_spec$cands[legacy_spec$cands$id == cid, ]
  form <- if (grepl("log\\(", r$term)) "power" else if (grepl("SEXF", r$term)) "cat" else "linear"
  covariate <- sub("^[A-Za-z0-9]+~", "", cid)
  covariate <- switch(covariate, SEX = "SEXF", covariate)
  cand <- pk_add_cov(cand, r$param, covariate, form = form, init = r$init)
}
# start from the final estimates of the legacy final model
th_leg <- legacy_final$theta
cand$spec$inits[c("ka", "cl", "v2", "q", "v3", "tlag")] <- lapply(
  c("lka", "lcl", "lv2", "lq", "lv3", "ltlag"), function(n) signif(exp(th_leg[[n]]), 4)
)
cand$spec$inits$prop.sd <- signif(th_leg[["prop.sd"]], 4)
for (i in seq_len(nrow(cand$spec$covariates))) {
  cand$spec$covariates$init[i] <- signif(th_leg[[cand$spec$covariates$theta[i]]], 4)
}
r_final <- pk_run(cand, data = dat, est = est, note = "Final covariate model (regression of final_model)", stage = "covariate")

# ---- 3. comparison with the legacy final model ---------------------------------------------------------
fin_new <- pk_result(r_final)
ofv_diff_final <- fin_new$ofv - legacy_final$objf
cat(sprintf("Final: poppkflow OFV %.2f, legacy OFV %.2f, difference %.2f\n", fin_new$ofv, legacy_final$objf, ofv_diff_final))
spec_new <- readRDS(file.path(proj$path, "models", r_final$run_id, "spec.rds"))
same_cov <- setequal(spec_new$covariates$theta, intersect(names(th_leg), legacy_spec$cands$theta[legacy_spec$cands$id %in% sel]))

pt <- pk_params_table(r_final)
fx <- pt[pt$Type == "Fixed effect", ]
bt_leg <- bt(legacy_final)
rel <- vapply(fx$Code, function(cd) {
  leg <- if (cd %in% names(th_leg) && !grepl("^l", cd)) th_leg[[cd]] else bt_leg[[cd]]
  (fx$Estimate[fx$Code == cd] - leg) / abs(leg)
}, numeric(1))
cat("\nRelative difference of fixed effects vs legacy final model (%):\n")
print(round(100 * rel, 1))

res <- c(
  base_ofv_within_5 = abs(ofv_diff_base) <= 5,
  final_ofv_within_5 = abs(ofv_diff_final) <= 5,
  same_covariates = same_cov,
  fixed_effects_within_10pct = all(abs(rel) <= 0.10, na.rm = TRUE)
)
cat("\nRegression checks:\n")
print(res)
cat("\nDevelopment history:\n")
print(pk_history())
cat("\nLineage:\n")
pk_tree()
saveRDS(list(checks = res, ofv_diff_base = ofv_diff_base, ofv_diff_final = ofv_diff_final, rel = rel), file.path(proj_dir, "regression_result.rds"))
if (!all(res)) quit(status = 1)
