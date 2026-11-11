# Generates inst/extdata/example_pk.csv (simulated single-dose oral study).
# Run from the package root: Rscript data-raw/simulate_example.R
pkgload::load_all(".")
d <- pk_simulate_example(n = 40, seed = 2026, dose = 100)
dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
write.csv(d, "inst/extdata/example_pk.csv", row.names = FALSE, na = ".")
cat("Wrote", nrow(d), "records for", length(unique(d$ID)), "subjects\n")
