# poppkflow

Structured, iterative population PK model development with
[nlmixr2](https://nlmixr2.org). Model development is a loop: specify a model,
fit it, diagnose it, compare it with its parent, decide, and modify.
`poppkflow` supports this loop and records it.

* **Declarative model specifications** (`pk_spec()`, `pk_add_iiv()`,
  `pk_add_cov()`, ...) instead of hand-edited model code.
* **A run ledger**: `ledger/runs.csv` (one row per run: lineage, hashes, OFV,
  dOFV against the parent, flags, decision) plus `ledger/events.csv` (append-only
  audit trail) and RDS files per run (`models/<run>/fit.rds`, `result.rds`,
  `spec.rds`, ...). Identical runs come from a cache.
* **Standard diagnostics, flags and suggestions** for every run.
* **Comparison and decisions with rationale** (`pk_compare()`, `pk_decide()`).
* **Covariate screening and stepwise covariate modelling**, robustness checks
  (multiple starts, seeds), bootstrap, VPC, exposure.
* **A report generated from the ledger** (`pk_report()`).

## Example

```r
library(poppkflow)

proj <- pk_project("my_analysis")
pk_covariates(tibble::tribble(
  ~name , ~type         , ~ref,
  "WT"  , "continuous"  , 70  ,
  "CRCL", "continuous"  , 100 ,
  "SEXF", "categorical" , NA
))
dat <- pk_data(system.file("extdata", "example_pk.csv", package = "poppkflow"))
pk_check(dat)

# base model
r1 <- pk_run(pk_spec(ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2")),
             note = "Base model", stage = "structural")
pk_diagnostics(r1)

# the data suggest a change: modify the parent and fit the candidate
r2 <- r1 |>
  pk_add_cov("cl", "WT", form = "power", init = 0.75) |>
  pk_inits_from(r1) |>
  pk_run(note = "WT on CL", stage = "covariate")

pk_compare(r1, r2)
pk_decide(r2, "accepted", reason = "dOFV < -6.63 for 1 df; plausible allometric effect")

pk_tree()
pk_history()
```

## Development

```r
devtools::load_all()
devtools::test()                                 # fast tests with a stub fit function
Sys.setenv(POPPKFLOW_SLOW = "true"); devtools::test()   # also real nlmixr2 fits
```
