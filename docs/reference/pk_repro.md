# Reproducibility of the OFV

Re-estimates a model with different SAEM seeds and/or from randomly
perturbed initial estimates (with FOCEI only). The OFV of the same
population model should be reproducible within a few points; larger
differences point to local minima or multimodal individual likelihoods.
The runs are regular ledger runs (stage `"repro"`) with the original run
as parent.

## Usage

``` r
pk_repro(
  run,
  seeds = c(7, 99),
  perturb_starts = 0,
  perturb_sd = 0.3,
  seed = 1,
  data = NULL,
  fit_fun = NULL,
  quiet = FALSE,
  proj = NULL
)
```

## Arguments

- run:

  A completed run (reference or id).

- seeds:

  SAEM seeds for repeated SAEM-based estimations (the seed of the
  original run is skipped).

- perturb_starts:

  Number of FOCEI fits from perturbed initial estimates.

- perturb_sd:

  Standard deviation of the log-normal perturbation of the structural
  parameters.

- seed:

  Seed for the perturbation.

- data, fit_fun, quiet, proj:

  See [`pk_run()`](pk_run.md).

## Value

A tibble with the columns `label`, `method`, `run_id`, `ofv` and the
estimates; the attribute `ofv_range` holds the range of the OFV across
the original and the repeated runs.
