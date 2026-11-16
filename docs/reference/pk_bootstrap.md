# Non-parametric bootstrap

Resamples subjects with replacement (optionally stratified, for example
by sampling design), re-estimates the model from the final estimates of
the run and stores the estimates of every replicate in
`models/<run>/eval/bootstrap.rds`. Replicates are saved after each fit
(or after each batch if `parallel = TRUE`), and a repeated call
continues from the stored replicates (`resume = TRUE`).

## Usage

``` r
pk_bootstrap(
  run,
  n = 200,
  strata = NULL,
  data = NULL,
  est = NULL,
  seed = 1,
  parallel = FALSE,
  resume = TRUE,
  fit_fun = NULL,
  quiet = FALSE,
  proj = NULL
)
```

## Arguments

- run:

  A completed run (reference or id).

- n:

  Total number of replicates wanted.

- strata:

  Name of a subject-level column to stratify the resampling by.

- data, fit_fun, quiet, proj:

  See [`pk_run()`](pk_run.md).

- est:

  Estimation settings for the replicates. Default: SAEM with 250 burn-in
  and 250 EM iterations.

- seed:

  Base seed; replicate `b` uses `seed + b`.

- parallel:

  Use
  [`future.apply::future_lapply()`](https://future.apply.futureverse.org/reference/future_lapply.html)?

- resume:

  Continue from stored replicates?

## Value

A tibble with one row per replicate (invisibly).

## Details

By default the replicates are estimated with SAEM, which is robust to
the starting values and does not need an OFV. For parallel execution
install `future.apply` and set a plan, for example
`future::plan("multisession")`. Only the main process writes to the
project; the workers return results.
