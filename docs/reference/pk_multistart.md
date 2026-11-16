# Search for local minima: multiple starting values

Fits the same model from several sets of initial estimates. With oral
data only, absorption and disposition rate constants can be confounded
("flip-flop"), and the likelihood can have local minima. Every start is
a regular run in the ledger (stage `"multistart"`).

## Usage

``` r
pk_multistart(
  x,
  starts,
  data = NULL,
  est = NULL,
  fit_fun = NULL,
  quiet = FALSE,
  proj = NULL
)
```

## Arguments

- x:

  A `pk_spec`, `pk_candidate`, run reference, or run id.

- starts:

  Tibble with one row per start. Columns are initial values named as the
  parameters (`ka`, `v2`, `q`, `v3`, `tlag`, ...); an optional column
  `set` labels the start.

- data, est, fit_fun, quiet, proj:

  See [`pk_run()`](pk_run.md).

## Value

A tibble with one row per start: `set`, `run_id`, `ofv`, the final
estimates of the structural parameters, and `best` (lowest OFV). The
best run is attached as attribute `best_run`.
