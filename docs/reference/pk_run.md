# Fit a model and register it in the ledger

`pk_run()` is the central function of the workflow. It

## Usage

``` r
pk_run(
  x,
  data = NULL,
  est = NULL,
  note = NA_character_,
  stage = NA_character_,
  id = NULL,
  refit = FALSE,
  cache = c("same_parent", "any"),
  fit_fun = NULL,
  quiet = FALSE,
  proj = NULL
)
```

## Arguments

- x:

  A `pk_spec`, a `pk_candidate` (result of the editing verbs, see
  [pk_edit](pk_edit.md)), a run reference, or a run id. For a run, its
  specification is re-used and the run becomes the parent.

- data:

  Data frame. Default: the registered dataset, see
  [`pk_data()`](pk_data.md). It must contain the required columns (`ID`,
  `TIME`, `DV`, `EVID`, `AMT`); otherwise `pk_run()` stops before a run
  is registered.

- est:

  Estimation settings, see [`pk_est()`](pk_est.md).

- note:

  Free-text description of the run (what changed and why).

- stage:

  Development stage, e.g. `"structural"`, `"stochastic"`, `"covariate"`,
  `"final"`.

- id:

  Optional run id. Default: the next free `runNNN`.

- refit:

  Fit again even if an identical completed run exists?

- cache:

  Which identical completed run may be returned instead of fitting:
  `"same_parent"` (default) only one with the same parent as the
  requested run, `"any"` one with any parent (a `cache_hit` event
  records a different requested parent). Ignored if `refit = TRUE`.

- fit_fun:

  Function `f(model, data, est)` that returns a fit. Default: the
  `nlmixr2` fit with the strategy of [`pk_est()`](pk_est.md). Mainly
  useful for tests.

- quiet:

  Suppress progress messages?

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A run reference (`pk_run_ref`). Use [`pk_result()`](pk_get.md) and
[`pk_get()`](pk_get.md) to access results and fit.

## Details

1.  compiles the specification to `nlmixr2` code,

2.  computes the run key from the specification (generated code), the
    data and the estimation settings, and returns the existing run if
    the same combination has already been completed under the same
    parent (caching, see `cache`; use `refit = TRUE` to force a new
    fit),

3.  registers the run (with parent, stage and note) in the ledger,

4.  fits the model,

5.  stores `fit.rds`, `result.rds`, `spec.rds`, `model.txt`,
    `settings.rds` and `diag.rds` in `models/<run_id>/`, and

6.  updates the ledger with OFV, dOFV against the parent, AIC, condition
    number, covariance step status, shrinkage, and the number of flags.

A run that does not complete is recorded with status `"failed"` and the
error message, and the error is re-thrown. This covers an error in the
fit, an error while the results are extracted and stored, and an
interrupt (the message then says that the run was interrupted). A run is
therefore never left with the status `"running"` unless the R process
itself is killed.

The cache respects the lineage. An identical model that was fitted under
a different parent is a different step in the development history, so
with `cache = "same_parent"` (default) it is fitted again as a new run.
A run that is re-run from itself (`pk_run("run003")` without changes)
returns that run. With `cache = "any"` the earlier run is returned
whatever its parent; the event log of that run then records the
`cache_hit` with the requested parent, so the attempted derivation stays
visible in [`pk_audit()`](pk_runs.md).

## Examples

``` r
# \donttest{
p <- pk_project(file.path(tempdir(), "demo_run"))
d <- pk_data(system.file("extdata", "example_pk.csv", package = "poppkflow"))
r1 <- pk_run(pk_spec(ncmt = 2, iiv = c("cl", "v2")), data = d, est = pk_est(method = "focei"),
             note = "Base model", stage = "structural")
#> → Fitting "run001": Base model
#> Registered S3 method overwritten by 'minqa':
#>   method      from     
#>   print.minqa RcppTrust
#> ✔ "run001": OFV -526.9316, 0 flags
pk_history()
#> # A tibble: 1 × 14
#>   run_id parent_id stage      description status     ofv dofv_parent n_par   aic
#>   <chr>  <chr>     <chr>      <chr>       <chr>    <dbl>       <dbl> <int> <dbl>
#> 1 run001 NA        structural Base model  complet… -527.          NA     8  224.
#> # ℹ 5 more variables: cov_step <lgl>, cond_no <dbl>, n_flags <int>,
#> #   decision <chr>, decision_reason <chr>
# }
```
