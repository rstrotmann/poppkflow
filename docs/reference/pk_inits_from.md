# Use the final estimates of a run as initial estimates

Replaces the initial estimates of the specification (structural
parameters, IIV variances, block covariances, residual error, covariate
parameters) with the estimates of a completed run. Typical use: start
the next model from the final estimates of its parent.

## Usage

``` r
pk_inits_from(x, run = NULL, digits = 4, proj = NULL)
```

## Arguments

- x:

  A `pk_spec`, `pk_candidate`, `pk_run_ref`, or a run id; the
  specification to update.

- run:

  The run (reference or id) providing the estimates. Defaults to the
  parent of `x` if `x` is a run or candidate.

- digits:

  Number of significant digits.

- proj:

  A `pk_project`.

## Value

A `pk_candidate`.
