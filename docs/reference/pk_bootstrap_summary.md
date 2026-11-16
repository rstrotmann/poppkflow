# Summary of a bootstrap

Median and 95% percentile interval of the bootstrap estimates, and the
bias relative to the final estimate. Log-scale parameters are
back-transformed.

## Usage

``` r
pk_bootstrap_summary(run, proj = NULL)
```

## Arguments

- run:

  A run (reference or id) with a stored bootstrap.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A tibble with `Code`, `Final`, `Bootstrap median`, `2.5th pct`,
`97.5th pct`, `Bias (%)`, `n successful`, `n total`.
