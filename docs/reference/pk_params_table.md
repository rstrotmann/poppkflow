# Parameter table of a run

Fixed effects (back-transformed from the log scale where applicable),
random effects as variances with CV (%), covariances with correlations,
relative standard errors, 95% confidence intervals and shrinkage.

## Usage

``` r
pk_params_table(run, proj = NULL)
```

## Arguments

- run:

  A run reference or a run id.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A tibble.
