# Standard diagnostics bundle for a run

Computes the same set of numerical diagnostics for every run: OFV, AIC,
BIC, number of parameters, condition number, covariance step status,
maximum ETA shrinkage, epsilon shrinkage, CWRES summary, and the
rule-based flags of [`pk_flags()`](pk_flags.md). The bundle is stored as
`diag.rds` in the run folder. Optionally the standard plots are created
([`pk_plot_gof()`](pk_plots.md), [`pk_plot_eta()`](pk_plots.md),
[`pk_plot_ind()`](pk_plots.md)) and saved as png files in
`models/<run>/diag/`.

## Usage

``` r
pk_diagnostics(run, plots = FALSE, thresholds = list(), proj = NULL)
```

## Arguments

- run:

  A run reference or a run id.

- plots:

  Create and save the standard plots? Requires the full fit.

- thresholds:

  Named list overriding flag thresholds, see
  [`pk_flags()`](pk_flags.md).

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A `pk_diagnostics` object: a list with `summary` (one-row tibble),
`flags`, and (if `plots = TRUE`) `plots`.
