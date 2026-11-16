# Screen ETAs against covariates

Graphical and numerical exploration of the empirical Bayes estimates
(ETAs) of a run against the covariates (those of the covariate
dictionary, or the columns named in `covariates`). Continuous
covariates: Pearson correlation and its p-value. Categorical covariates:
Wilcoxon test (two levels) or Kruskal-Wallis test (more levels). The
shrinkage of each ETA is reported because ETA-based screening is
unreliable for shrunk ETAs.

## Usage

``` r
pk_screen_covariates(
  run,
  covariates = NULL,
  data = NULL,
  plots = FALSE,
  proj = NULL
)
```

## Arguments

- run:

  A run reference or a run id.

- covariates:

  Covariate names; default: the covariate dictionary.

- data:

  Dataset containing the covariates. Default: registered data.

- plots:

  Also return the plots ([`pk_plot_eta_cov()`](pk_plots.md))?

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A tibble with the columns `ETA`, `Covariate`, `Type`, `Test`,
`Statistic`, `p`, `n`, `Shrinkage (%)`. With `plots = TRUE` a list with
`table` and `plots`.
