# Diagnostic plots

- `pk_plot_gof()`: observed vs PRED, observed vs IPRED, CWRES vs time,
  CWRES vs PRED. Returns a combined plot if the patchwork package is
  installed, otherwise a list of ggplots.

- `pk_plot_eta()`: histograms and QQ plots of the ETAs.

- `pk_plot_eta_cov()`: ETAs vs covariates (scatter plots with smoother
  for continuous, boxplots for categorical covariates).

- `pk_plot_ind()`: individual fits (observed, IPRED, PRED).

## Usage

``` r
pk_plot_gof(run, log = TRUE, proj = NULL)

pk_plot_eta(run, proj = NULL)

pk_plot_eta_cov(run, covariates = NULL, data = NULL, proj = NULL)

pk_plot_ind(run, ids = NULL, n = 12, log = TRUE, proj = NULL)
```

## Arguments

- run:

  A run reference or a run id.

- log:

  Use log axes for concentrations?

- proj:

  A `pk_project` object. Defaults to the current project.

- covariates:

  Covariate names; default: the covariate dictionary.

- data:

  Dataset that contains the covariates. Default: registered data.

- ids:

  Subject ids for individual fits; default: the first `n`.

- n:

  Number of subjects to show.

## Value

A ggplot (or patchwork), or a list of ggplots.
