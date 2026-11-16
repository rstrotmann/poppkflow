# Visual predictive check

Thin wrapper around
[`nlmixr2plot::vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.html)
(which uses the vpc package). The plot is stored in
`models/<run>/eval/`.

## Usage

``` r
pk_vpc(
  run,
  n = 300,
  pred_corr = FALSE,
  stratify = NULL,
  log_y = FALSE,
  ...,
  proj = NULL
)
```

## Arguments

- run:

  A run (reference or id).

- n:

  Number of simulated replicates.

- pred_corr:

  Prediction-corrected VPC?

- stratify:

  Optional character vector of stratification columns.

- log_y:

  Log-scale y axis?

- ...:

  Further arguments for
  [`nlmixr2plot::vpcPlot()`](https://nlmixr2.github.io/nlmixr2plot/reference/vpcPlot.html).

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A ggplot object.
