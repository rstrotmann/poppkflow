# Summarise exposure metrics

Geometric mean, geometric CV (%), median, 5th and 95th percentile of the
exposure and secondary parameters of [`pk_exposure()`](pk_exposure.md),
optionally by subgroup.

## Usage

``` r
pk_exposure_summary(
  expo,
  by = NULL,
  metrics = intersect(c("AUC_ss", "Cmax_ss", "Cmin_ss", "Cavg_ss", "Tmax_ss", "cl", "v",
    "v2", "t_half_beta", "Vss"), names(expo))
)
```

## Arguments

- expo:

  Result of [`pk_exposure()`](pk_exposure.md).

- by:

  Optional column name(s) in `expo` defining subgroups (for continuous
  covariates use [`cut()`](https://rdrr.io/r/base/cut.html) first).

- metrics:

  Columns to summarise.

## Value

A tibble.
