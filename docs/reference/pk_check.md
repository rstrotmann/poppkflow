# Check the analysis dataset

Runs a set of data checks and returns the findings as a tibble: required
columns (`ID`, `TIME`, `DV`, `EVID`, `AMT`; a missing column is an error
and stops the remaining checks), non-numeric or missing values,
duplicated records, time order, observations before the first dose,
subjects without observations, non-positive concentrations, and missing
or out-of-range covariates (ranges from the covariate dictionary, see
[`pk_covariates()`](pk_covariates.md)).

## Usage

``` r
pk_check(data = NULL, proj = NULL)
```

## Arguments

- data:

  Data frame. Defaults to the registered dataset.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A tibble with the columns `check`, `severity` (`"error"`, `"warning"` or
`"info"`), `n` and `detail`. A tibble with zero rows means that no
problem was found.

## Examples

``` r
d <- tibble::tribble(
  ~ID, ~TIME, ~DV , ~AMT, ~EVID,
  1  , 0    , NA  , 100 , 1    ,
  1  , 1    , 2.5 , 0   , 0    ,
  1  , 2    , 1.9 , 0   , 0
)
pk_check(as.data.frame(d))
#> # A tibble: 0 × 4
#> # ℹ 4 variables: check <chr>, severity <chr>, n <int>, detail <chr>
```
