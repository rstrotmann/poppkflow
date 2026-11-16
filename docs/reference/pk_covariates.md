# Covariate dictionary

Reads or sets the covariate dictionary stored in `config.yml`. Each
entry has a `name`, a `type` (`"continuous"` or `"categorical"`), and
optionally `unit`, `label`, `ref` (reference value used for centering),
`min` and `max` (plausible range used by [`pk_check()`](pk_check.md)).

## Usage

``` r
pk_covariates(covariates = NULL, proj = NULL)
```

## Arguments

- covariates:

  A data frame with at least the column `name`, or `NULL` to only read
  the dictionary.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

The dictionary as a tibble (invisibly when it is set).

## Examples

``` r
p <- pk_project(file.path(tempdir(), "demo_cov"))
pk_covariates(tibble::tribble(
  ~name , ~type         , ~unit , ~ref,
  "WT"  , "continuous"  , "kg"  , 70  ,
  "SEXF", "categorical" , NA    , 0
))
pk_covariates()
#> # A tibble: 2 × 7
#>   name  type        unit  label   ref   min   max
#>   <chr> <chr>       <chr> <chr> <dbl> <dbl> <dbl>
#> 1 WT    continuous  kg    NA       70    NA    NA
#> 2 SEXF  categorical NA    NA        0    NA    NA
```
