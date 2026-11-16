# Structural parameter names of a specification

Structural parameter names of a specification

## Usage

``` r
pk_params(spec)
```

## Arguments

- spec:

  A `pk_spec`.

## Value

Character vector of parameter names.

## Examples

``` r
pk_params(pk_spec(ncmt = 1, lag = TRUE))
#> [1] "ka"   "cl"   "v"    "tlag"
```
