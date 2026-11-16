# Simulate a small example dataset

Simulates a single-dose oral study from a two-compartment model with
first-order absorption and lag time. Clearance depends on body weight
(allometric, exponent 0.75) and creatinine clearance (exponent 0.3), the
central volume on body weight (exponent 1). Residual error is combined
proportional and additive. The result is used in the examples, tests and
the vignette
(`system.file("extdata", "example_pk.csv", package = "poppkflow")`).

## Usage

``` r
pk_simulate_example(n = 40, seed = 2026, dose = 100)
```

## Arguments

- n:

  Number of subjects.

- seed:

  Random seed.

- dose:

  Dose per subject (dose unit; concentrations are in dose unit per
  litre).

## Value

A data frame in nlmixr2 format with the columns `ID`, `TIME`, `DV`,
`AMT`, `EVID`, `WT`, `AGE`, `SEXF` and `CRCL`.

## Examples

``` r
d <- pk_simulate_example(n = 10)
#>  
#>  
head(d)
#>   ID TIME     DV AMT EVID   WT AGE SEXF CRCL
#> 1  1 0.00     NA 100    1 83.2  40    1   92
#> 2  1 0.25 0.0100   0    0 83.2  40    1   92
#> 3  1 0.50 0.7341   0    0 83.2  40    1   92
#> 4  1 0.99 1.2596   0    0 83.2  40    1   92
#> 5  1 1.97 2.3039   0    0 83.2  40    1   92
#> 6  1 2.75 1.5774   0    0 83.2  40    1   92
```
