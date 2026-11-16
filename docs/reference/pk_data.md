# Register and read the analysis dataset

With `file`, the dataset is copied into the project's `data/` folder
(unless it is already there), registered in `config.yml`, and returned.
Without `file`, the registered dataset is read. Missing values are read
from the `data$na` entry of the config (default `"."`).

## Usage

``` r
pk_data(file = NULL, copy = TRUE, proj = NULL)
```

## Arguments

- file:

  Path to a csv file. Optional after the first call.

- copy:

  Copy the file into the project `data/` folder?

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A data frame with attribute `data_hash` (md5 hash of the data).

## Details

A file that is registered must contain the required columns `ID`,
`TIME`, `DV`, `EVID` and `AMT` (the names can be changed in the `data`
section of `config.yml`; names are case-sensitive). The header is
checked before the file is copied or registered, so a file with missing
columns is rejected and the project stays unchanged. `CMT` is optional.
[`pk_run()`](pk_run.md) repeats the check for the data it fits, and
[`pk_check()`](pk_check.md) reports missing columns as errors.

## Examples

``` r
p <- pk_project(file.path(tempdir(), "demo_data"))
d <- pk_data(system.file("extdata", "example_pk.csv", package = "poppkflow"))
head(d)
#>   ID TIME     DV AMT EVID   WT AGE SEXF CRCL
#> 1  1 0.00     NA 100    1 83.2  27    0   71
#> 2  1 0.25 0.0514   0    0 83.2  27    0   71
#> 3  1 0.50 0.2651   0    0 83.2  27    0   71
#> 4  1 1.06 0.5431   0    0 83.2  27    0   71
#> 5  1 2.07 0.9640   0    0 83.2  27    0   71
#> 6  1 3.00 0.9350   0    0 83.2  27    0   71
```
