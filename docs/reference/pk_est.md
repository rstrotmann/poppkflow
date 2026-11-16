# Estimation settings

Creates the settings object used by [`pk_run()`](pk_run.md). Unspecified
arguments are taken from the `estimation` section of the project config.

## Usage

``` r
pk_est(
  method = NULL,
  seed = NULL,
  nBurn = NULL,
  nEm = NULL,
  maxOuterIterations = NULL,
  covMethod = NULL,
  ...,
  proj = NULL
)
```

## Arguments

- method:

  `"saem_focei"` (default), `"focei"` or `"saem"`.

- seed:

  Seed for SAEM.

- nBurn, nEm:

  Number of SAEM burn-in and EM iterations.

- maxOuterIterations:

  Maximum outer iterations of FOCEI.

- covMethod:

  Covariance step method for FOCEI (e.g. `"r,s"`).

- ...:

  Further settings stored with the run (not used by the default fit
  function).

- proj:

  A `pk_project`; only used to read defaults.

## Value

A `pk_est` object (a list).

## Details

The default method `"saem_focei"` estimates the model with SAEM and then
refines it with FOCEI started from the SAEM solution. With oral data
only and slow absorption (`ka` smaller than the elimination rate
constant) the individual likelihood surfaces can be multimodal. FOCEI
alone can then converge to different sets of individual modes depending
on the starting values, and the reported OFV of the same population
model can differ by several tens of points. SAEM locates the population
optimum and consistent individual modes robustly, and FOCEI started from
it provides the FOCEI OFV used for likelihood-ratio tests and the
covariance step. The OFV reproducibility across SAEM seeds can be
checked with [`pk_repro()`](pk_repro.md).

## Examples

``` r
pk_est(method = "focei")
#> <pk_est> "focei", seed 2026
```
