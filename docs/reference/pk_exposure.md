# Individual exposure and secondary parameters

Derives individual secondary PK parameters from the empirical Bayes
estimates of a run (half-lives, volume of distribution at steady state)
and simulates the individual steady-state profile for a dosing regimen
to obtain Cmax, Tmax, Cmin and AUC over the dosing interval. `AUC_ss` is
computed as dose / CL (linear pharmacokinetics; `CL` is `CL/F` for oral
data) and `AUC_ss_sim` by the trapezoidal rule on the simulated profile.

## Usage

``` r
pk_exposure(
  run,
  dose,
  ii = 24,
  n_doses = 14,
  dt = 0.1,
  data = NULL,
  proj = NULL
)
```

## Arguments

- run:

  A completed run (reference or id).

- dose:

  Dose amount per administration (same unit as `AMT` in the data).

- ii:

  Dosing interval (same time unit as the data).

- n_doses:

  Number of doses before the evaluated interval; the last interval is
  evaluated. Use a number large enough to reach steady state.

- dt:

  Time step of the simulated profile.

- data:

  Dataset with the subject covariates. Default: registered data.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A tibble with one row per subject: individual parameters, secondary
parameters, exposure metrics and the covariates of the dictionary.

## Details

Dose and concentration units follow the data (for example dose in the
unit of `AMT`, concentration in dose unit per volume unit). The result
is stored in `models/<run>/eval/exposure.rds`.
