# Rule-based warnings for a run

Evaluates a set of rules on the result of a run and returns the
triggered flags. Flags are hints for the analyst, not decisions. Rules
and default thresholds (configurable in the `flags` section of
`config.yml`, or with `thresholds`):

## Usage

``` r
pk_flags(run, thresholds = list(), proj = NULL)
```

## Arguments

- run:

  A run reference or a run id.

- thresholds:

  Named list overriding the thresholds.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A tibble with the columns `flag`, `severity`, `parameter`, `value`,
`threshold`, `message`; zero rows if nothing is flagged.

## Details

- `cov_step_failed`: the covariance step did not run or failed.

- `condition_number`: condition number above `max_condition_number`
  (1000).

- `eta_shrinkage`: SD shrinkage of an ETA above `max_shrinkage_eta` (30
  %).

- `eps_shrinkage`: IWRES shrinkage above `max_shrinkage_eps` (30 %).

- `high_rse`: relative standard error of a fixed effect above `max_rse`
  (50 %).

- `omega_near_zero`: IIV variance below `min_omega` (1e-4).

- `cwres_bias`: absolute mean CWRES above `max_abs_mean_cwres` (0.5).

- `cwres_tails`: more than `max_pct_cwres_gt3` (2 %) of \|CWRES\| above
  3.

- `no_ofv`: the OFV is missing or not finite.
