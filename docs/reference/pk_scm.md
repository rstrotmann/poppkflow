# Stepwise covariate modelling (SCM)

Forward inclusion followed by backward elimination, starting from a base
run. Every model that is fitted is a regular run in the ledger (stage
`"scm"`), with the current model as parent, so the complete search is
documented in the run tree. The step table is stored in
`scm/<scm_id>.rds`.

## Usage

``` r
pk_scm(
  base,
  candidates,
  forward_p = NULL,
  backward_p = NULL,
  data = NULL,
  est = NULL,
  require_cov_step = TRUE,
  inits_from_parent = TRUE,
  decide_trials = TRUE,
  fit_fun = NULL,
  quiet = FALSE,
  proj = NULL
)
```

## Arguments

- base:

  Base run (reference or id), completed.

- candidates:

  Tibble with one row per covariate relationship and the columns
  `param`, `covariate`, `form`, `ref` (for `power` and `linear`) and
  `init`. Relationships with a fixed parameter are not supported (a
  likelihood-ratio test needs a free parameter).

- forward_p, backward_p:

  Significance levels (1 df). Defaults from the `scm` section of
  `config.yml`.

- data:

  Data frame. Default: the registered dataset.

- est:

  Estimation settings, see [`pk_est()`](pk_est.md).

- require_cov_step:

  Only select candidates whose model has a successful covariance step in
  the forward step?

- inits_from_parent:

  Start each trial model from the final estimates of the current model?

- decide_trials:

  Mark non-selected trial runs as rejected?

- fit_fun, quiet, proj:

  See [`pk_run()`](pk_run.md).

## Value

A `pk_scm` object: a list with `steps` (tibble), `final` (run reference
of the final model), `selected`, `forward_selected`, `criteria`, and
`scm_id`.

## Details

Forward step: all remaining candidates are added one at a time to the
current model; the candidate with the largest drop in OFV is included if
the drop exceeds the chi-square criterion with 1 df at `forward_p`
(default 0.01, dOFV \> 6.63). Backward step: each included candidate is
removed one at a time; the one with the smallest increase in OFV is
removed if the increase is below the criterion at `backward_p` (default
0.001, dOFV \> 10.83).

Runs that were not selected in a step are marked as rejected (with the
dOFV in the reason) when `decide_trials = TRUE`; runs on the selected
path stay pending so that the analyst records the decision.

## Examples

``` r
# \donttest{
cands <- tibble::tribble(
  ~param, ~covariate, ~form   , ~ref, ~init,
  "cl"  , "WT"      , "power" , 70  , 0.75 ,
  "cl"  , "CRCL"    , "power" , 100 , 0.1  ,
  "v2"  , "WT"      , "power" , 70  , 1
)
# scm <- pk_scm(base_run, cands)
# }
```
