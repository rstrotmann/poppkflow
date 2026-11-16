# Edit a model specification

Each verb takes a specification, a candidate, a run, or a run id as
first argument and returns a *candidate*: the edited specification
together with the parent run. A candidate is fitted with
[`pk_run()`](pk_run.md), which records the lineage in the ledger.

## Usage

``` r
pk_set_structure(x, ncmt = NULL, route = NULL, lag = NULL, proj = NULL)

pk_add_iiv(x, ..., proj = NULL)

pk_drop_iiv(x, ..., proj = NULL)

pk_set_block(x, params, proj = NULL)

pk_drop_block(x, params, proj = NULL)

pk_add_iov(
  x,
  param,
  occasion = "OCC",
  levels = c(1, 2),
  init = 0.01,
  proj = NULL
)

pk_set_error(x, error = c("prop", "add", "comb"), proj = NULL)

pk_add_cov(
  x,
  param,
  covariate = NA_character_,
  form = c("power", "linear", "cat", "custom"),
  ref = NULL,
  init = 0,
  fix = FALSE,
  term = NULL,
  theta = NULL,
  id = NULL,
  proj = NULL
)

pk_drop_cov(x, id, proj = NULL)

pk_set_inits(x, ..., proj = NULL)
```

## Arguments

- x:

  A `pk_spec`, `pk_candidate`, `pk_run_ref`, or a run id.

- ncmt, route, lag:

  Model structure, see [`pk_spec()`](pk_spec.md).

- proj:

  A `pk_project`.

- ...:

  Parameter names (character), or named initial values for
  `pk_set_inits()`.

- params:

  Character vector of parameters for an omega block.

- param:

  Parameter name.

- occasion:

  Name of the occasion column (IOV).

- levels:

  Occasion levels (IOV).

- init:

  Initial value.

- error:

  Residual error model.

- covariate:

  Covariate (column) name.

- form:

  Functional form: `"power"` (`theta * log(COV / ref)`), `"linear"`
  (`theta * (COV - ref)`), `"cat"` (`theta * COV`, for 0/1 coded
  covariates), or `"custom"` (give `term` and `theta`).

- ref:

  Reference value for centering (`power` and `linear`). If `NULL`, the
  value from the covariate dictionary is used, see
  [`pk_covariates()`](pk_covariates.md).

- fix:

  Fix the covariate parameter at `init`?

- term, theta, id:

  Custom term (e.g. `"ss_cl * (OCC - 1)"`), parameter name, and
  relationship id for `form = "custom"`.

## Value

A `pk_candidate`.

## Examples

``` r
base <- pk_spec(ncmt = 2, iiv = c("cl", "v2"))
base |>
  pk_add_iiv("ka") |>
  pk_set_block(c("cl", "v2")) |>
  pk_set_error("comb") |>
  pk_add_cov("cl", "WT", form = "power", ref = 70, init = 0.75)
#> <pk_candidate> derived from NA
#> <pk_spec>: 2-compartment, oral
#> IIV: "cl", "v2", and "ka"; blocks: 1; IOV: none
#> Residual error: "comb"
#> Covariates: CL~WT
```
