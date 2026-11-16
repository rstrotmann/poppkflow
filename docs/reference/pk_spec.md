# Declarative model specification

A model is described as data, not as code: structure (number of
compartments, route, lag time), inter-individual variability (IIV),
omega blocks, inter-occasion variability (IOV), residual error model,
covariate relationships, and initial estimates.
[`pk_compile()`](pk_compile.md) turns the specification into an
`nlmixr2` model function.

## Usage

``` r
pk_spec(
  ncmt = 2,
  route = c("oral", "iv"),
  lag = FALSE,
  iiv = NULL,
  blocks = list(),
  iov = NULL,
  error = c("prop", "add", "comb"),
  covariates = NULL,
  extra_thetas = list(),
  inits = list(),
  custom_fn = NULL
)
```

## Arguments

- ncmt:

  Number of compartments (1 or 2).

- route:

  `"oral"` (first-order absorption) or `"iv"`.

- lag:

  Include an absorption lag time (oral only)?

- iiv:

  Parameters with IIV. Default: clearance and the central volume.

- blocks:

  List of character vectors; each defines an omega block, e.g.
  `list(c("cl", "v2"))`.

- iov:

  `NULL` or a list with `param`, `occasion` (column name) and `levels`,
  see [`pk_add_iov()`](pk_edit.md).

- error:

  Residual error model: `"prop"`, `"add"` or `"comb"`.

- covariates:

  Covariate relationships (a tibble, see [`pk_add_cov()`](pk_edit.md)).

- extra_thetas:

  Named list of additional fixed-effect parameters (name = initial
  value) that are written to the `ini` block.

- inits:

  Named list with initial estimates (natural scale): structural
  parameters, `prop.sd`, `add.sd`, `om` (named vector of IIV variances)
  and `om_cov` (named vector of block covariances, e.g.
  `c(cl.v2 = 0.01)`). Missing entries get defaults.

- custom_fn:

  Escape hatch: a ready-made `nlmixr2` model function. When given, it is
  used as is and the other arguments are ignored (except for
  bookkeeping), so the editing verbs such as
  [`pk_add_iiv()`](pk_edit.md) do not apply.

## Value

A `pk_spec` object.

## Details

Parameters are named `ka`, `cl`, `v` (one compartment) or `v2`, `q`,
`v3` (two compartments), and `tlag`. All structural parameters are
log-normal: `P = exp(lP + covariate terms + eta.P)`.

## Examples

``` r
spec <- pk_spec(ncmt = 2, lag = TRUE, iiv = c("ka", "cl", "v2"), error = "prop")
spec
#> <pk_spec>: 2-compartment, oral, lag time
#> IIV: "ka", "cl", and "v2"; blocks: 0; IOV: none
#> Residual error: "prop"
#> Covariates: none
```
