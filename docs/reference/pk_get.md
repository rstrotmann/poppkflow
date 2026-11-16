# Access a run

- `pk_run_ref()` creates a run reference from a run id.

- `pk_get()` loads the full `nlmixr2` fit of a run (`fit.rds`).

- `pk_result()` loads the slim result (`result.rds`): OFV, parameter
  table, omega, shrinkage, ETAs. It does not need the fit.

## Usage

``` r
pk_get(run, proj = NULL)

pk_result(run, proj = NULL)

pk_run_ref(run, proj = NULL)
```

## Arguments

- run:

  A run reference or a run id.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

`pk_get()`: the fit; `pk_result()`: a list; `pk_run_ref()`: a
`pk_run_ref`.
