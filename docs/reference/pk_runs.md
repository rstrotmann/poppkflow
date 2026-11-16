# Runs, history, audit trail and lineage

- `pk_runs()` returns the complete ledger (`runs.csv`) as a tibble.

- `pk_history()` returns a compact development history: one row per run
  with OFV, dOFV against the parent, number of flags, and the decision.

- `pk_audit()` returns the append-only event log (`events.csv`).

- `pk_tree()` prints the lineage tree of the runs (parent to child) and
  returns it invisibly as text; `format = "mermaid"` returns a mermaid
  flowchart definition instead.

## Usage

``` r
pk_runs(proj = NULL)

pk_history(stage = NULL, proj = NULL)

pk_audit(run = NULL, proj = NULL)

pk_tree(root = NULL, format = c("text", "mermaid"), proj = NULL)
```

## Arguments

- proj:

  A `pk_project` object. Defaults to the current project.

- stage:

  Optional character vector to filter on the `stage` column.

- run:

  Optional run id to filter the audit trail.

- root:

  Optional run id: show only the sub-tree below this run.

- format:

  `"text"` or `"mermaid"`.

## Value

A tibble, or the tree as a character vector.

## Examples

``` r
p <- pk_project(file.path(tempdir(), "demo_runs"))
pk_runs()
#> # A tibble: 0 × 36
#> # ℹ 36 variables: run_id <chr>, parent_id <chr>, stage <chr>,
#> #   description <chr>, created <chr>, finished <chr>, user <chr>,
#> #   spec_hash <chr>, data_hash <chr>, settings_hash <chr>, run_key <chr>,
#> #   seed <dbl>, est_method <chr>, nlmixr2_version <chr>, rxode2_version <chr>,
#> #   package_version <chr>, r_version <chr>, status <chr>, error_message <chr>,
#> #   ofv <dbl>, dofv_parent <dbl>, aic <dbl>, bic <dbl>, n_theta <int>,
#> #   n_eta <int>, n_par <int>, cond_no <dbl>, cov_step <lgl>, runtime_s <dbl>, …
```
