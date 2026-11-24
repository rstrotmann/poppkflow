# Project folders and configuration

A poppkflow project is a folder with a `config.yml`, a `data/` folder, a
`ledger/` folder (`runs.csv` and `events.csv`), a `models/` folder with
one sub-folder per run, and `output/` and `report/` folders.

## Usage

``` r
pk_project(path, name = basename(path), config = list())

pk_open(path)

pk_use(proj)

pk_current()
```

## Arguments

- path:

  Project folder.

- name:

  Project name (stored in the config).

- config:

  Named list that overrides entries of the default config.

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

A `pk_project` object.

## Details

`pk_project()` creates (or re-opens) a project and makes it the
*current* project, which is used by all other functions when `proj` is
not given.

## Examples

``` r
p <- pk_project(file.path(tempdir(), "demo_project"))
p
#> ──────── PK Project ────────
#> Project demo_project at
#> /private/var/folders/59/9639tths0zn82tnyr73znh6c0000gn/T/RtmpEd3bfM/demo_project
#> 0 runs in the ledger
```
