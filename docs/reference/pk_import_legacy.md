# Import existing runs into the ledger

Imports fits that were saved as `<run>.rds` (and optionally the
generated model code `<run>.txt`) by script-based workflows into the
ledger of the current project. The parent of each run is taken from run
history tables that have the columns `Run` and `Reference` (for example
the development tables written by a numbered-script workflow). Imported
runs get the stage `"legacy"`, the decision `"pending"`, and no run key
(they are never returned by the cache). The model code, if available, is
stored as a custom specification so that the run can be used as a
parent.

## Usage

``` r
pk_import_legacy(
  models_dir,
  history = character(),
  pattern = "^run[0-9A-Za-z_]+\\.rds$",
  quiet = FALSE,
  proj = NULL
)
```

## Arguments

- models_dir:

  Folder with `<run>.rds` files.

- history:

  Character vector of csv files with the columns `Run` and `Reference`
  (and optionally `Description`).

- pattern:

  Regular expression for the file names to import.

- quiet:

  Suppress progress messages?

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

The imported run ids (invisibly).

## Details

The source folder is only read, never modified.
