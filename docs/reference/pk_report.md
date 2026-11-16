# Generate the model development report

Renders an R Markdown template that reads the ledger and the stored
results: data checks, the model development history and lineage tree
with decisions and rationale, the final model (code, parameter table,
flags, diagnostic plots), SCM steps, bootstrap, VPC and exposure results
if they exist, software versions, and the audit trail. Pandoc is
required; the function looks in the usual RStudio and Quarto locations
if it is not on the path.

## Usage

``` r
pk_report(
  final,
  template = "regulatory",
  output_format = "html_document",
  output_dir = NULL,
  quiet = TRUE,
  proj = NULL
)
```

## Arguments

- final:

  Final run (reference or id).

- template:

  `"regulatory"` (the bundled template) or the path to your own Rmd file
  with the parameters `proj_path` and `final_id`.

- output_format:

  An rmarkdown output format, e.g. `"html_document"`.

- output_dir:

  Output folder. Default: `report/` in the project.

- quiet:

  Suppress rendering messages?

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

The path of the rendered file.
