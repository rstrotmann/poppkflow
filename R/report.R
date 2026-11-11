#' Generate the model development report
#'
#' Renders an R Markdown template that reads the ledger and the stored results:
#' data checks, the model development history and lineage tree with decisions
#' and rationale, the final model (code, parameter table, flags, diagnostic
#' plots), SCM steps, bootstrap, VPC and exposure results if they exist,
#' software versions, and the audit trail. Pandoc is required; the function
#' looks in the usual RStudio and Quarto locations if it is not on the path.
#'
#' @param final Final run (reference or id).
#' @param template `"regulatory"` (the bundled template) or the path to your own
#'   Rmd file with the parameters `proj_path` and `final_id`.
#' @param output_format An rmarkdown output format, e.g. `"html_document"`.
#' @param output_dir Output folder. Default: `report/` in the project.
#' @param quiet Suppress rendering messages?
#' @inheritParams pk_project
#' @return The path of the rendered file.
#' @export
pk_report <- function(final, template = "regulatory", output_format = "html_document",
                      output_dir = NULL, quiet = TRUE, proj = NULL) {
  if (!requireNamespace("rmarkdown", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) {
    cli::cli_abort("The packages {.pkg rmarkdown} and {.pkg knitr} are needed for {.fn pk_report}.")
  }
  r <- resolve_run(final, proj)
  proj <- pk_refresh_quiet(r$proj)
  ensure_pandoc()
  tmpl <- if (identical(template, "regulatory")) {
    system.file("templates", "report.Rmd", package = "poppkflow")
  } else {
    template
  }
  if (!nzchar(tmpl) || !file.exists(tmpl)) {
    cli::cli_abort("Report template not found.")
  }
  output_dir <- output_dir %||% proj_path(proj, "report")
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  work <- tempfile("report_")
  dir.create(work)
  file.copy(tmpl, file.path(work, "report.Rmd"))
  out <- rmarkdown::render(
    file.path(work, "report.Rmd"),
    output_format = output_format,
    output_file = paste0("report_", r$run_id, ".", if (grepl("word", output_format)) "docx" else if (grepl("pdf", output_format)) "pdf" else "html"),
    output_dir = normalizePath(output_dir),
    params = list(proj_path = proj$path, final_id = r$run_id),
    envir = new.env(parent = globalenv()), quiet = quiet
  )
  event_append(proj, r$run_id, "report", basename(out))
  out
}

ensure_pandoc <- function() {
  if (rmarkdown::pandoc_available()) {
    return(invisible(TRUE))
  }
  candidates <- c(
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/x86_64",
    "/Applications/RStudio.app/Contents/MacOS/pandoc",
    "/Applications/RStudio.app/Contents/Resources/app/bin/pandoc",
    "/opt/homebrew/bin", "/usr/local/bin", "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools"
  )
  for (d in candidates[dir.exists(candidates)]) {
    rmarkdown::find_pandoc(dir = d)
    if (rmarkdown::pandoc_available()) {
      return(invisible(TRUE))
    }
  }
  cli::cli_abort("Pandoc was not found. Install pandoc or RStudio, or set {.envvar RSTUDIO_PANDOC}.")
}
