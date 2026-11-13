#' Project folders and configuration
#'
#' A poppkflow project is a folder with a `config.yml`, a `data/` folder, a
#' `ledger/` folder (`runs.csv` and `events.csv`), a `models/` folder with one
#' sub-folder per run, and `output/` and `report/` folders.
#'
#' `pk_project()` creates (or re-opens) a project and makes it the *current*
#' project, which is used by all other functions when `proj` is not given.
#'
#' @param path Project folder.
#' @param name Project name (stored in the config).
#' @param config Named list that overrides entries of the default config.
#' @param proj A `pk_project` object. Defaults to the current project.
#'
#' @return A `pk_project` object.
#' @examples
#' p <- pk_project(file.path(tempdir(), "demo_project"))
#' p
#' @export
pk_project <- function(path, name = basename(path), config = list()) {
  stopifnot(is_string(path))
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  for (d in c("data", "ledger", "models", "scm", "output/figures",
              "output/tables", "report")) {
    dir.create(file.path(path, d), recursive = TRUE, showWarnings = FALSE)
  }
  cfg_file <- file.path(path, "config.yml")
  if (file.exists(cfg_file)) {
    cfg <- yaml::read_yaml(cfg_file)
    cfg <- utils::modifyList(cfg, config)
  } else {
    cfg <- utils::modifyList(pk_default_config(name), config)
  }
  yaml::write_yaml(cfg, cfg_file)
  proj <- structure(list(path = normalizePath(path), name = cfg$project$name,
                         config = cfg), class = "pk_project")
  pk_use(proj)
  proj
}

#' @rdname pk_project
#' @export
pk_open <- function(path) {
  cfg_file <- file.path(path, "config.yml")
  if (!file.exists(cfg_file)) {
    cli::cli_abort("No {.file config.yml} in {.path {path}}. Create the project with {.fn pk_project}.")
  }
  cfg <- yaml::read_yaml(cfg_file)
  proj <- structure(
    list(
      path = normalizePath(path),
      name = cfg$project$name,
      config = cfg
    ),
    class = "pk_project"
  )
  pk_use(proj)
  proj
}

#' @rdname pk_project
#' @export
pk_use <- function(proj) {
  options(poppkflow.project = proj)
  invisible(proj)
}

#' @rdname pk_project
#' @export
pk_current <- function() {
  proj <- getOption("poppkflow.project")
  if (is.null(proj)) {
    cli::cli_abort("No current project. Use {.fn pk_project} or {.fn pk_open} first.")
  }
  proj
}

#' Horizontal line using unicode characters
#'
#' @param length Line length.
#'
#' @returns A character string.
#' @noRd
hline <- function(length = 8) {
  paste0(rep("\U2500", length), collapse = "")
}

#' @export
print.pk_project <- function(x, ...) {
  runs <- ledger_read(x)

  cli::cli_text(paste0(hline(), " PK Project ", hline(), "\n"))
  # cli::cli_text("{.cls pk_project} {.val {x$name}} at {.path {x$path}}")
  cli::cli_text("Project {x$name} at {.path {x$path}}")

  cli::cli_text("{nrow(runs)} run{?s} in the ledger")
  invisible(x)
}

# Re-read the config from disk (keeps the object in sync after edits)
pk_refresh <- function(proj) {
  pk_open(proj$path)
}

pk_default_config <- function(name = "project") {
  list(
    project = list(name = name),
    data = list(
      file = NULL, id = "ID", time = "TIME", dv = "DV", evid = "EVID",
      amt = "AMT", cmt = "CMT", na = "."
    ),
    covariates = list(),
    estimation = list(
      method = "saem_focei", seed = 2026, nBurn = 300, nEm = 300,
      maxOuterIterations = 2000, covMethod = "r,s"
    ),
    scm = list(forward_p = 0.01, backward_p = 0.001),
    flags = list(
      max_shrinkage_eta = 30, max_shrinkage_eps = 30, max_rse = 50,
      max_condition_number = 1000, min_omega = 1e-4, max_abs_mean_cwres = 0.5,
      max_pct_cwres_gt3 = 2
    ),
    report = list(title = "Population pharmacokinetic analysis", author = "")
  )
}

proj_path <- function(proj, ...) {
  file.path(proj$path, ...)
}

resolve_proj <- function(proj = NULL) {
  if (is.null(proj)) pk_current() else proj
}

#' Covariate dictionary
#'
#' Reads or sets the covariate dictionary stored in `config.yml`. Each entry has
#' a `name`, a `type` (`"continuous"` or `"categorical"`), and optionally `unit`,
#' `label`, `ref` (reference value used for centering), `min` and `max`
#' (plausible range used by [pk_check()]).
#'
#' @param covariates A data frame with at least the column `name`, or `NULL` to
#'   only read the dictionary.
#' @inheritParams pk_project
#' @return The dictionary as a tibble (invisibly when it is set).
#' @examples
#' p <- pk_project(file.path(tempdir(), "demo_cov"))
#' pk_covariates(tibble::tribble(
#'   ~name , ~type         , ~unit , ~ref,
#'   "WT"  , "continuous"  , "kg"  , 70  ,
#'   "SEXF", "categorical" , NA    , 0
#' ))
#' pk_covariates()
#' @export
pk_covariates <- function(covariates = NULL, proj = NULL) {
  proj <- resolve_proj(proj)
  if (is.null(covariates)) {
    proj <- pk_refresh(proj)
    return(cov_dict_to_tibble(proj$config$covariates))
  }
  covariates <- tibble::as_tibble(covariates)
  if (!"name" %in% names(covariates)) {
    cli::cli_abort("The covariate dictionary needs a {.field name} column.")
  }
  for (col in c("type", "unit", "label")) {
    if (!col %in% names(covariates)) covariates[[col]] <- NA_character_
  }
  for (col in c("ref", "min", "max")) {
    if (!col %in% names(covariates)) covariates[[col]] <- NA_real_
  }
  cfg_file <- proj_path(proj, "config.yml")
  cfg <- yaml::read_yaml(cfg_file)
  cfg$covariates <- lapply(seq_len(nrow(covariates)), function(i) {
    row <- as.list(covariates[i, c("name", "type", "unit", "label", "ref", "min", "max")])
    row[!vapply(row, function(v) length(v) == 1L && is.na(v), logical(1))]
  })
  yaml::write_yaml(cfg, cfg_file)
  pk_refresh(proj)
  invisible(cov_dict_to_tibble(cfg$covariates))
}

cov_dict_to_tibble <- function(x) {
  empty <- tibble::tibble(
    name = character(), type = character(), unit = character(), label = character(),
    ref = numeric(), min = numeric(), max = numeric()
  )
  if (length(x) == 0L) {
    return(empty)
  }
  rows <- lapply(x, function(e) {
    tibble::tibble(
      name  = e$name %||% NA_character_,
      type  = e$type %||% NA_character_,
      unit  = e$unit %||% NA_character_,
      label = e$label %||% NA_character_,
      ref   = num1(e$ref),
      min   = num1(e$min),
      max   = num1(e$max)
    )
  })
  dplyr::bind_rows(rows)
}
