#' Register and read the analysis dataset
#'
#' With `file`, the dataset is copied into the project's `data/` folder (unless
#' it is already there), registered in `config.yml`, and returned. Without
#' `file`, the registered dataset is read. Missing values are read from the
#' `data$na` entry of the config (default `"."`).
#'
#' A file that is registered must contain the required columns `ID`, `TIME`,
#' `DV`, `EVID` and `AMT` (the names can be changed in the `data` section of
#' `config.yml`; names are case-sensitive). The header is checked before the
#' file is copied or registered, so a file with missing columns is rejected and
#' the project stays unchanged. `CMT` is optional. [pk_run()] repeats the check
#' for the data it fits, and [pk_check()] reports missing columns as errors.
#'
#' @param file Path to a csv file. Optional after the first call.
#' @param copy Copy the file into the project `data/` folder?
#' @inheritParams pk_project
#'
#' @return A data frame with attribute `data_hash` (md5 hash of the data).
#' @examples
#' p <- pk_project(file.path(tempdir(), "demo_data"))
#' d <- pk_data(system.file("extdata", "example_pk.csv", package = "poppkflow"))
#' head(d)
#' @export
pk_data <- function(file = NULL, copy = TRUE, proj = NULL) {
  proj <- resolve_proj(proj)
  cfg_file <- proj_path(proj, "config.yml")
  cfg <- yaml::read_yaml(cfg_file)
  if (!is.null(file)) {
    if (!file.exists(file)) {
      cli::cli_abort("Dataset {.path {file}} does not exist.")
    }
    header <- readr::read_csv(file, n_max = 0L, show_col_types = FALSE, progress = FALSE)
    assert_data_columns(names(header), cfg$data, what = paste0("The dataset ", basename(file)))
    target <- file
    if (copy) {
      target <- proj_path(proj, "data", basename(file))
      if (normalizePath(file) != normalizePath(target, mustWork = FALSE)) {
        file.copy(file, target, overwrite = TRUE)
      }
    }
    cfg$data$file <- if (copy) file.path("data", basename(file)) else normalizePath(file)
    yaml::write_yaml(cfg, cfg_file)
    proj <- pk_refresh(proj)
  }
  rel <- cfg$data$file
  if (is.null(rel)) {
    cli::cli_abort("No dataset registered. Call {.code pk_data(file = ...)} first.")
  }
  path <- if (grepl("^(/|[A-Za-z]:)", rel)) rel else proj_path(proj, rel)
  d <- readr::read_csv(path, na = c(cfg$data$na %||% ".", "", "NA"), show_col_types = FALSE, progress = FALSE)
  d <- as.data.frame(d)
  attr(d, "data_hash") <- data_hash_of(d)
  d
}

#' Check the analysis dataset
#'
#' Runs a set of data checks and returns the findings as a tibble: required
#' columns (`ID`, `TIME`, `DV`, `EVID`, `AMT`; a missing column is an error and
#' stops the remaining checks), non-numeric or missing values, duplicated
#' records, time order,
#' observations before the first dose, subjects without observations,
#' non-positive concentrations, and missing or out-of-range covariates (ranges
#' from the covariate dictionary, see [pk_covariates()]).
#'
#' @param data Data frame. Defaults to the registered dataset.
#' @inheritParams pk_project
#' @return A tibble with the columns `check`, `severity` (`"error"`, `"warning"`
#'   or `"info"`), `n` and `detail`. A tibble with zero rows means that no
#'   problem was found.
#' @examples
#' d <- tibble::tribble(
#'   ~ID, ~TIME, ~DV , ~AMT, ~EVID,
#'   1  , 0    , NA  , 100 , 1    ,
#'   1  , 1    , 2.5 , 0   , 0    ,
#'   1  , 2    , 1.9 , 0   , 0
#' )
#' pk_check(as.data.frame(d))
#' @export
pk_check <- function(data = NULL, proj = NULL) {
  if (is.null(data)) {
    proj <- resolve_proj(proj)
    data <- pk_data(proj = proj)
  } else {
    proj <- tryCatch(resolve_proj(proj), error = function(e) NULL)
  }
  if (is.null(proj)) {
    proj <- list(config = pk_default_config())
  }
  cfg <- proj$config$data
  cols <- required_data_columns(cfg)
  id <- cols[["id"]]
  time <- cols[["time"]]
  dv <- cols[["dv"]]
  evid <- cols[["evid"]]
  amt <- cols[["amt"]]
  out <- list()
  add <- function(check, severity, n, detail) {
    out[[length(out) + 1L]] <<- tibble::tibble(check = check, severity = severity, n = as.integer(n), detail = detail)
  }

  miss_cols <- missing_data_columns(names(data), cfg)
  if (length(miss_cols)) {
    add("required_columns", "error", length(miss_cols), paste("Missing column(s):", paste(miss_cols, collapse = ", ")))
    return(dplyr::bind_rows(out))
  }
  if (!is.numeric(data[[time]])) {
    add("time_numeric", "error", sum(!is.finite(suppressWarnings(as.numeric(data[[time]])))), "TIME is not numeric.")
    return(dplyr::bind_rows(out))
  }
  if (!is.numeric(data[[dv]])) {
    add("dv_numeric", "error", sum(!is.na(data[[dv]])), "DV is not numeric.")
    return(dplyr::bind_rows(out))
  }
  if (anyNA(data[[time]])) {
    add("time_missing", "error", sum(is.na(data[[time]])), "Records with missing TIME.")
  }

  obs <- data[[evid]] == 0
  dos <- data[[evid]] %in% c(1, 4)
  n_na_dv <- sum(obs & is.na(data[[dv]]))
  if (n_na_dv) {
    add("dv_missing_obs", "warning", n_na_dv, "Observation records (EVID = 0) with missing DV.")
  }
  n_nonpos <- sum(obs & !is.na(data[[dv]]) & data[[dv]] <= 0)
  if (n_nonpos) {
    add("dv_non_positive", "warning", n_nonpos, "Observations with DV <= 0 (BLQ handling needed?).")
  }
  dup <- duplicated(data[, intersect(c(id, time, evid), names(data)), drop = FALSE])
  if (any(dup)) {
    add("duplicated_records", "warning", sum(dup), "Records with duplicated ID/TIME/EVID.")
  }
  by_id <- split(seq_len(nrow(data)), data[[id]])
  n_unsorted <- sum(vapply(by_id, function(ix) is.unsorted(data[[time]][ix], na.rm = TRUE), logical(1)))
  if (n_unsorted) {
    add("time_order", "error", n_unsorted, "Subjects with non-monotonic TIME.")
  }
  if (any(dos)) {
    first_dose <- tapply(ifelse(dos, data[[time]], Inf), data[[id]], min)
    first_obs <- tapply(ifelse(obs & !is.na(data[[dv]]), data[[time]], Inf), data[[id]], min)
    ids <- names(first_dose)
    n_pre <- sum(first_obs[ids] < first_dose[ids])
    if (n_pre) {
      add("obs_before_first_dose", "warning", n_pre, "Subjects with an observation before the first dose record.")
    }
    n_nodose <- sum(!is.finite(first_dose))
    if (n_nodose) {
      add("subject_without_dose", "error", n_nodose, "Subjects without any dose record.")
    }
    n_amt <- sum(dos & (is.na(data[[amt]]) | data[[amt]] <= 0))
    if (n_amt) add("dose_amount", "error", n_amt, "Dose records with missing or non-positive AMT.")
  } else {
    add("no_dose_records", "warning", 0, "No dose records (EVID = 1) found.")
  }
  n_noobs <- sum(vapply(by_id, function(ix) !any(obs[ix] & !is.na(data[[dv]][ix])), logical(1)))
  if (n_noobs) {
    add("subject_without_obs", "warning", n_noobs, "Subjects without any observation.")
  }

  dict <- cov_dict_to_tibble(proj$config$covariates)
  for (i in seq_len(nrow(dict))) {
    nm <- dict$name[i]
    if (!nm %in% names(data)) {
      add("covariate_missing_column", "error", 0, paste0("Covariate ", nm, " is not in the dataset."))
      next
    }
    x <- data[[nm]]
    n_na <- sum(is.na(x))
    if (n_na) {
      add("covariate_missing_values", "warning", n_na, paste0("Missing values in covariate ", nm, "."))
    }
    if (is.numeric(x)) {
      if (!is.na(dict$min[i]) && any(x < dict$min[i], na.rm = TRUE)) {
        add("covariate_range", "warning", sum(x < dict$min[i], na.rm = TRUE), paste0(nm, " below ", dict$min[i], "."))
      }
      if (!is.na(dict$max[i]) && any(x > dict$max[i], na.rm = TRUE)) {
        add("covariate_range", "warning", sum(x > dict$max[i], na.rm = TRUE), paste0(nm, " above ", dict$max[i], "."))
      }
      varies <- tapply(x, data[[id]], function(v) length(unique(v[!is.na(v)])) > 1L)
      if (any(varies, na.rm = TRUE) && identical(dict$type[i], "continuous")) {
        add("covariate_time_varying", "info", sum(varies, na.rm = TRUE), paste0(nm, " varies within subject (treated as time-varying)."))
      }
    }
  }
  if (!length(out)) {
    return(tibble::tibble(check = character(), severity = character(), n = integer(), detail = character()))
  }
  dplyr::bind_rows(out)
}

# One row per subject with the covariates of the first record
subject_table <- function(data, proj = NULL, covariates = NULL) {
  proj <- resolve_proj(proj)
  id <- proj$config$data$id %||% "ID"
  if (is.null(covariates)) {
    covariates <- cov_dict_to_tibble(proj$config$covariates)$name
  }
  covariates <- intersect(covariates, names(data))
  d <- data[!duplicated(data[[id]]), c(id, covariates), drop = FALSE]
  names(d)[1] <- "ID"
  d$ID <- as.numeric(as.character(d$ID))
  tibble::as_tibble(d)
}

# ---- required columns -------------------------------------------------------------------------

# Columns that every analysis dataset must contain, named by role. The names come from the `data`
# section of the config (defaults ID, TIME, DV, EVID, AMT). CMT is optional and not listed.
required_data_columns <- function(cfg_data = NULL) {
  roles <- c("id", "time", "dv", "evid", "amt")
  vapply(roles, function(k) cfg_data[[k]] %||% toupper(k), character(1))
}

# Required columns that are not in `data_names`
missing_data_columns <- function(data_names, cfg_data = NULL) {
  req <- required_data_columns(cfg_data)
  req[!req %in% data_names]
}

# Abort if a required column is missing. `what` names the dataset in the message.
assert_data_columns <- function(data_names, cfg_data = NULL, what = "The dataset") {
  req <- unname(required_data_columns(cfg_data))
  miss <- req[!req %in% data_names]
  if (!length(miss)) {
    return(invisible(TRUE))
  }
  # a column that exists under a different case is the most likely cause
  found <- vapply(miss, function(m) data_names[tolower(data_names) == tolower(m)][1], character(1))
  case <- !is.na(found)
  msg <- c(
    "{what} is missing {length(miss)} required column{?s}: {.field {miss}}.",
    "i" = "Required columns: {.field {req}}. The names can be changed in the {.field data} section of {.file config.yml}."
  )
  if (any(case)) {
    msg <- c(msg, "i" = "Column names are case-sensitive: found {.field {found[case]}} instead of the required {.field {miss[case]}}.")
  }
  cli::cli_abort(msg)
}
