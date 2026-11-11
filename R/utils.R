#' @importFrom rlang .data %||%
#' @importFrom stats AIC BIC median quantile sd setNames
NULL

# Canonical hash of an arbitrary R object (md5 of the serialization)
pk_hash <- function(x) {
  digest::digest(x, algo = "md5", serialize = TRUE)
}

pk_now <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%S")
}

pk_user <- function() {
  u <- Sys.info()[["user"]]
  if (is.null(u) || is.na(u) || !nzchar(u)) "unknown" else u
}

# Write a text file atomically: write a temp file in the same folder, then rename
atomic_write <- function(path, writer) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(pattern = ".tmp_", tmpdir = dirname(path))
  on.exit(unlink(tmp), add = TRUE)
  writer(tmp)
  ok <- file.rename(tmp, path)
  if (!ok) {
    file.copy(tmp, path, overwrite = TRUE)
  }
  invisible(path)
}

# Simple directory-based lock (mkdir is atomic). Stale locks (> 120 s) are removed.
with_lock <- function(path, code, timeout = 30) {
  lock <- paste0(path, ".lock")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  t0 <- Sys.time()
  repeat {
    if (dir.create(lock, showWarnings = FALSE)) {
      break
    }
    age <- as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "secs"))
    if (!is.na(age) && age > 120) {
      unlink(lock, recursive = TRUE)
      next
    }
    if (as.numeric(difftime(Sys.time(), t0, units = "secs")) > timeout) {
      cli::cli_abort("Could not obtain ledger lock {.path {lock}} within {timeout} s.")
    }
    Sys.sleep(0.05)
  }
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  force(code)
}

# Is `x` a single non-empty string?
is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
}

# Format a numeric run counter as run001, run002, ...
format_run_id <- function(n) {
  sprintf("run%03d", as.integer(n))
}

# Safe extraction of a numeric scalar
num1 <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0L) {
    return(default)
  }
  x <- suppressWarnings(as.numeric(x)[1])
  if (is.na(x)) default else x
}
