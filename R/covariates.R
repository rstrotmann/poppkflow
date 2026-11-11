#' Screen ETAs against covariates
#'
#' Graphical and numerical exploration of the empirical Bayes estimates (ETAs)
#' of a run against the covariates (those of the covariate dictionary, or the
#' columns named in `covariates`). Continuous covariates: Pearson correlation
#' and its p-value. Categorical covariates: Wilcoxon test (two levels) or
#' Kruskal-Wallis test (more levels). The shrinkage of each ETA is reported
#' because ETA-based screening is unreliable for shrunk ETAs.
#'
#' @param run A run reference or a run id.
#' @param covariates Covariate names; default: the covariate dictionary.
#' @param data Dataset containing the covariates. Default: registered data.
#' @param plots Also return the plots ([pk_plot_eta_cov()])?
#' @inheritParams pk_project
#' @return A tibble with the columns `ETA`, `Covariate`, `Type`, `Test`,
#'   `Statistic`, `p`, `n`, `Shrinkage (%)`. With `plots = TRUE` a list with
#'   `table` and `plots`.
#' @export
pk_screen_covariates <- function(run, covariates = NULL, data = NULL, plots = FALSE, proj = NULL) {
  tab <- eta_cov_table(run, covariates, data, proj)
  res <- pk_result(run, proj)
  d <- tab$data
  etas <- grep("^eta\\.", names(d), value = TRUE)
  rows <- list()
  for (e in etas) {
    sh <- num1(res$eta_shrink[[e]] %||% NA)
    for (cn in tab$continuous) {
      ok <- stats::complete.cases(d[[e]], d[[cn]])
      if (sum(ok) > 3L && stats::sd(d[[cn]][ok]) > 0) {
        ct <- stats::cor.test(d[[e]][ok], d[[cn]][ok])
        rows[[length(rows) + 1L]] <- tibble::tibble(
          ETA = e, Covariate = cn, Type = "continuous", Test = "Pearson r",
          Statistic = unname(ct$estimate), p = ct$p.value, n = sum(ok), `Shrinkage (%)` = sh
        )
      }
    }
    for (cn in tab$categorical) {
      ok <- stats::complete.cases(d[[e]], d[[cn]])
      lv <- unique(d[[cn]][ok])
      if (sum(ok) > 3L && length(lv) > 1L) {
        if (length(lv) == 2L) {
          p <- suppressWarnings(stats::wilcox.test(d[[e]][ok] ~ factor(d[[cn]][ok]))$p.value)
          test <- "Wilcoxon"
        } else {
          p <- stats::kruskal.test(d[[e]][ok], factor(d[[cn]][ok]))$p.value
          test <- "Kruskal-Wallis"
        }
        rows[[length(rows) + 1L]] <- tibble::tibble(
          ETA = e, Covariate = cn, Type = "categorical", Test = test,
          Statistic = NA_real_, p = p, n = sum(ok), `Shrinkage (%)` = sh
        )
      }
    }
  }
  out <- dplyr::arrange(dplyr::bind_rows(rows), .data$ETA, .data$Covariate)
  if (plots) {
    return(list(table = out, plots = pk_plot_eta_cov(run, covariates, data, proj)))
  }
  out
}
