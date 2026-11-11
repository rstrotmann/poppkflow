#' poppkflow: structured, iterative population PK workflow with nlmixr2
#'
#' Population PK model development is a loop: specify a model, fit it, diagnose
#' it, compare it with its parent, decide, and modify. poppkflow supports this
#' loop with
#'
#' * declarative model specifications ([pk_spec()], [pk_add_iiv()],
#'   [pk_add_cov()], ...), compiled to `nlmixr2` code ([pk_compile()]),
#' * a run ledger (`ledger/runs.csv` plus RDS files per run) filled by
#'   [pk_run()], with caching, lineage and an audit trail ([pk_history()],
#'   [pk_tree()], [pk_audit()]),
#' * a standard diagnostics bundle and rule-based flags ([pk_diagnostics()],
#'   [pk_flags()], [pk_suggest()]),
#' * comparison and decisions with rationale ([pk_compare()], [pk_decide()]),
#' * covariate screening and stepwise covariate modelling
#'   ([pk_screen_covariates()], [pk_scm()]),
#' * robustness checks ([pk_multistart()], [pk_repro()]),
#' * evaluation ([pk_bootstrap()], [pk_vpc()], [pk_exposure()]), and
#' * reporting from the ledger ([pk_report()]).
#'
#' @keywords internal
"_PACKAGE"
