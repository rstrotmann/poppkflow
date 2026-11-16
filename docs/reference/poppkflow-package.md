# poppkflow: structured, iterative population PK workflow with nlmixr2

Population PK model development is a loop: specify a model, fit it,
diagnose it, compare it with its parent, decide, and modify. poppkflow
supports this loop with

## Details

- declarative model specifications ([`pk_spec()`](pk_spec.md),
  [`pk_add_iiv()`](pk_edit.md), [`pk_add_cov()`](pk_edit.md), ...),
  compiled to `nlmixr2` code ([`pk_compile()`](pk_compile.md)),

- a run ledger (`ledger/runs.csv` plus RDS files per run) filled by
  [`pk_run()`](pk_run.md), with caching, lineage and an audit trail
  ([`pk_history()`](pk_runs.md), [`pk_tree()`](pk_runs.md),
  [`pk_audit()`](pk_runs.md)),

- a standard diagnostics bundle and rule-based flags
  ([`pk_diagnostics()`](pk_diagnostics.md), [`pk_flags()`](pk_flags.md),
  [`pk_suggest()`](pk_suggest.md)),

- comparison and decisions with rationale
  ([`pk_compare()`](pk_compare.md), [`pk_decide()`](pk_decide.md)),

- covariate screening and stepwise covariate modelling
  ([`pk_screen_covariates()`](pk_screen_covariates.md),
  [`pk_scm()`](pk_scm.md)),

- robustness checks ([`pk_multistart()`](pk_multistart.md),
  [`pk_repro()`](pk_repro.md)),

- evaluation ([`pk_bootstrap()`](pk_bootstrap.md),
  [`pk_vpc()`](pk_vpc.md), [`pk_exposure()`](pk_exposure.md)), and

- reporting from the ledger ([`pk_report()`](pk_report.md)).

## Author

**Maintainer**: Rainer Strotmann <rainer.strotmann@mailbox.org>
([ORCID](https://orcid.org/0000-0003-0543-7984)) \[copyright holder\]

Authors:

- Rainer Strotmann <rainer.strotmann@mailbox.org>
  ([ORCID](https://orcid.org/0000-0003-0543-7984)) \[copyright holder\]
