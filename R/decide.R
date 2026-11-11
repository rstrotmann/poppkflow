#' Record a decision on a run
#'
#' Accepts or rejects a run, or sets it back to pending. A reason is mandatory
#' for `"accepted"` and `"rejected"`, because the rationale for model
#' development decisions is part of the audit trail. The decision is stored in
#' the ledger and appended to the event log (`events.csv`), so earlier
#' decisions stay visible with [pk_audit()].
#'
#' @param run A run reference or a run id.
#' @param decision `"accepted"`, `"rejected"` or `"pending"`.
#' @param reason Free-text rationale (required unless `decision = "pending"`).
#' @param quiet Suppress the confirmation message?
#' @inheritParams pk_project
#' @return The run reference (invisibly).
#' @export
pk_decide <- function(run, decision = c("accepted", "rejected", "pending"), reason = NA_character_, quiet = FALSE, proj = NULL) {
  decision <- match.arg(decision)
  r <- resolve_run(run, proj)
  if (decision != "pending" && !is_string(reason)) {
    cli::cli_abort("A {.arg reason} is required to record the decision {.val {decision}}.")
  }
  row <- ledger_get(r$proj, r$run_id)
  if (!identical(row$status, "completed")) {
    cli::cli_abort("Only completed runs can be accepted or rejected (status of {.val {r$run_id}}: {.val {row$status}}).")
  }
  ledger_upsert(r$proj, list(
    run_id = r$run_id, decision = decision, decision_reason = reason,
    decision_by = pk_user(), decision_time = pk_now()
  ))
  event_append(r$proj, r$run_id, paste0("decision:", decision), if (is.na(reason)) "" else reason)
  if (!quiet) cli::cli_alert_success("{.val {r$run_id}} {decision}")
  invisible(new_run_ref(r$proj, r$run_id))
}
