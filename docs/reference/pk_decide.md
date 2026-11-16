# Record a decision on a run

Accepts or rejects a run, or sets it back to pending. A reason is
mandatory for `"accepted"` and `"rejected"`, because the rationale for
model development decisions is part of the audit trail. The decision is
stored in the ledger and appended to the event log (`events.csv`), so
earlier decisions stay visible with [`pk_audit()`](pk_runs.md).

## Usage

``` r
pk_decide(
  run,
  decision = c("accepted", "rejected", "pending"),
  reason = NA_character_,
  quiet = FALSE,
  proj = NULL
)
```

## Arguments

- run:

  A run reference or a run id.

- decision:

  `"accepted"`, `"rejected"` or `"pending"`.

- reason:

  Free-text rationale (required unless `decision = "pending"`).

- quiet:

  Suppress the confirmation message?

- proj:

  A `pk_project` object. Defaults to the current project.

## Value

The run reference (invisibly).
