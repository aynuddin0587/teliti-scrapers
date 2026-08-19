# Teliti collector validation registry

This directory is the version-controlled operational record for collector validation and retirement decisions.

## Files

- `collector_registry.csv` — one current-state row per collector. Update an existing row when the operational state changes.
- `validation_log.csv` — append-only record of formal reconciliation events. Never delete or overwrite historical validation rows merely because a newer validation exists.

Generated reconciliation products under `teliti_reconciliation/output/` remain untracked. The registry records the reproducible validation script and a concise result summary instead of committing diagnostic outputs or raw research data.

## Update sequence

For every formal reconciliation:

1. Run the relevant reconciliation script.
2. Review its status and exceptions.
3. Append one new row to `validation_log.csv` using an absolute validation date in `YYYY-MM-DD` format.
4. Update the corresponding row in `collector_registry.csv` to reflect the latest validated state.
5. If a retirement gate has been satisfied, change the Windows task state only after the validation result is accepted.
6. Commit the reconciliation script and registry changes; keep generated reconciliation outputs untracked.

## Retirement rule

A successful GitHub workflow run alone is not sufficient evidence to retire a Windows collector. Each collector has an explicit `retirement_gate` in `collector_registry.csv`. Disable the Windows scheduled task only when that gate is satisfied. Do not delete the task definition, local collector script, or historical local archive; retain them as fallback and validation infrastructure.

## Status conventions

Validation statuses currently used:

- `PASS` — the declared reconciliation scope fully passed.
- `PASS_INITIAL` — an initial equivalence test passed, but additional independent runs are required before operational retirement.
- `PASS_PARTIAL_CATCHUP` — all completed GitHub catch-up coverage reconciles, but the historical backfill has not yet reached its terminal date for every station.
- `PASS_WITH_SOURCE_REVISION_CONTEXT` — collector equivalence is supported, but apparent row differences include demonstrated upstream source revisions and collection-timing effects.
- `REVIEW_*` — the reconciliation found a specific issue requiring investigation.
- `FAIL` — the declared reconciliation scope failed.

Operational roles currently used:

- `github_primary` — GitHub is the accepted primary collector.
- `parallel_validation` — GitHub and Windows remain in parallel while operational evidence accumulates.
- `github_catchup` — GitHub is performing a resumable historical catch-up against an existing PC archive.
- `parallel_targeted_delta_validation` — the underlying collector is validated, but a new persistence/storage design is still being tested in parallel.

Retirement states currently used:

- `retired` — Windows scheduled collection has been disabled after satisfying its gate.
- `pending` — validation is strong but one or more explicit retirement gates remain.
- `not_eligible` — the collector should not yet be considered for Windows retirement.
- `not_applicable` — no Windows retirement decision applies to this workflow.

## Evidence policy

Registry entries should record only results that are supported by a reconciliation run or an explicitly confirmed operational action. Do not convert a pending expectation into `PASS`, and do not interpret unfinished catch-up as missing-data failure when the validation design explicitly limits comparison to completed GitHub coverage.
