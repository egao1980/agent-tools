---
system: agent-tools
test-system: agent-tools/tests
brief: /Users/nikolaimatiushev/Projects/cl-workspace/cl-stack/docs/capabilities/agent-tools-units.md
---

# H8c run 2 of 2: unit-tools + shell jobs as units (agent-tools 0.3.0)

Depends on run 1: start only after egao1980/supervisor-protocol#3 is merged and
supervisor-protocol 0.3.0 (with `supervisor-protocol/search`) is published to
GHCR; the verdict resolves it from there.

Check out `agent/h8c-contract-238f` first (draft PR egao1980/agent-tools#5).
Implement the brief (`agent-tools-units.md`). Replace every call of
`%h8c-pending` (`src/unit-source.lisp`, `src/unit-run.lisp`), then delete
`%h8c-pending` (`src/unit-tools.lisp`).

Scope:

- `%run-unit-tool`: `unit_list`, `unit_search`, `unit_read`, `unit_tail`,
  `unit_control` with the exact output formats, caps (`max-output` counts
  everything), errors (`unit-not-found`, `tool-input-error`,
  `tool-unavailable`) and visibility (the tools' scope; previous sessions'
  logs when `unit-tools-previous`, read-only) in the brief's "Tools" section.
- `parse-time-argument` (RFC 3339 with fraction / offset / space, or
  `<n>s|m|h|d` ago).
- `%unit-permission-resources` (never signals).
- Shell tools under a supervisor (`%call-supervised-shell-tool`,
  `%close-supervised-shell-tools`): every bash command a `:shell-job` unit in
  `shell-tools-scope` with labels (:tool "bash"); foreground EOF via
  `close-unit-input`; H2c texts for bash and the job tools with unit ids as job
  ids; job output from the unit log since a per-job cursor; the supervised
  truncation line; `shell-timeout` restarts; `shell-tools-jobs` /
  `find-shell-job` with `shell-job-unit`; close = close the scope.

Given (do not change the shape): exports, constants, `unit-tools` class and
readers (add slots as needed), `make-unit-tools`, `call-unit-tool` /
`close-unit-tools`, descriptors and schemas, approval and invocation methods,
the `make-shell-tools` :supervisor / :scope keys and checks.

Use only supervisor-protocol's public API (`sup:` / `ss:`). Add no dependencies.

Verdict: `agent-tools/tests` on macOS, Linux and Windows: the H2b / H2c suites
keep passing; the 59 new `unit-*` cases pass (one skips on Windows).
