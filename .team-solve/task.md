---
system: agent-tools
test-system: agent-tools/tests
brief: ../cl-stack/docs/capabilities/agent-tools-shell.md
---

# H2c: agent-tools shell tools (0.2.0)

Implement the `shell-tools` source so that `agent-tools/tests` passes: `make-shell-tools`,
`shell-dialect`, `call-shell-tool` (bash, job_output, job_input, job_kill, job_list),
`close-shell-tools`, `shell-tools-jobs`, `find-shell-job` (src/shell-tools.lisp), the pure
helpers `shell-command-resources`, `clean-shell-output` and `truncate-shell-output`, and the
`shell-tools` methods on the ai-agent-protocol GFs (src/shell-source.lisp). Unix runs commands
on `pty-protocol`; Windows and `:executor :process` use `process-protocol`. Remove
`%not-implemented` and every call to it. You may add slots to `shell-tools` and subclass
`shell-job`, and split the code into more files under src/ (list them in agent-tools.asd).

The brief's locked decisions, result formats, failure texts, restarts and resource grammar
stay as written. The H2b file tools (src/file-tools.lisp, src/source.lisp) are not part of
this slice: do not change their behaviour. In particular, do not edit `resolve-tool-path`
(call it). Process-tree kill for the `:process` executor is process-protocol#9, out of scope.
