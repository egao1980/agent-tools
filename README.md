# agent-tools

The coding agent's tools as [`ai-agent-protocol`](https://github.com/egao1980/ai-agent-protocol) tool sources.

- `file-tools`: `read`, `write`, `edit`, `multi_edit`, `glob`, `grep` and `ls`, as one [`ai-agent-protocol`](https://github.com/egao1980/ai-agent-protocol) tool source over any [`cl-stack-pathlib`](https://github.com/egao1980/cl-stack-pathlib) filesystem (local, memory, zip).

- `shell-tools` (**H2c, contract, 0.2.0**): `bash` with timeouts and output caps, background jobs (`job_output`, `job_input`, `job_kill`, `job_list`), and a shell command scanner that feeds permission resources. Commands run on `pty-protocol` on Unix and `process-protocol` on Windows.

Briefs: [`agent-tools.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/agent-tools.md) (H2b), [`agent-tools-shell.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/agent-tools-shell.md) (H2c). Plan: Track H, milestone H2.

Version **0.2.0** (in progress). 0.1.1 is milestone H2b: `read`, `write`, `edit`, `multi_edit`, `glob`, `grep` and `ls` as an ai-agent-protocol tool source over any cl-stack-pathlib filesystem, confined to a root, with compare-and-swap writes (`expected_hash`), unified-diff edit previews, gitignore-aware listings and `path:` permission resources. 0.2.0 adds the H2c shell tools; until that slice is implemented, every `shell-tools` function signals "not implemented".

```lisp
(asdf:load-system "agent-tools")

(let ((tools (agent-tools:make-file-tools :root #p"/path/to/project/")))
  ;; As a tool source on an agent:
  (ai-agent-protocol:make-ai-agent :tools (list tools) :backend backend)
  ;; Or called directly:
  (agent-tools:call-file-tool tools "read" "{\"file_path\": \"src/a.lisp\", \"limit\": 50}"))
```

| Tool | Arguments (required in bold) |
|------|------------------------------|
| `read` | **`file_path`**, `offset` (1-based line), `limit` (lines) |
| `write` | **`file_path`**, **`content`**, `expected_hash` |
| `edit` | **`file_path`**, **`old_string`**, **`new_string`**, `replace_all`, `expected_hash` |
| `multi_edit` | **`file_path`**, **`edits`** (`[{old_string, new_string, replace_all}]`), `expected_hash` |
| `glob` | **`pattern`**, `path` |
| `grep` | **`pattern`**, `path`, `include`, `regex`, `case_insensitive`, `max_results` |
| `ls` | `path` |

Paths resolve under the root; anything outside it is a `path-outside-root` tool input error. A leading `\` (UNC) or drive letter (`C:`) makes a path absolute, and a segment below the root holding `:` or NUL is refused on every OS. Reads return a `hash: sha256:…` line; pass it back as `expected_hash` for a compare-and-swap write. Edits always write with compare-and-swap against the content they read, and return a unified diff. Permission resources are `path:<relative>` strings.

## License

MIT — see [LICENSE](LICENSE).
