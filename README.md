# agent-tools

The coding agent's file tools — `read`, `write`, `edit`, `multi_edit`, `glob`, `grep`, `ls` — as one [`ai-agent-protocol`](https://github.com/egao1980/ai-agent-protocol) tool source over any [`cl-stack-pathlib`](https://github.com/egao1980/cl-stack-pathlib) filesystem (local, memory, zip).

Brief: [`cl-stack/docs/capabilities/agent-tools.md`](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/agent-tools.md). Plan: Track H, milestone H2b. Shell and pty tools are H2c.

Version **0.1.1** — milestone H2b: `read`, `write`, `edit`, `multi_edit`, `glob`, `grep` and `ls` as an ai-agent-protocol tool source over any cl-stack-pathlib filesystem, confined to a root, with compare-and-swap writes (`expected_hash`), unified-diff edit previews, gitignore-aware listings and `path:` permission resources. Shell tools are milestone H2c.

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
