# Development

The repository uses ASDF systems, a Nix development shell, and a strict
documentation configuration.

## Tests and checks

From the repository root:

```sh
nix run .#test-core
nix run .#test
nix run .#coverage
nix run .#lint
nix flake check --all-systems
```

The `test-core` app runs the core-only suite without optional transport or
client subsystems. The `test` app runs the full suite for the core, client,
HTTP/2, HTTP/3, network, observability, and test systems.
Coverage produces the repository's coverage report and enforces the configured
expression and branch thresholds; it also verifies that the generated HTML
index is non-empty. Lint checks the source tree with paredit-cli. The flake
check evaluates the declared systems and checks.

Prefer cl-weave properties for byte-domain invariants and other exhaustive
wire-safe boundaries, then keep example-based tests for protocol scenarios and
diagnostics.
For HTTP/1 wire serialization boundaries, prefer table-style helper macros so
request and response fixtures stay aligned while boundary-specific assertions remain explicit.

When validating a dirty worktree, use `nix build path:.#checks.aarch64-darwin.test
--no-link --print-build-logs` so untracked source files are included. A plain
Git-backed flake source intentionally contains only tracked files.

## Documentation

The documentation source lives under docs/src and is built with MkDocs
Material:

```sh
mkdocs build --strict -f docs/mkdocs.yml
```

The MkDocs command requires MkDocs Material to be available in the developer's
environment. The current flake exposes `test-core`, `test`, `coverage`, and
`lint` apps but does not expose a docs app.

Documentation changes should keep README.md as a short entry point and put
detail in docs/src. Use one H1 per page, H2 and H3 headings, tagged code
fences, package-qualified Common Lisp examples, and relative links between
pages. Keep release history in GitHub Releases and avoid unlisted root Markdown
files.

## Source layout

| Path | Contents |
| --- | --- |
| src/ | Core HTTP model, conditions, HTTP/1.1 wire processing, transport, recording session, and observability implementation |
| client/ | Optional high-level URI, authentication, cookie, cache, proxy, redirect, retry, multipart body, Server-Sent Events, and WebSocket policies |
| http2/ | Optional HTTP/2 framing, client, and injected-stream server-session boundaries |
| http3/ | Optional HTTP/3 framing, QPACK, and injected-stream client/server-session boundaries |
| network/ | Optional native SBCL TCP, DNS, and HTTP/1 listener service |
| t/ | Tests and test-system definitions |
| docs/src/ | Published documentation pages and assets |
| flake.nix | Development shell and test-core, test, coverage, and lint apps |

## Review checklist

Before handing off a change, verify that examples match the exported API, that
limits and condition names are accurate, and that no documentation promises
socket, TLS, proxy, pooling, or retry behavior owned by an application.
