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
HTTP/3 SETTINGS deadline tests use a local TLS-authenticated QUIC peer. The
Nix shell and full-suite apps provide it through `HTTP_QUIC_PEER`; when running
the suite outside Nix, set that variable to the executable built from
`t/fixtures/quic-peer`. These tests exercise a delayed SETTINGS frame and
peers that omit the control stream or send only its type prefix. They use the
adapter's default deadline, not a shorter test-specific timeout. Certificates
and processes belong to each test and are cleaned up after it.
Coverage requires complete expression and branch counts and audits unexecuted
source spans in the generated HTML. Missing, duplicate, or malformed summary
counts fail the audit, as does an empty HTML index. A rounded percentage of
100 is not sufficient when the executed and total counts differ.
On SBCL 2.6.6 on ARM64 Darwin, coverage removes proven non-executable atomic
keyword defaults in top-level functions, directly returned lambdas, and
single explicit BOA structure constructors. The same projection covers exact
`(quote nil)` defaults in those keyword argument lists, including supplied-p
parameters, and non-executable NIL defaults in implicit keyword structure
constructors. Other quoted or compound defaults remain measured. Mixed argument
lists, including optional or rest parameters, remain measured because their defaults can have
executable native paths. The projection checks source bytes, reader features,
constant values, and native coverage paths before changing the reporting table.
Atomic `#.` constants are resolved without evaluating arbitrary forms; their
values are checked again before reporting. Unsupported reader forms remain
unnormalized. Function bodies, dynamic defaults, and branch records remain
measured. Other compiler versions and platforms use the unmodified coverage
table and the same gates.
Fresh-process client capability tests also contribute their native coverage on
SBCL 2.6.6 and 2.6.8 on ARM64 Darwin. The collector requires matching source,
FASL, runtime, and native coverage metadata, a successful child exit, and
unchanged result bytes before using SBCL's coverage merge. A failed validation
stops reporting rather than substituting coverage data.
The test and coverage apps isolate their compilation caches without changing
`HOME`. Lint checks the source tree with paredit-cli. The flake
checks include both suites, coverage, lint, the coverage auditor's regression
tests, and a strict documentation build. `--all-systems` evaluates both
declared platforms; running their builds requires a matching builder.

Prefer cl-weave properties for byte-domain invariants and other exhaustive
wire-safe boundaries, then keep example-based tests for protocol scenarios and
diagnostics.
For HTTP/1 wire serialization boundaries, prefer table-style helper macros so
request and response fixtures stay aligned while boundary-specific assertions remain explicit.

A Git-backed flake source contains only tracked files, so it can omit new
files in a dirty worktree. A `path:` flake source includes untracked files,
but can also copy private keys, credentials, and scratch artifacts into the
Nix store. Review and isolate the intended source before using a `path:`
build; the Nix store is not a secret store. A source filter inside a flake
does not guarantee that sensitive files were excluded from the flake input.

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
