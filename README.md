# cl-http-kit

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-online-blue.svg)](https://nerima-lisp.github.io/cl-http-kit/)

cl-http-kit is a portable, binary-safe Common Lisp HTTP substrate. It
provides validated HTTP message values, HTTP/1.1 serialization and HTTP/1.x
parsing and server sessions,
deadline-aware transport callbacks, deterministic recording sessions, and
optional high-level client, HTTP/2, HTTP/3, native-network, and metrics
integrations.

## Release status

The checked-in ASDF systems are version 0.4.1. `make-http-client` creates a
native TCP/TLS connection pool when no transport boundary is supplied, while
`http-client-request` and `http-client-send` remain supported request paths.
`http-client-send` also accepts a method and URL convenience form. See the
[changelog](docs/src/project/changelog.md) and
[migration guide](docs/src/project/migration.md) for the 0.4.0 integration and
planned API migrations.

The client uses cl-deflate-kit for compression and cl-tls-kit 0.1.2 or later
for TLS 1.3.
TLS certificate-chain, hostname, and trust verification is required by
default. `:verify :optional` has the same verification policy as `:required`;
`:verify nil` skips those certificate-policy checks but still requires the
peer's CertificateVerify signature. Use `nil` only for deliberately
unauthenticated test connections. Server TLS remains outside the current kit
boundary.

## Quick Start

After following the [ASDF setup and transport installation guide](docs/src/getting-started.md#connect-a-real-transport),
make a real HTTPS request with the native client:

```lisp
(let* ((client (http-kit/client:make-http-client))
       (response (http-kit/client:http-client-send
                  client "GET" "https://example.com/")))
  (http-kit:http-response-status response))
```

## Install

See [Getting Started](docs/src/getting-started.md#load-the-core-system) for
ASDF loading, the recording example, and optional system setup. The native
TCP/DNS implementation requires SBCL. Loading `cl-http-kit/http3` also requires
SBCL because its cl-quic-kit dependency loads the UDP implementation, even when
QUIC callbacks are injected. The repository also includes a Nix development
shell.

## Systems

| System | Purpose |
| --- | --- |
| cl-http-kit | HTTP message model, HTTP/1.1 wire format, HTTP/1.x server sessions, transport callbacks, deadlines, limits, conditions, and recording sessions |
| cl-http-kit/client | URI, authentication, cookies, RFC 6797 HSTS, RFC 7838 Alt-Svc discovery, RFC 9111 cache with RFC 5861 `stale-if-error`, proxy, redirect, retry, gzip/deflate response decoding, HTTP/1.1 connection-pool, multipart body |
| cl-http-kit/http2 | HTTP/2 client and injected-stream server-session boundaries |
| cl-http-kit/http3 | HTTP/3 frames, SETTINGS, QPACK static/literal/Huffman codecs with caller-owned dynamic tables, and injected-QUIC client/request-stream server-session boundaries |
| cl-http-kit/network | Optional SBCL TCP/DNS stream opener, IPv4/IPv6 listener, and HTTP/1 listener service |
| cl-http-kit/tls | Native TLS 1.3 client stream wrapper backed by cl-tls-kit; server wrapping remains an explicit unsupported boundary until the kit exposes a server driver |
| cl-http-kit/observability | Request and error counters backed by cl-observability-kit |
| cl-http-kit/test-core | Internal core-only test runner without optional subsystems |
| cl-http-kit/test | Internal test runner for the core and optional systems |

## Documentation

- [Documentation home](https://nerima-lisp.github.io/cl-http-kit/)
- [Getting Started](https://nerima-lisp.github.io/cl-http-kit/getting-started/)
- [Core Concepts](https://nerima-lisp.github.io/cl-http-kit/guide/core-concepts/)
- [API reference](https://nerima-lisp.github.io/cl-http-kit/reference/api/)
- [Architecture](https://nerima-lisp.github.io/cl-http-kit/reference/architecture/)
- [Conditions](https://nerima-lisp.github.io/cl-http-kit/reference/conditions/)
- [Compatibility](https://nerima-lisp.github.io/cl-http-kit/reference/compatibility/)
- [Migration guide](docs/src/project/migration.md)
- [Changelog](docs/src/project/changelog.md)

## Development

```sh
nix develop
nix run .#test-core
nix run .#test
nix run .#coverage
nix run .#lint
nix flake check --all-systems
```

`.#test-core` is the deterministic gate for the core-only suite, while
`.#test` runs the full registered test system. `.#lint` uses paredit-cli's
structural Lisp inspection, while `.#coverage` runs cl-weave with expression
and branch thresholds and writes the report below `coverage/`. Coverage
instrumentation can be more expensive than the test run; set
`CL_HTTP_KIT_COVERAGE_TIMEOUT_SECONDS` when running it in a slower
environment.

The documentation build is described in
[Project Development](https://nerima-lisp.github.io/cl-http-kit/project/development/).

## HTTP/3 fallback

When a client supplies `:http3-transport-function`, an advertised
Alt-Svc `h3` alternative is attempted before TCP. TCP fallback requires an
`http-connection-error` whose `http-connection-error-request-sent-p` reader is
false, and no request-body producer or response callback may have run.
Other failures are propagated. An explicit HTTP/3 request is a hard
requirement and does not fall back.

## Support

Use the [GitHub issue tracker](https://github.com/nerima-lisp/cl-http-kit/issues)
for reproducible bugs, documentation corrections, and feature discussions.

## Contributing

Read the [development guide](https://nerima-lisp.github.io/cl-http-kit/project/development/)
before proposing changes. Bug reports and focused patches are welcome.

## License

cl-http-kit is released under the [MIT License](LICENSE).
