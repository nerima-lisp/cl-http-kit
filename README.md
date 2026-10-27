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

The checked-in ASDF systems remain at version 0.4.1 while the 0.5.0 changes
are pending. `make-http-client` creates a native TCP/TLS connection pool when
no transport boundary is supplied, while `http-client-request` and
`http-client-send` remain supported request paths. `http-client-send` also
accepts a method and URL convenience form. See the
[changelog](docs/src/project/changelog.md) and [migration guide](docs/src/project/migration.md)
for the 0.4.0 and 0.5.0 compatibility boundaries.

The client uses cl-deflate-kit for compression, and the client and HTTPS
listener use cl-tls-kit 0.5.1 or later for TLS 1.3.
TLS certificate-chain, hostname, and trust verification is required by
default. `:verify :optional` has the same verification policy as `:required`;
`:verify nil` is rejected because the pinned cl-tls-kit client driver does not
provide a supported certificate-verification bypass. Use a trusted test
certificate when exercising HTTPS clients.

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
QUIC callbacks are injected.

## Systems

| System | Purpose |
| --- | --- |
| cl-http-kit | HTTP message model, HTTP/1.1 wire format, HTTP/1.x server sessions, transport callbacks, deadlines, limits, conditions, and recording sessions |
| cl-http-kit/client | URI, authentication, cookies, RFC 6797 HSTS, RFC 7838 Alt-Svc discovery, RFC 9111 cache with RFC 5861 `stale-if-error`, proxy, redirect, retry, gzip/deflate response decoding, HTTP/1.1 connection-pool, multipart body |
| cl-http-kit/http2 | HTTP/2 client and injected-stream server-session boundaries |
| cl-http-kit/http3 | HTTP/3 frames, SETTINGS, QPACK static/literal/Huffman codecs with caller-owned dynamic tables, and injected-QUIC client/request-stream server-session boundaries |
| cl-http-kit/network | Optional SBCL TCP/DNS stream opener, IPv4/IPv6 listener, and HTTP/1 listener service |
| cl-http-kit/tls | Native TLS 1.3 client wrapper and HTTPS HTTP/1.1/HTTP/2 server backed by cl-tls-kit |
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

## Scope

The library owns HTTP message validation, HTTP/1.1 framing, HTTP/1.x request
and response sessions, bounded body handling, high-level request policies, and
small composition boundaries. The optional client system adds cookie-jar
partitioning and SameSite request context, content-coding adapter selection,
cache request-directive handling with 304 metadata refresh, and ALPN protocol
name helpers. The current client integration can supply native dialing, TLS,
proxy selection, content coding, and a locked pool when its default boundary is
used. The optional
`cl-http-kit/http2` system supplies both a client and a
server session over caller-provided binary I/O. The optional
`cl-http-kit/http3` system supplies client and per-request server stream
framing with static/literal/Huffman QPACK and caller-owned dynamic-table
references over caller-provided QUIC streams. Its optional cl-quic-kit adapter
can drive the client QUIC connection and stream lifecycle when explicitly
supplied. QUIC packets, loss recovery, congestion control, TLS 1.3, ALPN,
sockets, certificate policy, and native HTTP/3 server acceptance remain
outside the framing boundary. The optional
`cl-http-kit/network` system supplies native TCP, DNS, and an HTTP/1 listener
service on SBCL. The optional `cl-http-kit/tls` system adds an HTTPS listener
that negotiates HTTP/2 or HTTP/1.1 with ALPN and uses the shared handler
contract `(request) -> (values response informational-responses)`. It bounds
concurrent connections and handshakes, HTTP/2 streams, request sizes, aggregate
buffered request bodies, and the time spent in handshakes, requests, idle waits,
and shutdown.
Applications can pass a store created with
`cl-tls-kit:make-tls13-server-ticket-key-store` to multiple server instances to
share TLS resumption keys. See the
[HTTPS server API](docs/src/reference/api.md#https-server). The listener
supports HTTPS only; HTTP/3 server acceptance remains outside this boundary.
The default origin client offers
`h2` and `http/1.1` through ALPN. TLS to an HTTPS proxy offers only
`http/1.1`; custom transports supply their own ALPN policy. The optional
client system also supplies a thread-safe HTTP/1.1 connection pool.

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
