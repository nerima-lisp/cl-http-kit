# cl-http-kit

[![License: MIT](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Documentation](https://img.shields.io/badge/docs-online-blue.svg)](https://nerima-lisp.github.io/cl-http-kit/)

cl-http-kit is a portable, binary-safe Common Lisp HTTP substrate. It
provides validated HTTP message values, HTTP/1.1 serialization and HTTP/1.x
parsing and server sessions,
deadline-aware transport callbacks, deterministic recording sessions, and
optional high-level client, HTTP/2, HTTP/3, native-network, and metrics
integrations.

## Quick Start

The recording transport is useful when the network boundary should be supplied
by an application or a test:

```lisp
(asdf:load-system "cl-http-kit")

(let* ((request
         (http-kit:make-http-request
          :method "GET"
          :uri (http-kit:make-http-uri
                :scheme "https"
                :authority "example.test"
                :path "/health")))
       (session
         (http-kit:make-recording-session
          :responses
          (list (http-kit:make-http-response
                 :status 200
                 :headers (list (http-kit:make-http-header
                                 "content-type"
                                 "text/plain"))
                 :body #(79 75))))))
  (let ((response
          (http-kit:send-recorded-http-request session request)))
    (list (http-kit:http-response-status response)
          (http-kit:http-request-summary request)
          (http-kit:http-response-summary response)
          (http-kit:http-response-body response))))
;; => (200
;;     "GET https://example.test/health headers=0 trailers=0 body-bytes=0"
;;     "HTTP/1.1 200 OK headers=1 trailers=0 body-bytes=2"
;;     #(79 75))
```

## Install

Load the core system through ASDF:

```lisp
(asdf:load-system "cl-http-kit")
```

The repository also includes a Nix development shell. See
[Getting Started](https://nerima-lisp.github.io/cl-http-kit/getting-started/)
for the package layout and transport integration boundary.

## Systems

| System | Purpose |
| --- | --- |
| cl-http-kit | HTTP message model, HTTP/1.1 wire format, HTTP/1.x server sessions, transport callbacks, deadlines, limits, conditions, and recording sessions |
| cl-http-kit/client | URI, authentication, cookies, cache, content-coding selection, ALPN protocol helpers, proxy, redirect, retry, HTTP/1.1 connection-pool, multipart body, Server-Sent Events, and WebSocket client/server-session policies |
| cl-http-kit/http2 | HTTP/2 client and injected-stream server-session boundaries |
| cl-http-kit/http3 | HTTP/3 frames, SETTINGS, QPACK static/literal/Huffman codecs with caller-owned dynamic tables, and injected-QUIC client/request-stream server-session boundaries |
| cl-http-kit/network | Optional SBCL TCP/DNS stream opener, IPv4/IPv6 listener, and HTTP/1 listener service |
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
name helpers while leaving compressor implementations, dialing, and TLS/ALPN
negotiation to the application. The optional `cl-http-kit/http2` system supplies
both a client and a server session over caller-provided binary I/O. The
optional `cl-http-kit/http3` system supplies client and per-request server
stream framing with static/literal/Huffman QPACK and caller-owned dynamic-table
references over caller-provided QUIC streams;
QUIC packets, loss recovery, congestion control, TLS 1.3, ALPN, sockets, and
native HTTP/3 connection/server acceptance remain outside that boundary. The
optional `cl-http-kit/network` system supplies native TCP, DNS, and an HTTP/1
listener service on SBCL; TLS, ALPN, and application-specific proxy
negotiation remain callback policies. The optional client system also supplies
an owner-thread HTTP/1.1 connection pool and RFC 6455 frame, message,
close-payload, HTTP upgrade, client-side 101 handshake, and upgraded server
session helpers. WebSocket key generation, dialing, TLS/ALPN, extension
negotiation, and masking-key generation remain application-owned; the server
session handles Ping/Pong and the close handshake it owns.

## Support

Use the [GitHub issue tracker](https://github.com/nerima-lisp/cl-http-kit/issues)
for reproducible bugs, documentation corrections, and feature discussions.

## Contributing

Read the [development guide](https://nerima-lisp.github.io/cl-http-kit/project/development/)
before proposing changes. Bug reports and focused patches are welcome.

## License

cl-http-kit is released under the [MIT License](LICENSE).
