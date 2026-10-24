# cl-http-kit

cl-http-kit is a portable, binary-safe Common Lisp HTTP substrate. It
separates HTTP message semantics and wire processing from the application-owned
network boundary.

The checked-in systems are version 0.4.1. The integration includes native
default client wiring, dependency declarations for cl-crypto-kit,
cl-deflate-kit, cl-tls-kit, and cl-quic-kit, and a URL-and-method client entry
point. The optional cl-quic-kit adapter supports native
IPv4 HTTP/3 client connections. Native QUIC server acceptance is not provided;
HTTP/3 server sessions use injected streams. Read the
[migration guide](project/migration.md) for the 0.4.0 integration and planned
0.5.0 migrations, and the
[changelog](project/changelog.md) for the release scope.

## Start here

- [Getting Started](getting-started.md) introduces loading, recording
  sessions, and transport callbacks.
- [Core Concepts](guide/core-concepts.md) explains message values, octets,
  deadlines, limits, and callback styles.
- [API Reference](reference/api.md) documents the primary public symbols of
  each registered runtime system; package definitions remain authoritative for
  lower-level exports.

## Systems

The repository currently documents these ASDF systems:

| System | Responsibility |
| --- | --- |
| cl-http-kit | HTTP messages, HTTP/1.1, HTTP/1.x server sessions, transport callbacks, limits, deadlines, conditions, and recording sessions |
| cl-http-kit/client | URI, authentication, cookies, cache, content-coding selection, ALPN protocol helpers, proxy, redirect, retry, and HTTP/1.1 connection-pool |
| cl-http-kit/http2 | HTTP/2 client and injected-stream server-session boundaries |
| cl-http-kit/http3 | HTTP/3 request-stream framing, static/literal/Huffman QPACK codecs with caller-owned dynamic tables, injected-QUIC client/server-session boundaries, and an optional cl-quic-kit client adapter |
| cl-http-kit/network | Optional native SBCL TCP, DNS, and HTTP/1 listener service |
| cl-http-kit/tls | Native TLS 1.3 client stream wrapper; server wrapping is unsupported because cl-tls-kit has no server driver |
| cl-http-kit/observability | Request and error counters |
| cl-http-kit/test-core | Internal core-only test runner without optional subsystems |
| cl-http-kit/test | Internal test runner for the core and optional systems |

## Scope

The core system handles validation, serialization, parsing, bounded body
materialization, HTTP/1.x server sessions, and deterministic transport
composition. The client system adds reusable request policies, cookie
partitioning and SameSite request context, content-coding and ALPN helper
APIs, cache request-directive handling, and a thread-safe HTTP/1.1
connection pool. The optional HTTP/2 system adds client and
server sessions over injected I/O. The optional HTTP/3 system adds client and
per-request server sessions over injected QUIC streams. Its optional cl-quic-kit
client adapter drives the client QUIC connection and HTTP/3 stream lifecycle
when explicitly supplied. The optional network
system supplies native SBCL TCP, DNS, and HTTP/1 listener service. The
optional TLS system supplies the native TLS 1.3 client wrapper; server-side TLS
is not supported because the current cl-tls-kit integration does not expose a
server driver. For HTTPS servers, terminate TLS in a reverse proxy or load
balancer and forward HTTP to the listener. The HTTP/3 adapter delegates QUIC
and native IPv4 UDP sockets to cl-quic-kit. QUIC/TLS options, certificate
policy, and shutdown policy remain caller-owned; native QUIC server accept
loops and proxy negotiation are not provided by this adapter.

See [Architecture](reference/architecture.md) for the layer boundaries and
[Compatibility](reference/compatibility.md) for protocol behavior.
