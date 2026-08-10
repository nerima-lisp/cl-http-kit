# cl-http-kit

cl-http-kit is a portable, binary-safe Common Lisp HTTP substrate. It
separates HTTP message semantics and wire processing from the application-owned
network boundary.

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
| cl-http-kit/client | URI, authentication, cookies, cache, proxy, redirect, retry, HTTP/1.1 connection-pool, and WebSocket client/server-session policies |
| cl-http-kit/http2 | HTTP/2 client and injected-stream server-session boundaries |
| cl-http-kit/http3 | HTTP/3 request-stream framing, static/literal/Huffman QPACK codecs with caller-owned dynamic tables, and injected-QUIC client/server-session boundaries |
| cl-http-kit/network | Optional native SBCL TCP, DNS, and HTTP/1 listener service |
| cl-http-kit/observability | Request and error counters |
| cl-http-kit/test | Internal test runner for the core and optional systems |

## Scope

The core system handles validation, serialization, parsing, bounded body
materialization, HTTP/1.x server sessions, and deterministic transport
composition. The client system adds reusable request policies and an
owner-thread HTTP/1.1 connection pool. It also includes RFC 6455 frame,
message, close-payload, HTTP upgrade, and upgraded server-session helpers in
the `http-kit/websocket` package. The optional HTTP/2 system adds client and
server sessions over injected I/O. The optional HTTP/3 system adds client and
per-request server sessions over injected QUIC streams. The optional network
system supplies native SBCL TCP, DNS, and HTTP/1 listener service; QUIC, TLS,
ALPN, and proxy negotiation remain application-owned.

See [Architecture](reference/architecture.md) for the layer boundaries and
[Compatibility](reference/compatibility.md) for protocol behavior.
