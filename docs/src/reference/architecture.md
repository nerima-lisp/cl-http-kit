# Architecture

cl-http-kit is organized around a small protocol core and explicit integration
boundaries. The registered ASDF systems share the HTTP-KIT message model where
appropriate, while application-owned I/O remains outside the package.

## Core layers

| Layer | Responsibility |
| --- | --- |
| URI and headers | Validate URI components, header names, field values, and lookup semantics |
| Message model | Construct and copy request and response values |
| HTTP/1.1 wire format | Serialize requests and parse response framing, trailers, and bounded bodies |
| HTTP/1.1 server session | Parse requests, enforce limits, and serialize responses on an open stream |
| Control | Carry deadlines, timeouts, byte limits, and structured conditions |
| Transport boundary | Open and close an injected binary stream |
| Recording transport | Replay supplied responses and retain request history |

The source follows these layers: package definitions and conditions establish
the public vocabulary; URI, header, and model files define values; serializer
and parser files implement HTTP/1.1; transport and recording files connect the
wire layer to an application.

## HTTP/1.1 boundary

The request serializer returns an octet vector for an HTTP/1.1 request. It
derives Host from the URI and validates body framing before returning. The
transport boundary is responsible for writing those bytes to a binary stream.
The response parser accepts an octet vector or binary stream and handles
informational responses, fixed-length bodies, chunked bodies and trailers,
close-delimited bodies, and no-body status rules.

The stream callbacks are deliberately narrow. The open callback receives the
request and the effective timeout and deadline; the close callback receives the
stream. The surrounding application controls how a stream is created and
whether it is pooled or discarded.

The core also exposes serve-http1-session for an already-open binary stream.
It parses HTTP/1.0 and HTTP/1.1 requests, supports bounded request bodies,
trailers, Expect: 100-continue, protocol upgrades, response streams, and
request limits. The caller still owns the listener, TLS, protocol selection,
and connection lifecycle.

## Optional HTTP/2 system

The cl-http-kit/http2 system accepts either an exchange callback or an
open-stream callback. Its client is intentionally a small boundary: the
application supplies connection setup and the HTTP/2 transport implementation
needed by its environment.

The connection object owns stream identifiers, peer settings, flow-control
windows, GOAWAY/draining state, and request/response event processing. The
optional connection manager reuses connections by origin and applies an LRU
connection cap. The application still supplies the binary stream or exchange,
socket, TLS, ALPN, proxy negotiation, and reconnect policy. Frame size and
header and body limits are validated at construction or request time, and
unsupported features are reported as structured conditions.

The same package exposes serve-http2-session for a caller-supplied binary
stream. It owns the HTTP/2 preface, SETTINGS, stream state, HPACK, flow
control, request-body collection, and response framing for the session, while
the caller owns socket setup, TLS, ALPN, and connection acceptance.

## Optional HTTP/3 system

The cl-http-kit/http3 system provides a client-side HTTP/3 framing boundary
and serve-http3-request-stream for one caller-supplied request stream. It
opens the local control stream, writes SETTINGS, encodes request headers and
DATA, and parses or produces response headers, trailers, DATA, and
content-length. QPACK uses the RFC static table, literal and Huffman
representations, and caller-owned dynamic tables. The system exposes dynamic
table insertion plus encoder- and decoder-stream codecs, but does not
synchronize those instruction streams automatically; callers must keep the
peer tables synchronized before using dynamic references.

The HTTP/3 system does not implement QUIC packet handling, TLS, ALPN, native
socket setup, peer unidirectional-stream acceptance, connection-level stream
dispatch, or a native HTTP/3 listener. The surrounding QUIC implementation
must provide those boundaries.

The implementation is split by responsibility: `transport.lisp` owns shared
control-stream, frame, settings, and header helpers; `transport-client.lisp`
owns response parsing and client request execution; and
`transport-server.lisp` owns request-stream parsing and response production.
The client entry points also expose CPS variants so an application can keep
I/O scheduling and error continuation policy outside the protocol code.

## Optional high-level client system

The cl-http-kit/client system consumes core request and response values and
composes URI resolution, authentication, cookies, cache, proxy, redirect,
retry, and HTTP/1.1 connection-pool policies. Its `http-client-send` operation
delegates actual I/O to an injected transport function, stream callbacks, or a
callback-driven connection pool. Socket creation, DNS, TLS, and ALPN remain
outside this system.

## Optional native network system

The `cl-http-kit/network` system supplies an SBCL-native TCP stream opener and
IPv4/IPv6 DNS resolution. It is an endpoint boundary for applications that
want a built-in socket implementation; TLS, ALPN, and proxy negotiation remain
explicit policies supplied by the surrounding application.

The network system also provides TCP listener primitives. Its
serve-http1-listener combines accept, HTTP/1 session dispatch, connection
limits, and error callbacks for a caller-owned listener. TLS, ALPN, proxy
negotiation, and HTTP/2 or HTTP/3 protocol selection remain explicit policies
supplied by the surrounding application.

## Optional observability system

The cl-http-kit/observability system adapts request completion and errors to
cl-observability-kit counters. It records request totals by method and outcome,
and error totals by method and condition kind. Metrics are injected into the
operation wrapper rather than coupled to the HTTP/1.1 parser.

## Ownership boundary

The library owns protocol values, wire correctness, and the policies explicitly
provided by the optional client system. The optional network system can own
native TCP and DNS setup on SBCL; applications still own TLS, ALPN, pool
synchronization, and policy about sensitive logging. This boundary keeps the
core portable and makes deterministic testing possible.
