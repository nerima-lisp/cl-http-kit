# Compatibility

cl-http-kit focuses on reusable HTTP/1.x client and server framing with
optional HTTP/2 connection/session support, HTTP/3 request-stream framing,
WebSocket sessions, and native TCP/DNS integration. This page describes the
behavior implemented by the registered systems; it is not a claim of complete
RFC coverage.

## HTTP/1.1

The core serializer returns an octet vector containing HTTP/1.1 request lines
and headers. The application transport writes that vector to a binary stream.
The serializer validates method and header syntax, derives Host from the URI,
checks Content-Length against the body, and rejects request transfer encoding
that the core cannot safely construct.

The response parser supports:

- informational responses, except for the unsupported protocol switch status;
- fixed-length bodies with exact byte accounting;
- chunked bodies and trailers;
- close-delimited bodies;
- no-body rules for HEAD responses and the relevant status codes;
- header and body byte limits.

Conflicting transfer framing, malformed chunk data, invalid status lines, and
truncated fixed-length bodies raise public conditions.

serve-http1-session serves requests on an already-open binary stream. It
supports request-body limits and streaming callbacks, Expect: 100-continue,
trailers, response streams, protocol upgrades, and bounded request counts.
The listener and any TLS or protocol-selection layer remain application-owned,
except for the optional native HTTP/1 listener service.

## HTTP/2

The optional cl-http-kit/http2 system accepts an injected frame exchange or
binary stream. It validates frame size and header and body limits, then exposes
the same request and response model as the core system where the boundary
supports it.

The HTTP/2 connection API supports reusable, owner-thread connections,
stream-id allocation, request multiplexing within a batch, flow-controlled
request and response bodies, GOAWAY/draining state, and an optional
`http2-connection-manager` with origin-keyed reuse and an LRU connection cap.
The manager is cooperative and callback-driven: the application still
provides the stream, socket, TLS, ALPN, proxy negotiation, and retry policy.
The peer must send a non-ACK `SETTINGS` frame first; server push and protocol
switching are unsupported. Unknown extension frames are ignored as permitted
by HTTP/2. Unsupported features are reported with
`HTTP-UNSUPPORTED-FEATURE`.

serve-http2-session provides the corresponding server-side session boundary
over a caller-supplied binary stream. It handles the HTTP/2 preface, SETTINGS,
HPACK, stream state, flow control, request bodies, response framing, and
session limits; it does not accept sockets or provide TLS/ALPN negotiation.

## HTTP/3

The optional cl-http-kit/http3 system implements HTTP/3 control and
request-stream framing over callbacks supplied by a QUIC implementation. It
supports a client and serve-http3-request-stream for one injected server
request stream, including request/response headers and trailers, DATA frames,
content-length checks, static and literal QPACK field sections, HPACK Huffman
strings, and caller-owned QPACK dynamic-table references.

The boundary does not provide QUIC packets, loss recovery, congestion control,
TLS 1.3, ALPN, native sockets, peer unidirectional-stream acceptance,
connection-level stream dispatch, automatic QPACK instruction-stream
synchronization, or a native HTTP/3 server. Those capabilities must be
supplied by the surrounding transport.
Invalid or deliberately unsupported protocol features raise public protocol or
unsupported-feature conditions.

## High-level client

The optional `cl-http-kit/client` system supplies reusable URI, authentication,
cookie, cache, proxy, redirect, retry, and HTTP/1.1 connection-pool policies
around core messages. The pool is an owner-thread, callback-driven boundary;
applications still provide the transport function or stream callbacks. The
optional network system can provide native TCP and DNS setup on SBCL, while
TLS and ALPN remain application-owned.

Client transport functions use one canonical keyword contract: they receive
the timeout, deadline, size limits, proxy context, request-body producer,
response callbacks, and `collect-body-p` keyword arguments described by the
client API. Older transports that accept only a subset of these keywords are
not supported; define the complete boundary or use the provided stream or
connection-pool constructors.

## WebSocket

The `http-kit/websocket` package is loaded by `cl-http-kit/client`. It provides
RFC 6455 frame serialization and parsing, masking and payload-size validation,
fragmented-message assembly, close-code and UTF-8 reason handling, the
`Sec-WebSocket-Accept` handshake calculation, and validated HTTP/1.1 upgrade
request/response helpers. `make-websocket-upgrade-request` and
`websocket-client-handshake` also cover the client-side HTTP 101 exchange on
an already-open stream and leave that stream available for frame I/O. The
frame API exposes masking explicitly: callers
must supply masking keys or a per-frame key function when producing masked
frames. It does not open sockets, perform TLS or ALPN, or negotiate
extensions. Client handshake key generation and connection lifecycle remain
application-owned.

serve-websocket-session serves an already-upgraded stream: it requires masked
client frames by default, automatically answers Ping frames, echoes a valid
peer Close frame, dispatches complete messages to a handler, and closes with
an appropriate protocol or size code on errors. It does not open sockets,
perform TLS or ALPN, negotiate extensions, or generate client handshake keys.

## Native network boundary

`cl-http-kit/network` provides a native SBCL TCP stream opener with IPv4/IPv6
DNS resolution and deadline-aware connect/read operations. It also provides
an IPv4/IPv6 listening socket boundary: `open-http-tcp-listener` binds and
listens, `accept-http-tcp-stream` accepts one binary stream and returns peer
address metadata, and `close-http-tcp-listener` closes the listener. It
intentionally does not implement TLS, ALPN, or proxy negotiation, so those
concerns remain replaceable callbacks at the application boundary.

serve-http1-listener combines accept and serve-http1-session dispatch for a
caller-owned listener, with connection limits and accept/error callbacks.

## Application-owned behavior

The portable core does not choose a socket library, DNS resolver, or TLS
implementation. Applications can adapt those concerns to their deployment,
including the optional SBCL network boundary, and can supply callbacks to the
library's pool without changing the message and wire layers. Proxy negotiation,
character encoding, and logging policy remain application-owned.

## Portability

The core is written in portable Common Lisp and uses binary streams and
injected callbacks rather than a platform-specific socket API. The repository's
Nix flake currently declares Darwin ARM64 and Linux x86-64 development targets;
the HTTP model itself does not depend on those operating-system details.
