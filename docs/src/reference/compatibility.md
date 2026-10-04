# Compatibility

cl-http-kit focuses on reusable HTTP/1.x client and server framing with
optional HTTP/2 connection/session support, HTTP/3 request-stream framing,
WebSocket sessions, and native TCP/DNS integration. This page describes the
behavior implemented by the registered systems; it is not a claim of complete
RFC coverage.

The registered systems in this checkout are version 0.4.0. The current
integration source already has native default client wiring, proxy-environment
parsing, lock-backed pool/cookie state, and four-kit dependency declarations.
The URL-and-method convenience form is available through `http-client-send`.
The TLS wrapper uses cl-tls-kit for the native TLS 1.3 client path; server
wrapping remains an explicit unsupported boundary because the kit does not
expose a server driver.

## HTTP/1.1

The core serializer returns an octet vector containing HTTP/1.1 request lines
and headers. The application transport writes that vector to a binary stream.
The serializer validates method and header syntax, derives Host from the URI,
checks Content-Length against the body, and rejects request transfer encoding
that the core cannot safely construct.

The response parser supports:

- informational responses and the terminal 101 protocol-switch response,
  leaving upgraded bytes unread;
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
`http2-connection-manager` with origin-keyed reuse, an LRU connection cap, and
optional idle expiry and maximum connection lifetime. The manager can retry once on a new connection when a
peer explicitly reports that an unstreamed request was not processed through
GOAWAY or `REFUSED_STREAM`; body producers are never replayed automatically.
The manager is cooperative and callback-driven: the application still
provides the stream, socket, TLS, ALPN, proxy negotiation, and broader retry
policy.
The peer must send a non-ACK `SETTINGS` frame first. RFC 8441 Extended CONNECT
is supported when the peer advertises `SETTINGS_ENABLE_CONNECT_PROTOCOL`;
HTTP/1.1-style `101 Switching Protocols` is not valid on HTTP/2. The client
advertises `SETTINGS_ENABLE_PUSH=0`; a subsequent `PUSH_PROMISE` is rejected as
`PROTOCOL_ERROR`. Unknown extension frames are ignored as permitted by HTTP/2.
Unsupported features are reported with
`HTTP-UNSUPPORTED-FEATURE`.

serve-http2-session provides the corresponding server-side session boundary
over a caller-supplied binary stream. It handles the HTTP/2 preface, SETTINGS,
HPACK, stream state, flow control, request bodies, response framing, and
session limits; it does not accept sockets or provide TLS/ALPN negotiation.
HPACK Huffman strings use the RFC 7541 Appendix B code table for encoding and
decoding.

## HTTP/3

The optional cl-http-kit/http3 system implements HTTP/3 control and
request-stream framing over callbacks supplied by a QUIC implementation. It
supports a client and serve-http3-request-stream for one injected server
request stream, including request/response headers and trailers, DATA frames,
content-length checks, static and literal QPACK field sections, HPACK Huffman
strings, caller-owned QPACK dynamic-table references, and Extended CONNECT
`:protocol` pseudo-headers for transports that negotiate the capability.
The client connection manager reuses injected QUIC clients by origin and
supports LRU capacity, idle expiry, and maximum connection lifetime policies.
Its high-level transport adapter supports in-memory and streaming request
bodies plus response-body and ordered informational-response callbacks; proxy
routing currently raises an explicit unsupported error. Per-request response
header-byte, field-count, and body limits can be stricter than the retained
connection defaults.

The HTTP/3 client uses finite defaults of 65,536 bytes for each QPACK
instruction, 262,144 bytes for each retained QPACK instruction buffer, 100
request streams, 16 peer unidirectional streams, and 4 MiB for aggregate
retained connection state. The injected QUIC backend still owns packet
processing, flow control, and connection shutdown. `max-body-bytes` defaults
to the core 16 MiB body limit at the client, server-stream, push, and
connection-manager entry points. Passing `NIL` explicitly disables that body
limit for that call.

The boundary does not provide QUIC packets, loss recovery, congestion control,
TLS 1.3, ALPN, native sockets, a connection-level accept loop, or a native
HTTP/3 server. The injected
transport can pass accepted peer unidirectional streams to
`accept-http3-peer-unidirectional-stream` for fragmented-prefix classification
and control/QPACK attachment, or to
`process-http3-peer-unidirectional-stream` to classify and apply the first
control or QPACK input. It remains responsible for accepting streams, invoking
the matching reader on later readiness notifications, and discarding unknown
extension streams.
The client can advertise `MAX_PUSH_ID`, receive promised responses on injected
push streams, and cancel pushes. The server boundary can emit a push promise
and its matching push stream when the surrounding QUIC transport supplies the
required callbacks.
The optional cl-quic-kit client adapter drives a caller-selected QUIC client,
including its TLS handshake and HTTP/3 stream lifecycle; it does not replace
the caller's QUIC, socket, certificate, or server-accept policy.
Invalid or deliberately unsupported protocol features raise public protocol or
unsupported-feature conditions.

## High-level client

The optional `cl-http-kit/client` system supplies reusable URI, authentication,
cookie, cache, content-coding selection, ALPN protocol-name, proxy, redirect,
retry, and thread-safe HTTP/1.1 connection-pool policies around core messages.
The client creates native TCP, TLS 1.3, and pooling boundaries when no custom
transport is supplied. The `cl-http-kit/tls` system uses cl-tls-kit for client
certificate verification, trust-store loading, and handshake deadlines.
Server-side TLS wrapping is explicitly unsupported by the current kit boundary.
For an HTTPS server, terminate TLS in a reverse proxy or load balancer and
forward plain HTTP to `cl-http-kit/network`. Configure the proxy to preserve
the original host and scheme according to the application's trusted-forwarding
policy; cl-http-kit does not validate forwarded headers automatically.
Origin 401 and proxy 407 challenges are parsed with the RFC 9110
challenge grammar and can be answered by separate provider callbacks. Each
provider receives at most one safe replay opportunity; streamed responses and
non-replayable request bodies suppress automatic origin or forward-proxy
replay. A rejected CONNECT tunnel is closed and reopened at most once when the
proxy provider supplies credentials for its parsed challenge.
The client enables an in-memory RFC 6797 HSTS store by default. Valid secure
responses can add, replace, expire, or remove host policies; matching HTTP
requests and redirect targets are upgraded before transport selection. IP
literals and insecure responses cannot establish policy. Applications can
inject a store to retain policy across client instances, clear it explicitly,
or pass `:strict-transport-store nil` to disable HSTS.
The client also enables an in-memory RFC 7838 alternative-service store by
default. It learns valid `Alt-Svc` response fields, accounts for `Age`, applies
the default `ma`, handles `clear`, expiry, and `persist=1`, and can discard
non-persistent alternatives when the network changes. Applications select from
the discovered services and remove a rejected service after a 421 response.
When `:http3-transport-function` is supplied, an advertised `h3` alternative is
attempted before TCP; a failed QUIC attempt removes that alternative for the
request and falls back to HTTP/2 or HTTP/1.1. The transport boundary must
authenticate the certificate for the origin, send the origin as TLS SNI, and
preserve proxy policy. An explicit HTTP/3 request does not fall back. Pass
`:alternative-service-store nil` to disable discovery.
Collected client responses automatically decode `gzip` and `deflate`
`Content-Encoding` values, including stacked codings, and remove stale
`Content-Encoding` and `Content-Length` fields. Decoding can be disabled with
`:automatic-decompression-p nil`. Additional codings such as `br` and `zstd`
can be supplied as decoder functions without imposing compression-library
dependencies on the core system. Eligible requests advertise the registered
codings with `Accept-Encoding` unless the caller supplies that field. The configured body limit is checked against
both transport bytes and each decoded representation, and bounded decoding stops
before allocating an unbounded expanded body. Streaming body callbacks
receive the encoded transport bytes and remain application-owned.
The private response cache honors RFC 5861 `stale-if-error` on requests and
responses after retries are exhausted for 500, 502, 503, 504, timeout, and
connection errors. Cache fallback eligibility is independent of whether the
retry policy retries those conditions. This fallback is limited to collected responses because a
streaming callback might already have observed bytes from the failed response.
RFC 5861 `stale-while-revalidate` is supported when a
`:stale-while-revalidate-scheduler` is configured. The scheduler receives a
revalidation function and owns asynchronous execution and duplicate-request
coalescing; returning false makes the request perform normal synchronous
validation. Serving stale updates `Age`, honors the response window, and is
suppressed by explicit request revalidation directives.
Fresh authenticated responses also honor RFC 8246 `immutable` for ordinary
`max-age=0` reloads. The extension is ignored for plain HTTP and for explicit
`no-cache` force reloads.

The cookie layer enforces host-only, prefix, SameSite, and partitioned-cookie
constraints and lets the request path provide SameSite context and a top-level
partition key. The cache layer evaluates request directives such as `no-store`,
`no-cache`, `max-age=0`, `min-fresh`, and `max-stale`, can return stale entries
when policy allows, and merges cache metadata from a 304 revalidation response
without discarding the stored body.

Client transport functions use one canonical keyword contract: they receive
the timeout, deadline, size limits, proxy context, request-body producer,
response callbacks, and `collect-body-p` keyword arguments described by the
client API. Older transports that accept only a subset of these keywords are
not supported; define the complete boundary or use the provided stream or
connection-pool constructors.

The current integration keeps these policy objects and compatibility entry
points, and its native default supplies TCP/TLS, redirects, cookies, content
coding, proxy environment lookup, and pooling when the optional systems are
available. HTTP/3 connection selection is available when the cl-quic-kit
adapter is explicitly supplied.

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
extensions. Extension fields use strict RFC 6455 token and quoted-string
parsing, and RFC 7692 `permessage-deflate` offer/response parameters are
validated. The client handshake returns normalized negotiated extensions as
its third value. Message reads and writes accept application-provided raw
DEFLATE codec functions; only this explicit opt-in permits RSV1, and expanded
messages remain subject to the configured size limit. The codec owns context
takeover, window settings, and the RFC 7692 DEFLATE tail transformation.
`make-websocket-client-key` validates and Base64-encodes 16 octets
from an application-supplied cryptographically secure random source;
`make-websocket-upgrade-request` can invoke that source directly. The entropy
source itself and connection lifecycle remain application-owned.

serve-websocket-session serves an already-upgraded stream: it requires masked
client frames by default, automatically answers Ping frames, echoes a valid
peer Close frame, dispatches complete messages to a handler, and closes with
an appropriate protocol or size code on errors. It does not open sockets,
perform TLS or ALPN, or choose extension and codec policy.

## Native network boundary

`cl-http-kit/network` provides a native SBCL TCP stream opener with IPv4/IPv6
DNS resolution and deadline-aware connect/read operations. It also provides
an IPv4/IPv6 listening socket boundary: `open-http-tcp-listener` binds and
listens, `accept-http-tcp-stream` accepts one binary stream and returns peer
address metadata, and `close-http-tcp-listener` closes the listener. It keeps
TLS, ALPN, and proxy negotiation outside the socket opener. Applications can
compose the separate `cl-http-kit/tls` cl-tls-kit client wrapper around
connected streams while retaining control of certificate and protocol policy.
Server-side wrapping is an explicit unsupported boundary.

serve-http1-listener combines accept and serve-http1-session dispatch for a
caller-owned listener, with connection limits and accept/error callbacks.

The native client path uses cl-tls-kit. Client-side HTTP/3 connection setup is
available through the cl-quic-kit adapter, while the HTTP/3 framing system
remains an injected QUIC stream boundary and applications retain socket and
certificate policy.

## Application-owned behavior

The portable core does not choose a socket library, DNS resolver, or TLS
implementation. Applications can adapt those concerns to their deployment,
including the optional SBCL network boundary, and can supply callbacks to the
library's pool without changing the message and wire layers. The optional
high-level client supplies native TCP/TLS when its default boundary is used;
custom transports retain ownership of their network and TLS setup. Proxy
negotiation, character encoding, and logging policy remain application-owned.

## Portability

The core is written in portable Common Lisp and uses binary streams and
injected callbacks rather than a platform-specific socket API. The repository's
Nix flake currently declares Darwin ARM64 and Linux x86-64 development targets;
the HTTP model itself does not depend on those operating-system details.
