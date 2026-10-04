# API Reference

This page documents the primary exported symbols of the registered ASDF
systems. Package definition files remain authoritative for lower-level and
auxiliary exports not described here. Common arguments such as timeout,
deadline, byte limits, and clock-function are accepted by the direct and CPS
transport APIs where shown.

The registered systems in this checkout are version 0.4.0. The current
integration includes native default client wiring and the URL-and-method
convenience form. The signatures below describe the current callback and
stream API.

## Core package

The core package name is HTTP-KIT.

### `http-uri`

The URI structure type.

### `http-uri-p`

Return true when the object is an HTTP-URI.

### `make-http-uri`

Construct a URI. The signature is
make-http-uri (&key (scheme "http") authority (path "/") query). The scheme
must be HTTP or HTTPS, authority is required, and path must begin with a slash.

### `parse-http-uri`

Parse a string into an HTTP-URI.

### `http-uri-scheme`

Return the normalized lower-case scheme from a URI.

### `http-uri-authority`

Return the normalized authority string from a URI.

### `http-uri-host`

Return the host component from a URI.

### `http-uri-port`

Return the numeric port, or nil when the authority has no explicit port.

### `http-uri-path`

Return the origin path from a URI.

### `http-uri-query`

Return the query string, or nil when no query was supplied.

### `http-uri-string`

Serialize a URI value to its HTTP URI string representation.

### `http-header`

The header structure type. It contains a name and a trimmed string content.

### `http-header-p`

Return true when the object is an HTTP-HEADER.

### `make-http-header`

Construct a header with make-http-header (name content). Names must be
non-empty ASCII tokens and content must be a string without controls or CRLF.

### `http-header-name`

Return the original header name string.

### `http-header-content`

Return the trimmed header content string.

### `http-header-values`

Return all values in headers whose names match name case-insensitively. The
order of duplicate values is preserved.

### `http-header-value`

Return the first matching header value, or the optional default when no value
is present.

### `http-header-present-p`

Return true when at least one header with the requested name is present.

### `http-request`

The request structure type.

### `http-request-p`

Return true when the object is an HTTP-REQUEST.

### `make-http-request`

Construct a request with make-http-request (&key method uri headers trailers
body). Methods are normalized to upper-case tokens, URI strings are parsed,
headers and trailers are normalized, and the body is copied.

### `http-request-method`

Return the normalized method string.

### `http-request-uri`

Return the HTTP-URI value.

### `http-request-headers`

Return the normalized list of HTTP-HEADER values.

### `http-request-trailers`

Return the normalized list of trailing HTTP-HEADER values.

### `http-request-body`

Return the request body as an octet vector. An omitted body is represented by
an empty octet vector.

### `http-request-authority`

Return the request URI authority.

### `http-request-path`

Return the request URI path.

### `http-request-query`

Return the request URI query.

### `http-response`

The response structure type.

### `http-response-p`

Return true when the object is an HTTP-RESPONSE.

### `make-http-response`

Construct a response with make-http-response (&key status reason headers
trailers body). Status must be between 100 and 599; an omitted reason uses the
default reason for the status when one is known.

### `http-response-status`

Return the numeric response status.

### `http-response-reason`

Return the response reason string.

### `http-response-headers`

Return the response header list.

### `http-response-trailers`

Return the response trailer list.

### `http-response-body`

Return the response body as an octet vector. An omitted body is represented by
an empty octet vector.

### `serialize-http-request`

Serialize a request to a one-dimensional octet vector using HTTP/1.1 request
framing. The serializer derives Host from the URI and validates body framing.

### `parse-http-request`

Parse an HTTP/1.0 or HTTP/1.1 request from an octet vector or binary input
stream. The signature is parse-http-request (input &key timeout deadline
max-header-bytes max-fields max-body-bytes default-authority on-body-chunk
collect-body-p on-expect-continue clock-function). MAX-FIELDS defaults to 256
and applies independently to request headers and request trailers.

### `parse-http-response`

Parse an HTTP/1.1 response from an octet vector or binary input stream. The signature is
parse-http-response (input &key timeout deadline max-header-bytes
max-fields max-body-bytes request-method clock-function). It handles fixed-length,
chunked, close-delimited, and no-body responses subject to the supplied byte
and field-count limits. MAX-FIELDS defaults to 256 and applies independently
to each informational, final, and trailer field section.

### `send-http-request-over-stream`

Send a request through an injected binary stream. The signature is
send-http-request-over-stream (request &key open-stream close-stream timeout
deadline max-header-bytes max-fields max-body-bytes clock-function). The open callback
receives the request and effective timeout and deadline; close-stream receives
the returned stream.

### `send-http-request-over-stream/cps`

The CPS form of send-http-request-over-stream. It takes
send-http-request-over-stream/cps (request on-success &key on-error open-stream
close-stream timeout deadline max-header-bytes max-fields max-body-bytes clock-function).

### serve-http1-session

Serve HTTP/1.0 and HTTP/1.1 requests on an already-open binary stream. The
signature is serve-http1-session (stream handler &key timeout deadline
max-header-bytes max-fields max-body-bytes default-authority on-body-chunk
collect-body-p on-expect-continue max-requests on-error on-upgrade
close-stream clock-function). The handler receives each parsed request and
returns an HTTP response or response stream. Its optional second value is an
ordered list of HTTP/1.1 informational responses (status 100-199 except 101)
to send before the final response. Informational responses are not sent to
HTTP/1.0 requests. The session supports request-body streaming, Expect:
100-continue, trailers, protocol upgrades, and bounded request counts.
MAX-FIELDS defaults to 256 and applies independently to request headers and
request trailers. It returns the number of final responses written and one of
EOF, CLOSE, UPGRADE, or MAX-REQUESTS.

### `recording-session`

The deterministic recording-session structure type.

### `recording-session-p`

Return true when the object is a RECORDING-SESSION.

### `make-recording-session`

Construct a session with make-recording-session (&key responses
response-function). Responses are consumed in order. Alternatively, the
response function receives request, timeout, and deadline keyword arguments.

### `recording-session-requests`

Return copies of accepted requests in request order.

### `recording-session-responses`

Return copies of responses returned by the session in response order.

### `send-recorded-http-request`

Return the next response from a recording session. The signature is
send-recorded-http-request (session request &key timeout deadline
max-header-bytes max-body-bytes clock-function).

### `send-recorded-http-request/cps`

The CPS form of send-recorded-http-request. It takes session, request,
on-success, and optional on-error, timeout, deadline, byte-limit, and clock
arguments.

### `http-deadline`

Compute an absolute deadline from a timeout. The signature is
http-deadline (timeout &key deadline clock-function). A nil timeout leaves an
inherited deadline unchanged; timeout values must be non-negative.

### `with-http-deadline`

Bind an effective deadline around body forms. The macro signature is
with-http-deadline ((deadline timeout &key inherited clock-function kind)
&body body). It checks the deadline before and after the body.

### `http-size-limit-exceeded`

The condition type for a header or body byte limit failure.

### `http-size-limit-exceeded-limit`

Return the configured size limit from an HTTP-SIZE-LIMIT-EXCEEDED condition.

### `http-size-limit-exceeded-observed`

Return the observed size from an HTTP-SIZE-LIMIT-EXCEEDED condition.

### `http-size-limit-exceeded-kind`

Return the affected area, such as headers or body.

### `http-error`

The common condition type for public HTTP-KIT failures.

### `http-error-message`

Return the diagnostic message from an HTTP-ERROR condition.

### `http-error-operation`

Return the operation keyword or value associated with an HTTP-ERROR condition.

### `http-protocol-error`

The condition type for malformed or contradictory protocol data.

### `http-protocol-error-detail`

Return structured protocol detail.

### `http-invalid-uri`

The condition type for invalid URI input.

### `http-invalid-uri-input`

Return the bounded diagnostic input associated with an invalid URI.

### `http-invalid-header`

The condition type for invalid header input.

### `http-invalid-header-name`

Return the header name associated with an invalid header condition.

### `http-invalid-header-reason`

Return the structured reason for header rejection.

### `http-invalid-status`

The condition type for invalid HTTP status lines or codes.

### `http-invalid-status-line`

Return the diagnostic status line.

### `http-invalid-status-code`

Return the invalid or unsupported status code.

### `http-connection-error`

The condition type for failures from an injected transport boundary.

### `http-connection-error-cause`

Return the underlying transport cause.

### `http-timeout`

The condition type for deadline and timeout failures.

### `http-timeout-kind`

Return the operation kind associated with a timeout.

### `http-unsupported-feature`

The condition type for protocol features outside the implementation.

### `http-unsupported-feature-name`

Return the unsupported feature identifier.

## HTTP/2 package

The optional package name is HTTP-KIT/HTTP2.

### `http2-client`

The HTTP/2 client structure type.

### `http2-client-p`

Return true when the object is an HTTP2-CLIENT.

### `make-http2-client`

Construct a client with make-http2-client (&key exchange open-stream
close-stream max-frame-size max-header-bytes max-fields max-body-bytes
max-control-frames max-control-window clock-function).
Exactly one of exchange or open-stream must be supplied. Frame size must be
between 16384 and 16777215. `max-fields` limits each decoded response header or
trailer section and defaults to 256.

### `send-http2-request`

Send one request through an HTTP/2 client. The signature is
send-http2-request (client request &key timeout deadline max-header-bytes
max-fields max-body-bytes clock-function). Per-request limits may tighten but
cannot weaken the client or reusable connection limits.

### `send-http2-request/cps`

The CPS form of send-http2-request. It takes client, request, on-success, and
optional on-error, timeout, deadline, byte-limit, and clock arguments.

### Reusable HTTP/2 connections

`make-http2-connection` creates an owner-thread connection over an already
negotiated binary stream. `send-http2-request-over-connection` sends one
request, while `send-http2-requests-over-connection` multiplexes a non-empty
batch. `send-http2-request-over-connection/cps` and
`send-http2-requests-over-connection/cps` accept success and error
continuations, and
`make-http2-connection-transport` adapts a connection to the high-level client
transport protocol. `http2-client-connection` returns the reusable connection
owned by a stream-backed client, when present. These constructors, send
functions, CPS forms, and transport adapters accept `max-fields`; the default
per decoded header or trailer section is 256.

`cancel-http2-stream` emits `RST_STREAM` for an opened client stream.
`ping-http2-connection` sends a PING and waits for the matching acknowledgement.
`send-http2-priority-update` emits an RFC 9218 priority update.
`send-http2-goaway` and its convenience alias
`graceful-shutdown-http2-connection` make the connection drain without
discarding already-created streams. `close-http2-connection` closes it
idempotently.

Connection state is exposed by `http2-connection-p`,
`http2-connection-open-p`, `http2-connection-session-started-p`,
`http2-connection-draining-p`, `http2-connection-goaway-last-stream-id`, and
`http2-connection-local-goaway-last-stream-id`. Negotiated peer limits are
available through `http2-connection-peer-max-frame-size`,
`http2-connection-peer-max-table-size`,
`http2-connection-peer-max-concurrent-streams`,
`http2-connection-peer-max-header-list-size`,
`http2-connection-peer-initial-window-size`, and
`http2-connection-peer-connection-window-size`.

HTTP/2 header-block assembly has three configurable implementation caps in
the `http-kit/http2` package. `*h2-max-header-block-frames*` defaults to 128
frames per block, `*h2-max-continuation-frames*` defaults to 127 continuation
frames, and `*h2-max-header-block-parts*` defaults to 128 retained fragments.
They apply while reading client response headers and batched response headers;
exceeding a cap raises `HTTP-PROTOCOL-ERROR` before the block is decoded.
These caps are separate from the per-section `max-header-bytes` and
`max-fields` limits.

The HTTP/2 server also accepts `max-concurrent-streams`, `max-reset-streams`,
and `max-hpack-table-size`. Their defaults are 100, 100, and 4096 bytes.
The first bounds active request streams per session. The reset budget defaults
to 100 resets in a one-second window and closes the session with
`HTTP-PROTOCOL-ERROR` when exceeded. Wire-level HTTP/2 connection failures
send GOAWAY before the session closes; stream admission failures send a
REFUSED_STREAM RST_STREAM and keep the connection usable. The bounded server
failure paths use these RFC 9113 error codes:

| Condition | Frame and error code | RFC 9113 |
| --- | --- | --- |
| RST_STREAM on an idle stream | GOAWAY / PROTOCOL_ERROR | §5.1, §6.4 |
| Concurrent stream limit | RST_STREAM / REFUSED_STREAM | §5.1, §6.5.2 |
| Reset budget | GOAWAY / ENHANCE_YOUR_CALM | §7 |
| HPACK decoding failure | GOAWAY / COMPRESSION_ERROR | §4.3, §5.4.1 |

The last
clamps the peer's HPACK dynamic-table capacity. The client connection path
also clamps peer HPACK table settings to 4096 bytes by default through
`*h2-max-peer-hpack-table-size*`.

### HTTP/2 connection manager

`make-http2-connection-manager` constructs an owner-thread connection pool
with `&key open-connection max-connections idle-timeout max-connection-age
clock-function`.
Connections are reused by origin unless a caller supplies `connection-key`,
the least-recently-used idle connection is evicted at the configured cap, and
an idle timeout of `nil` disables idle expiry. `max-connection-age` optionally
recycles connections after a total lifetime even when they remain active. The open callback receives
the request and may accept `timeout` and `deadline`; it must return an
`http2-connection`.

`send-http2-request-over-connection-manager` sends one request through the
pool. `send-http2-requests-over-connection-manager` sends a non-empty batch on
one connection so the underlying session can multiplex the streams.
`make-http2-connection-manager-transport` adapts a manager to the high-level
client transport protocol. All three accept and forward the per-section
`max-fields` limit. `close-http2-connection-manager` closes all retained
connections and permanently closes the manager.

The inspection functions are `http2-connection-manager-p`,
`http2-connection-manager-open-p`,
`http2-connection-manager-max-connections`,
`http2-connection-manager-idle-timeout`,
`http2-connection-manager-max-connection-age`,
`http2-connection-manager-clock-function`,
`http2-connection-manager-connection-count`, and
`http2-connection-manager-connections`. The returned connection list is a
snapshot and does not expose the manager's internal list structure.

### serve-http2-session

Serve HTTP/2 requests on a caller-supplied binary stream. The signature is
serve-http2-session (stream handler &key timeout deadline max-frame-size
max-header-bytes max-fields max-body-bytes default-authority collect-body-p
on-body-chunk max-requests on-error close-stream clock-function
max-control-frames max-control-window). The session
owns the HTTP/2 preface, SETTINGS, HPACK, stream state, flow control, request
body collection, and response framing, and returns a request count and
termination reason. `max-fields` defaults to 256 for each request header or
trailer section. `max-control-frames` defaults to 100 frames in a one-second
`max-control-window`; RST_STREAM is excluded from this budget, while all other
control frames, including SETTINGS, PING, PRIORITY, WINDOW_UPDATE, GOAWAY, and
PRIORITY_UPDATE, are counted.
Exceeding the budget sends GOAWAY with ENHANCE_YOUR_CALM before closing. The
same keywords are accepted by `make-http2-client` and
`make-http2-connection`. The handler's primary value is the final response. Its
optional second value is a list of informational responses, which are emitted
in order before the final response; informational responses must use a status
from 100 through 199 other than 101 and cannot carry a body or trailers.

## HTTP/3 package

The optional package name is HTTP-KIT/HTTP3.

### HTTP/3 codecs

HTTP3-VARINT-ENCODE and HTTP3-VARINT-DECODE implement QUIC variable-length
integers. MAKE-HTTP3-FRAME, ENCODE-HTTP3-FRAME, and DECODE-HTTP3-FRAMES
construct and parse bounded HTTP/3 frames. MAKE-HTTP3-SETTINGS-FRAME,
DECODE-HTTP3-SETTINGS, and HTTP3-CONTROL-STREAM-PREFIX handle the local
control-stream preface and settings.

QPACK-ENCODE-FIELD-SECTION and QPACK-DECODE-FIELD-SECTION encode and decode
field sections using static entries, literal representations, and optional
HPACK Huffman coding. Supplying a caller-owned dynamic table enables dynamic
entry references. QPACK-DYNAMIC-TABLE-INSERT and the encoder/decoder stream
codecs expose insertion and synchronization instructions; field-section
encoding only references entries already present, so callers must synchronize
the peer table before sending a section that has a non-zero required insert
count.

### HTTP/3 client

MAKE-HTTP3-CLIENT opens the local control stream and writes SETTINGS using
caller-supplied OPEN-STREAM, WRITE-STREAM, and READ-STREAM callbacks. The
callbacks receive stream type, timeout, deadline, and FIN options as
appropriate. MAX-HEADER-BYTES and MAX-FIELDS bound decoded response field
sections. CLOSE-HTTP3-CLIENT closes the control stream.

ACCEPT-HTTP3-PEER-UNIDIRECTIONAL-STREAM reads a possibly fragmented stream-type
prefix and routes control and QPACK streams to the client's singleton stream
state. It returns :PUSH for a stream that should be passed to
RECEIVE-HTTP3-PUSH and :UNKNOWN for an extension stream whose remaining bytes
the surrounding QUIC transport must discard. Prefix bytes consumed during
classification are replayed to the corresponding HTTP/3 reader.
PROCESS-HTTP3-PEER-UNIDIRECTIONAL-STREAM additionally applies the first
available control or QPACK input and returns its stream kind, effects, and end
status. A connection event loop can call the matching READ-HTTP3-CONTROL-STREAM
or QPACK reader on later readiness notifications. Push and unknown extension
streams remain caller-owned.

SEND-HTTP3-REQUEST takes an HTTP request and optional ON-BODY-CHUNK,
ON-INFORMATION, COLLECT-BODY-P, MAX-HEADER-BYTES, MAX-FIELDS, MAX-BODY-BYTES,
REQUEST-BODY-FUNCTION, REQUEST-BODY-LENGTH, QPACK-ENCODER-TABLE,
QPACK-DECODER-TABLE, HUFFMAN-P, TIMEOUT, and DEADLINE arguments. A request-body
producer receives the client's maximum DATA payload
size and returns non-empty octet arrays until NIL; a declared length is
enforced and emitted as Content-Length. It opens a request stream, writes
request headers, DATA, and trailers, then parses the response. The QPACK tables
are caller-owned and are not synchronized automatically. HTTP3-CLIENT accessors
expose the callback slots, limits, settings, and open state.

MAKE-HTTP3-CLIENT also accepts MAX-REQUEST-STREAMS,
MAX-PEER-UNIDIRECTIONAL-STREAMS, MAX-STATE-BYTES,
QPACK-MAX-INSTRUCTION-BYTES, and QPACK-MAX-BUFFER-BYTES. Their finite defaults
are 100, 16, 4 MiB, 65,536 bytes, and 262,144 bytes respectively. The
client, server-stream, push, and connection-manager body-limit entry points
default MAX-BODY-BYTES to the core 16 MiB default; an explicitly supplied NIL
disables that limit for the call.

MAKE-HTTP3-CONNECTION-MANAGER pools clients by request origin. Its
MAX-CONNECTIONS policy evicts the least recently used origin, while
IDLE-TIMEOUT and MAX-CONNECTION-AGE expire retained clients using a monotonic
CLOCK-FUNCTION. SEND-HTTP3-REQUEST-OVER-CONNECTION-MANAGER preserves the
SEND-HTTP3-REQUEST options, including streaming request bodies, and
MAKE-HTTP3-CONNECTION-MANAGER-TRANSPORT adapts the manager to a high-level
transport callback. Informational-response callbacks receive each validated
1xx response in wire order; proxy routing is rejected rather than silently
ignored. Per-request MAX-HEADER-BYTES and MAX-FIELDS values can tighten but
never weaken the retained client's configured limits.
CLOSE-HTTP3-CONNECTION-MANAGER closes all retained clients and rejects
subsequent sends.

RECEIVE-HTTP3-PUSH accepts per-receive MAX-HEADER-BYTES, MAX-FIELDS, and
MAX-BODY-BYTES limits. Header limits can tighten but never weaken the HTTP/3
client's configured limits, matching ordinary response handling.

### serve-http3-request-stream

Serve one HTTP/3 request stream over caller-supplied QUIC stream callbacks.
The signature is serve-http3-request-stream (stream handler &key read-stream
write-stream close-stream max-frame-size max-header-bytes max-fields
max-body-bytes collect-body-p on-body-chunk qpack-decoder-table
qpack-encoder-table huffman-p timeout deadline on-error). The session owns
HTTP/3 framing, QPACK field sections, request-body limits, and response
framing; the caller owns QPACK instruction-stream synchronization, QUIC, TLS,
ALPN, and connection-level stream dispatch.

## Native network package

The optional package name is HTTP-KIT/NETWORK. HTTP-NETWORK-RESOLVE-HOST
resolves an IPv4 or IPv6 address on SBCL. OPEN-HTTP-TCP-STREAM opens a
deadline-aware binary TCP stream for a request or proxy endpoint, and
CLOSE-HTTP-TCP-STREAM closes it. MAKE-HTTP-NETWORK-STREAM-OPENER adapts the
native boundary to the client stream-opener callback. OPEN-HTTP-TCP-LISTENER
binds and listens on an IPv4 or IPv6 address. ACCEPT-HTTP-TCP-STREAM accepts
one connection and returns the binary stream together with the peer address
and port; CLOSE-HTTP-TCP-LISTENER releases the listening socket. Listener
accept and stream reads support TIMEOUT and DEADLINE arguments. TLS, ALPN,
and proxy negotiation remain outside this system.

SERVE-HTTP1-LISTENER combines accept and SERVE-HTTP1-SESSION dispatch for a
caller-owned listener, with connection limits and accept/error callbacks.

## TLS package

The optional package name is HTTP-KIT/TLS and is provided by the
`cl-http-kit/tls` system. MAKE-HTTP-TLS-UPGRADER returns a client upgrade
callback backed by cl-tls-kit. Its VERIFY argument accepts NIL, :OPTIONAL, or
:REQUIRED and defaults to :REQUIRED; the callback passes the request host as
TLS SNI and accepts ALPN-PROTOCOLS, TIMEOUT, and DEADLINE values. Each ALPN
protocol name must contain 1 to 255 ASCII characters. CERTIFICATE, KEY, and
PASSWORD are reserved for a future client-certificate integration and currently
signal an unsupported-feature condition when supplied. The client wrapper
preserves the binary-stream boundary and applies handshake deadlines.
MAKE-HTTP-TLS-SERVER-WRAPPER validates its certificate arguments then signals
an explicit unsupported-feature condition because the current kit does not
expose a server driver. For an HTTPS server, terminate TLS in a reverse proxy
or load balancer and forward HTTP to `cl-http-kit/network`.
HTTP-TLS-SELECTED-ALPN-PROTOCOL returns the protocol negotiated by the
cl-tls-kit client driver for an upgraded client stream, or NIL when no protocol
was negotiated. Applications can use that result to dispatch HTTP/2 or
HTTP/1.1 handling.

The TLS package is TLS 1.3 only through cl-tls-kit. TLS 1.2, OCSP/CRL, and
server-side wrapping are outside the current boundary.

## Observability package

The optional package name is HTTP-KIT/OBSERVABILITY.

### `http-metrics`

The metrics adapter structure type.

### `http-metrics-p`

Return true when the object is an HTTP-METRICS value.

### `make-http-metrics`

Construct metrics with make-http-metrics (&optional registry). When registry
is omitted, a new metric registry is created.

### `http-metrics-registry`

Return the metric registry used by the adapter.

### `http-metrics-request-counter`

Return the request counter. It uses method and outcome labels.

### `http-metrics-error-counter`

Return the error counter. It uses method and condition-kind labels.

### `call-with-http-observability/cps`

Wrap an operation with metrics collection. The signature is
call-with-http-observability/cps (metrics request operation on-success &key
on-error).

## Client package

The optional package name is HTTP-KIT/CLIENT. It builds request policies around
the core HTTP-KIT request and response values while leaving network setup to the
application.

## Conditions

### `http-client-error`

The base condition type for failures raised by the client policy layer.

### `http-client-error-detail`

Return the detail value carried by an HTTP-CLIENT-ERROR.

### `http-redirect-limit-exceeded`

The condition type signaled when a redirect policy reaches its maximum.

### `http-redirect-limit-exceeded-uri`

Return the URI at which the redirect limit was reached.

### `http-redirect-limit-exceeded-redirects`

Return the number of redirects attempted before the failure.

### `http-retry-exhausted`

The condition type signaled when a retry policy has no attempts remaining.

### `http-retry-exhausted-attempts`

Return the number of attempts made before retry exhaustion.

### `http-retry-exhausted-last-condition`

Return the last transport or policy condition observed by a retry operation.

### `http-retry-exhausted-last-response`

Return the last response observed by a retry operation, when one exists.

### `http-cookie-error`

The condition type for invalid or unusable cookie state.

### `http-cache-error`

The condition type for cache policy failures.

### `http-proxy-error`

The condition type for proxy selection or planning failures.

## URI and encoding

### `resolve-http-uri`

Resolve a URI reference against a base URI and return an HTTP-URI.

### `http-uri-origin`

Return the scheme-and-authority origin for a URI.

### `http-same-origin-p`

Return true when two URIs have the same origin.

### `http-percent-encode`

Encode a string for URI use. The signature is
http-percent-encode (string &key (safe "-._~") (space-as-plus-p nil)).

### `http-form-urlencode`

Encode form fields as an application/x-www-form-urlencoded string.

### `http-form-urlencoded-octets`

Encode form fields as an application/x-www-form-urlencoded octet vector.

### `make-http-content-decoders`

Return a Content-Encoding decoder registry containing the built-in `gzip` and
`deflate` decoders plus any supplied `(coding . function)` pairs. Each function
receives the encoded body and decoded-body byte limit and returns decoded
octets. A later pair replaces a built-in decoder with the same coding name.

### `decode-http-response-content`

Decode a collected response body. The signature is
decode-http-response-content (response &key max-body-bytes content-decoders).
Stacked codings are decoded in reverse application order. If any coding is not
registered, the response is returned unchanged so it is never partially
decoded.

### `http-multipart-part`

The multipart-part structure type.

### `http-multipart-part-p`

Return true when the object is an HTTP-MULTIPART-PART.

### `make-http-multipart-part`

Construct a multipart part with a name, value, and optional filename and
content type.

### `http-multipart-part-name`

Return the multipart part field name.

### `http-multipart-part-value`

Return the multipart part value.

### `http-multipart-part-filename`

Return the optional multipart part filename.

### `http-multipart-part-content-type`

Return the optional multipart part content type.

### `make-http-multipart-body`

Construct a multipart body from parts. The signature is
make-http-multipart-body (parts &key boundary). It returns the body octets and
the corresponding Content-Type value with a quoted boundary parameter. An
explicit boundary must satisfy the RFC 2046 boundary grammar: one through
seventy ASCII boundary characters, with no trailing space.

### `parse-http-multipart-body`

Parse a multipart/form-data octet vector into multipart parts. The signature
is parse-http-multipart-body (body &key content-type boundary max-parts
max-header-bytes max-body-bytes max-input-bytes). Content-Type and an explicit
boundary may both be supplied only when they agree. The parser validates
delimiter lines, requires exactly one Content-Disposition per part, rejects
duplicate Content-Type fields, and applies the configured per-part header,
aggregate part-body, part-count, and complete encoded-input limits. The input
limit defaults to 64 MiB and is checked before the parser copies the body; NIL
disables it.

## Date and authentication

### `http-parse-date`

Parse an RFC 9110 HTTP-date string and return universal time, or nil for an
invalid value. IMF-fixdate and both obsolete date formats are supported with
their case-sensitive grammar. The signature is `http-parse-date (value &key
(now (get-universal-time)))`; `now` controls the RFC 850 two-digit-year rule.

### `http-authentication-challenge`

The parsed representation of one `WWW-Authenticate` challenge. Access its
scheme, optional token68 value, and parameters with
`http-authentication-challenge-scheme`,
`http-authentication-challenge-token68`, and
`http-authentication-challenge-parameters`.

### `http-parse-authentication-challenges`

Parse one field value, or a list of field values, into RFC 9110 authentication
challenges. Quoted commas and quoted-pairs are preserved while malformed
challenges are omitted.

### `http-authentication-challenge-parameter`

Return a challenge parameter by case-insensitive name, or nil when absent.

### `http-basic-authorization`

Construct a Basic authorization value from a username and password. Credentials
are UTF-8 encoded. Control characters are rejected, and the username cannot
contain a colon.

### `http-bearer-authorization`

Construct a Bearer authorization value from an RFC 6750 `b64token`. Empty
tokens, whitespace, non-ASCII characters, and padding outside the token suffix
are rejected.

### `http-digest-authorization`

Construct an RFC 7616 Digest authorization value from a parsed Digest
challenge, method, request target, username, password, and caller-generated
cnonce. The signature is `http-digest-authorization (challenge method
request-target username password cnonce &key (nonce-count 1) qop entity-body)`.
SHA-512-256, SHA-512-256-sess, SHA-256, SHA-256-sess, MD5, and MD5-sess are
supported with `auth`, `auth-int`, and the challenge's `userhash=true` option;
non-ASCII unhashed usernames use the RFC 7616 `username*` UTF-8 extended
parameter. Supply `entity-body` as an octet vector when selecting `auth-int`.
The caller owns nonce-count persistence and cnonce generation.

### `http-content-digest`

Construct an RFC 9530 `Content-Digest` field value from an octet vector. The
active `sha-256` algorithm is supported and used by default.

### `http-content-digest-valid-p`

Verify an octet vector against one `Content-Digest` field value or a non-empty
list of combined field values. Unknown algorithms are ignored. Malformed byte
sequences and duplicate algorithm keys signal `http-protocol-error`; a missing
supported algorithm or digest mismatch returns false. Digest comparison is
constant-time with respect to digest bytes.

## Cookies

### `http-cookie`

The cookie structure type.

### `make-http-cookie`

Construct a cookie. The signature is
make-http-cookie (&key name value domain (path "/") expires max-age secure-p
http-only-p same-site partitioned-p partition-key (host-only-p nil)
(creation-time (get-universal-time))).

### `http-cookie-name`

Return the cookie name.

### `http-cookie-value`

Return the cookie value.

### `http-cookie-domain`

Return the normalized cookie domain.

### `http-cookie-path`

Return the cookie path.

### `http-cookie-expires`

Return the cookie expiry time, when present.

### `http-cookie-max-age`

Return the cookie max-age value, when present.

### `http-cookie-secure-p`

Return true when the cookie is marked Secure.

### `http-cookie-http-only-p`

Return true when the cookie is marked HttpOnly.

### `http-cookie-same-site`

Return the cookie SameSite value.

### `http-cookie-partition-key`

Return the cookie partition key, or NIL for an unpartitioned cookie.

### `http-cookie-host-only-p`

Return true when the cookie is host-only.

### `http-cookie-creation-time`

Return the cookie creation time.

### `http-cookie-expiry-time`

Return the effective expiry time computed from Max-Age or Expires, when one
exists.

### `http-cookie-last-access-time`

Return the last time the cookie participated in a Cookie header.

### `http-cookie-jar`

The cookie-jar structure type.

### `make-http-cookie-jar`

Construct a cookie jar. The signature is
make-http-cookie-jar (&key clock-function public-suffix-p-function
max-cookies max-cookies-per-domain). The optional clock function is used for
expiry and ordering decisions. The optional public-suffix predicate rejects
Domain attributes that would escape an application's registrable-domain
boundary. Storage limits use least-recently-used eviction.

### `http-cookie-jar-cookies`

Return the cookie snapshot held by a jar.

### `http-cookie-jar-accept-response`

Accept response cookies for a request URI. The signature is
http-cookie-jar-accept-response (jar request-uri response &key now
partition-key same-site-p top-level-navigation-p). SameSite=Strict and
SameSite=Lax cookies received by a cross-site subresource or nested navigation
are rejected; cross-site top-level navigations may store them. Cookie name
prefixes are matched case-insensitively. `__Secure-` requires the Secure
attribute; `__Host-` additionally requires host-only scope and an explicit Path
attribute whose resulting cookie path is `/`.

### `http-cookie-jar-cookie-header`

Return the Cookie header value applicable to a request URI. The signature is
http-cookie-jar-cookie-header (jar uri &key now same-site-p
top-level-navigation-p method partition-key). SameSite defaults to a same-site
subresource request; callers must describe cross-site and top-level-navigation
contexts when applicable. PARTITION-KEY is the caller's canonical schemeful
top-level-site identifier. A Partitioned Set-Cookie is rejected without that
context or Secure, and partitioned cookies are returned only for an equal key.
This API follows the expired CHIPS individual draft; callers should treat the
key representation as an application contract until the extension is
standardized.

### `http-cookie-jar-clear`

Remove all cookies from a jar.

## Redirect and retry policies

### `http-redirect-policy`

The redirect-policy structure type.

### `make-http-redirect-policy`

Construct a redirect policy with a maximum redirect count, allowed status
codes, downgrade policy, and authorization preservation policy.

### `http-redirect-policy-max-redirects`

Return the maximum number of redirects.

### `http-redirect-policy-statuses`

Return the status codes that trigger redirects.

### `http-redirect-policy-allow-downgrade-p`

Return whether an HTTPS-to-HTTP redirect is allowed.

### `http-redirect-policy-preserve-authorization-p`

Return whether authorization may be preserved across a redirect.

### `http-retry-policy`

The retry-policy structure type.

### `make-http-retry-policy`

Construct a retry policy with method and status allowlists, delay bounds,
optional exponential-backoff jitter, and timeout and connection-error behavior.

### `http-retry-policy-max-attempts`

Return the maximum retry attempts.

### `http-retry-policy-methods`

Return the methods eligible for retry.

### `http-retry-policy-statuses`

Return the response statuses eligible for retry.

### `http-retry-policy-base-delay`

Return the base retry delay in seconds.

### `http-retry-policy-max-delay`

Return the maximum retry delay in seconds.

### `http-retry-policy-jitter-ratio`

Return the exponential-backoff jitter ratio. Zero disables jitter; values up
to one randomize the delay symmetrically around the exponential delay. A valid
Retry-After value is not randomized.

### `http-retry-policy-respect-retry-after-p`

Return whether Retry-After is honored.

### `http-retry-policy-retry-on-timeout-p`

Return whether timeout conditions are retryable.

### `http-retry-policy-retry-on-connection-error-p`

Return whether connection conditions are retryable.

## HTTP Strict Transport Security

### `http-strict-transport-store`

The in-memory RFC 6797 policy-store structure type.

### `make-http-strict-transport-store`

Construct a store with an optional clock function.

### `http-strict-transport-store-policies`

Return a snapshot of unexpired policies.

### `http-strict-transport-store-known-host-p`

Return true when a host has an exact or `includeSubDomains` policy. IP
literals never match.

### `http-strict-transport-store-note-response`

Process the first Strict-Transport-Security field from a secure response.
Malformed fields and fields received over HTTP are ignored.

### `http-strict-transport-store-upgrade-uri`

Upgrade an HTTP URI for a known host to HTTPS. An explicit port 80 becomes
443; other explicit ports are retained.

### `http-strict-transport-store-clear`

Remove all policies from a store.

## HTTP Alternative Services

### `http-alternative-service`

An RFC 7838 alternative-service record. Accessors expose its origin,
protocol ID, host, port, expiry time, and persistence flag.

### `http-alternative-service-p`

Return true when the object is an alternative-service record.

### `http-alternative-service-store`

The in-memory RFC 7838 alternative-service store type.

### `http-alternative-service-store-p`

Return true when the object is an alternative-service store.

### `make-http-alternative-service-store`

Construct a store with an optional clock function.

### `http-alternative-service-store-services`

Return the unexpired alternative services known for an origin URI.

### `http-alternative-service-store-note-response`

Process `Alt-Svc` fields from a response. The store applies `Age`, `ma`,
`persist`, replacement, `clear`, and the RFC 7838 rule that ignores fields on
421 responses.

### `http-alternative-service-store-remove`

Remove one service for an origin, such as after that alternative returned 421.

### `http-alternative-service-store-network-changed`

Discard alternatives without `persist=1` after a network change.

### `http-alternative-service-store-clear`

Clear one origin when given a URI, or clear the entire store when omitted.

The service accessors are `http-alternative-service-origin`,
`http-alternative-service-protocol-id`, `http-alternative-service-host`,
`http-alternative-service-port`, `http-alternative-service-expires-at`, and
`http-alternative-service-persist-p`. Selecting and dialing an alternative is
application-owned so the transport can authenticate the origin certificate,
send origin SNI, and retain the applicable proxy policy.

## Cache

### `http-cache`

The cache structure type.

### `make-http-cache`

Construct a cache. The signature is
make-http-cache (&key (max-entries 256) (clock-function #'get-universal-time)
status-identifier). When `status-identifier` is a non-empty visible ASCII
string, responses handled by the cache append an RFC 9211 `Cache-Status`
member describing local hits, forwarded requests, revalidation status, and
successful storage. Existing upstream members are preserved before the local
member. It is disabled by default because cache diagnostics can disclose
sensitive deployment information.

### `http-cache-status-identifier`

Return the optional RFC 9211 cache identifier.

### `http-cache-entries`

Return the cache entry snapshot.

### `http-cache-entry`

The cache-entry structure type.

### `http-cache-entry-p`

Return true when the object is an HTTP-CACHE-ENTRY.

### `http-cache-entry-key`

Return the cache key.

### `http-cache-entry-method`

Return the cached request method.

### `http-cache-entry-uri`

Return the cached request URI.

### `http-cache-entry-response`

Return the cached response.

### `http-cache-entry-stored-at`

Return the time at which the entry was stored.

### `http-cache-entry-expires-at`

Return the entry expiry time, when present.

### `http-cache-entry-etag`

Return the cached ETag value, when present.

### `http-cache-entry-last-modified`

Return the cached Last-Modified value, when present.

### `http-cache-entry-vary`

Return the response Vary fields associated with the entry.

### `http-cache-entry-request-vary-values`

Return the request header values captured for Vary matching.

### `http-cache-entry-accessed-at`

Return the most recent access time.

### `http-cache-max-entries`

Return the cache capacity.

### `http-cache-clock-function`

Return the clock function used by the cache.

### `http-cache-key`

Return a cache key for a request or URI.

### `http-cache-lookup`

Look up a request in a cache. The signature is
http-cache-lookup (cache request &key now); it returns three values: the
response when directly reusable or NIL, a state of :FRESH, :STALE, or :MISS,
and the matching cache entry when available. A :STALE state can include a
response when the request's `max-stale` directive explicitly permits reuse;
otherwise a NIL response indicates that validation is required.
When duplicate numeric freshness directives conflict, lookup applies the most
restrictive valid value; malformed duplicates do not permit reuse.
Requests that already contain HTTP precondition fields bypass cache reuse so
their conditions are forwarded unchanged to the origin server.
Fresh HTTPS responses marked `Cache-Control: immutable` remain reusable for
ordinary reload requests carrying `max-age=0`. Plain HTTP responses and
force-reload requests carrying `no-cache` continue to require validation.
When `http-client-send` revalidates a stale entry, valid `stale-if-error`
request or response directives permit a collected cached response after
eligible server or connection failures. The returned `Age` reflects its
current age. Streaming callbacks do not use this fallback.

### `http-cache-store`

Store a request and response in a cache. Responses carrying `no-store` are
not stored, except when `must-understand` is also present and the response
status is understood by the cache, as recommended by RFC 9111. Unknown status
codes carrying `must-understand` are not stored.

### `http-cache-invalidate`

Invalidate entries matching a request or URI.

### `http-cache-clear`

Remove all entries from a cache.

## Proxy

### `http-proxy`

The proxy structure type.

### `make-http-proxy`

Construct a proxy from a scheme, host, port, credentials, and no-proxy rules.
HTTP and HTTPS proxies encode credentials as HTTP Basic authentication. SOCKS5
and SOCKS5H encode credentials as UTF-8 for RFC 1929 username/password
authentication; when a username is supplied, both fields must contain from 1
through 255 encoded octets. RFC 1929 sends these credentials without
confidentiality, so use it only over a trusted path.

### `http-proxy-scheme`

Return the proxy scheme.

### `http-proxy-host`

Return the proxy host.

### `http-proxy-port`

Return the proxy port.

### `http-proxy-username`

Return the optional proxy username.

### `http-proxy-password`

Return the optional proxy password.

### `http-proxy-no-proxy`

Return the proxy no-proxy rules.

### `http-proxy-for-uri`

Return the proxy applicable to a URI, or nil when the URI is bypassed.

### `http-proxy-no-proxy-p`

Return true when a URI matches a proxy bypass rule. Rules may be exact hosts,
domains that also match their subdomains, optional `host:port` pairs, IPv4 or
IPv6 CIDR blocks, or `*` to bypass every destination.

### `http-proxy-plan`

Construct the proxy plan for a URI.

## Client

### HTTP/1.1 connection pools

`MAKE-HTTP-CONNECTION-POOL` constructs a thread-safe pool around an
`OPEN-STREAM` function. `IDLE-TIMEOUT` optionally expires unused streams, while
`MAX-CONNECTION-AGE` optionally limits total stream lifetime so continuously
used connections still refresh DNS, certificates, and routes.
`HTTP-CONNECTION-POOL-SEND` reuses a stream only after
the HTTP/1 parser proves that the response is self-delimited and neither peer
requested closure. High-level clients partition pooled connections by origin,
proxy route, DNS mode, and proxy credentials.

`HTTP-CONNECTION-POOL-STATS` returns `:IDLE-COUNT`, `:MAX-IDLE`,
`:IDLE-TIMEOUT`, and `:MAX-CONNECTION-AGE`. The corresponding policy accessors
include `HTTP-CONNECTION-POOL-IDLE-TIMEOUT` and
`HTTP-CONNECTION-POOL-MAX-CONNECTION-AGE`. It deliberately omits internal route keys because they can
contain authentication material. `HTTP-CONNECTION-POOL-CLEAR` closes all idle
streams. Pool operations may be used concurrently from multiple SBCL threads.

### `http-client`

The high-level client structure type.

### `http-client-p`

Return true when the object is an HTTP-CLIENT.

### `make-http-client`

Construct a client. The signature is
make-http-client (&key transport-function open-stream close-stream
connection-pool
default-headers cookie-jar cache strict-transport-store
alternative-service-store redirect-policy
retry-policy proxy tls-upgrade resolve-host resolve-host-style auth-provider
challenge-auth-provider proxy-challenge-auth-provider
stale-while-revalidate-scheduler clock-function
wall-clock-function sleep-function random-function default-timeout max-header-bytes max-fields max-body-bytes
automatic-decompression-p content-decoders on-request on-response). At most one
of transport-function, open-stream, or connection-pool may be supplied;
omitting all three selects the native TCP/TLS connection pool.
The connection pool owns pooled stream opening and closing; close-stream is
optional for the direct stream transport boundary. Omitting
strict-transport-store creates a new RFC 6797 store; explicitly passing nil
disables HSTS. Omitting alternative-service-store creates a new RFC 7838 store;
explicitly passing nil disables alternative-service discovery.

`resolve-host-style` is `:keywords` by default. In that mode, the resolver
receives HOST, `:timeout`, and `:deadline`. Set it to `:host-only` for a
legacy resolver that accepts only HOST. The resolver is not retried when its
body signals an error, so the style must match the callback.

The injected transport-function must accept the complete keyword boundary used
by the client, including timeout, deadline, limits, proxy context, request-body
callbacks, response callbacks, and collect-body-p. Partial legacy keyword
signatures are intentionally unsupported.

`HTTP-CLIENT-SEND` also accepts a method and URL convenience form while
retaining its request-object form.

### Client and HTTP/1.1 defaults

| Setting | Default | Scope |
| --- | ---: | --- |
| Client request timeout | 30 seconds | One request deadline covering DNS, connect, TLS, write, read, redirects, and retries |
| Maximum header bytes | 65,536 | HTTP/1.1 headers and trailers |
| Maximum fields | 256 | Each HTTP/1.1 field section |
| Maximum wire body bytes | 16 MiB | HTTP/1.1 collected request or response body |
| Maximum decoded body bytes | 16 MiB | Automatic gzip/deflate response decoding, enforced during expansion |
| Connection-pool max idle | 16 | Idle streams retained per pool |
| Connection-pool idle timeout | 60 seconds | Idle stream expiry during pool operations |
| Connection-pool max connection age | 300 seconds | Absolute stream lifetime during pool operations |

Passing `:timeout NIL`, `:idle-timeout NIL`, or `:max-connection-age NIL`
explicitly opts into an unbounded value. Passing `:max-body-bytes NIL` returns
to the corresponding finite default; specify a sufficiently large integer when
an application needs a larger body limit. The native
client offers only `http/1.1` through ALPN; HTTP/2 requires the explicit
`cl-http-kit/http2` transport boundary until native client dispatch is provided.

### `http-client-transport-function`

Return the injected client transport function.

### `http-client-default-headers`

Return the default request headers.

### `http-client-cookie-jar`

Return the client's cookie jar.

### `http-client-cache`

Return the client's cache.

### `http-client-strict-transport-store`

Return the client's HSTS policy store, or nil when disabled.

### `http-client-alternative-service-store`

Return the client's alternative-service store, or nil when disabled.

### `http-client-content-decoders`

Return the client's Content-Encoding decoder registry. Its coding names are
advertised in `Accept-Encoding` when automatic collected-body decoding applies.

### `http-client-redirect-policy`

Return the client's redirect policy.

### `http-client-retry-policy`

Return the client's retry policy.

### `http-client-proxy`

Return the client's proxy configuration.

### `http-client-auth-provider`

Return the client's preemptive authentication provider. The provider receives
the prepared request and may return an `Authorization` field value.

### `http-client-challenge-auth-provider`

Return the client's challenge authentication provider. After a 401 response,
the provider receives the request, response, and parsed `WWW-Authenticate`
challenges and may return an `Authorization` field value. The client performs
at most one challenge retry and only when no response body was streamed and the
request body is absent, directly replayable, or has a body factory.

### `http-client-proxy-challenge-auth-provider`

Return the client's proxy challenge authentication provider. After a 407
response, the provider receives the request, response, proxy plan, and parsed
`Proxy-Authenticate` challenges and may return a `Proxy-Authorization` field
value. The client applies the same single-retry and body-replay safeguards as
origin challenge authentication. This response-level hook covers forward
proxies and injected transports. The built-in CONNECT transport also parses a
rejected tunnel's challenges, closes that connection, and reopens the tunnel
at most once with the returned field value.

### `http-client-stale-while-revalidate-scheduler`

Return the optional RFC 5861 revalidation scheduler. When an eligible cached
response is stale, the scheduler receives the client, request, stale response,
and a zero-argument revalidation function. It must arrange asynchronous,
deduplicated execution and return true when accepted. A false return falls back
to synchronous validation. Request `no-cache`, `max-age`, and `min-fresh`
directives suppress this behavior.

### `http-client-clock-function`

Return the wall-clock function used for date-based policy decisions such as
Retry-After.

### `http-client-wall-clock-function`

Return the configured monotonic timing function.

### `http-client-sleep-function`

Return the sleep function used between retries.

### `http-client-random-function`

Return the random source used for retry jitter. It is called with `1.0` and
must return a real number greater than or equal to zero and less than one.

### `http-client-max-header-bytes`

Return the client's maximum response header size.

### `http-client-max-fields`

Return the client's maximum response field count per field section.

### `http-client-max-body-bytes`

Return the client's maximum response body size.

### `http-client-on-request`

Return the request hook.

### `http-client-on-response`

Return the response hook.

### `http-client-request`

Construct a validated core request with client defaults. The signature is
http-client-request (client method uri &key headers trailers body). TRAILERS
are sent after the request body when the selected transport supports request
trailers.

### `http-client-send`

Send a request through the client. The signature is
http-client-send (client request &key timeout deadline redirect-policy
retry-policy request-body-function request-body-factory request-body-length
on-body-chunk on-information collect-body-p cookie-same-site-p
cookie-top-level-navigation-p cookie-partition-key). It returns the response
and the effective request after redirects or retries. REQUEST-BODY-FUNCTION
produces octet chunks; REQUEST-BODY-FACTORY returns a fresh producer for each
retry or same-method redirect. A non-replayable producer is sent once.
ON-BODY-CHUNK and ON-INFORMATION receive response body and informational
responses, and COLLECT-BODY-P controls whether the response body is retained.
COOKIE-SAME-SITE-P and COOKIE-TOP-LEVEL-NAVIGATION-P supply the request context
for SameSite cookie selection. COOKIE-PARTITION-KEY supplies the canonical
top-level-site identifier for Partitioned cookie acceptance and selection. The
convenience forms are http-client-send (client method uri &rest arguments) and
http-client-send (client uri &key method ...); request options such as HEADERS,
TRAILERS, and BODY are consumed while the remaining keywords use the
request-object send contract.

## WebSocket package

The package name is HTTP-KIT/WEBSOCKET and it is included by the optional
`cl-http-kit/client` system.

### Frames and messages

`MAKE-WEBSOCKET-FRAME` constructs a validated frame. `SERIALIZE-WEBSOCKET-FRAME`
and `PARSE-WEBSOCKET-FRAME` convert between frames and wire octets;
`READ-WEBSOCKET-FRAME` and `WRITE-WEBSOCKET-FRAME` adapt the same operations to
binary streams. RSV bits, opcodes, control-frame finality and size, masking,
extended lengths, and configured payload limits are validated. Parsed masked
payloads are returned unmasked. `ALLOW-RSV1-P` is disabled by default and
permits RSV1 only on an initial text or binary frame.

`READ-WEBSOCKET-MESSAGE` consumes control frames while assembling one text or
binary message, including continuation frames, and can deliver control frames
to `ON-CONTROL`. `WRITE-WEBSOCKET-MESSAGE` fragments a text string or octet
vector and can produce one masking key per frame through
`MASKING-KEY-FUNCTION`. `DECOMPRESS-FUNCTION` and `COMPRESS-FUNCTION` enable
compressed-message handling. RSV1 is accepted or set only on the initial data
frame, control and continuation frames remain uncompressed, and expanded data
is checked against `MAX-MESSAGE-BYTES`. These codec functions receive and
return octet vectors and own RFC 7692 raw-DEFLATE, context-takeover,
window-size, and tail handling. `WEBSOCKET-PING` and `WEBSOCKET-PONG` write
control frames.

### Handshake and close helpers

`MAKE-WEBSOCKET-CLIENT-KEY` calls an injected cryptographically secure random
octet source for exactly 16 octets, validates its result, and returns the
Base64 client key. `WEBSOCKET-ACCEPT-KEY` calculates the RFC 6455 accept value.
`WEBSOCKET-UPGRADE-REQUEST-P` validates the HTTP/1.1 request headers and
version, while `WEBSOCKET-UPGRADE-RESPONSE` constructs a 101 response and
only selects a subprotocol or extension that the request offered.

`PARSE-WEBSOCKET-EXTENSIONS` returns normalized `WEBSOCKET-EXTENSION` objects
with name and parameter accessors. It rejects malformed separators, quoted
values, and duplicate parameters. `permessage-deflate` also enforces RFC 7692
parameter names, valueless takeover flags, window-bit ranges, and
offer/response asymmetry.

`MAKE-WEBSOCKET-UPGRADE-REQUEST` creates a client request from a URI, exactly
one of an already-generated Base64 `Sec-WebSocket-Key` or an injected secure
random-octet source, optional subprotocol tokens, optional serialized
extensions, and additional headers.
`WEBSOCKET-CLIENT-HANDSHAKE` sends that request on an already-open binary
stream, validates the 101 response, and leaves the stream positioned for
WebSocket frames. It returns the response and the transport's reusable flag;
the third value is the parsed negotiated extension list. The caller owns
dialing, TLS/ALPN, extension selection and codec state, and closing the stream.

`WEBSOCKET-VALID-CLOSE-CODE-P`, `MAKE-WEBSOCKET-CLOSE-PAYLOAD`, and
`PARSE-WEBSOCKET-CLOSE-PAYLOAD` validate close codes and UTF-8 reasons;
`WEBSOCKET-CLOSE` writes a close control frame. Masking direction, extension
selection and codec policy, Ping/Pong policy, and connection lifecycle remain
caller policy.

SERVE-WEBSOCKET-SESSION serves messages on an already-upgraded stream. It
dispatches complete text and binary messages to a handler, requires masked
client frames by default, automatically answers Ping frames, echoes a valid
peer Close frame, and reports protocol, size, or handler errors through the
close frame and ON-ERROR callback.
Supplying `DECOMPRESS-FUNCTION` enables negotiated compressed input while
retaining the post-decompression message-size limit.

## Server-Sent Events

Server-Sent Events support is included by the optional `cl-http-kit/client`
system and shares its `HTTP-KIT/CLIENT` package.

### `http-sse-event`

The server-sent-event structure type.

### `http-sse-event-p`

Return true when the object is an HTTP-SSE-EVENT.

### `make-http-sse-event`

Construct an event with make-http-sse-event (&key (event "message") (data "")
id retry comments). EVENT and DATA must be strings without line breaks; ID
must be a string without line breaks or NUL when supplied; RETRY must be a
non-negative integer when supplied; COMMENTS may be a string or a list of
strings and is emitted as SSE comment lines.

### `http-sse-event-event`

Return the event's field name, defaulting to "message".

### `http-sse-event-data`

Return the event's data string.

### `http-sse-event-id`

Return the event's optional ID string.

### `http-sse-event-retry`

Return the event's optional retry field as a non-negative integer.

### `http-sse-event-comments`

Return the event's associated comment lines.

### `parse-http-sse-events`

Parse a complete SSE body into a list of HTTP-SSE-EVENT values. The signature
is parse-http-sse-events (input &key max-events max-line-bytes
max-data-bytes max-event-bytes). INPUT is a UTF-8 string or an octet vector. The parser
recognizes CRLF, LF, and CR line endings, strips a leading UTF-8 BOM, and
discards an event that is not terminated by a blank line. Event IDs persist
across event blocks until another `id` field replaces them. Each limit is a
safety bound; `max-event-bytes` also bounds retained comment and metadata
lines, and passing NIL for a limit disables it.

### `read-http-sse-events`

Read SSE events from an already-open stream. The signature is
read-http-sse-events (stream &key max-events max-line-bytes max-data-bytes max-event-bytes
on-event (collect-events-p t)). STREAM may be binary or character. ON-EVENT,
when supplied, is called with each dispatched HTTP-SSE-EVENT as it arrives.
The call returns the collected event list when COLLECT-EVENTS-P is true,
otherwise NIL.

### `serialize-http-sse-event`

Serialize one HTTP-SSE-EVENT to UTF-8 octets ending in a blank line, suitable
for writing to an SSE response body. Comment lines are emitted first, then
event, id, retry, and one or more data lines split on the event's own line
breaks.

## Example

```lisp
(let ((request
        (http-kit:make-http-request
         :method "GET"
         :uri "https://example.test/health")))
  (http-kit:serialize-http-request request))
;; => #(71 69 84 32 47 104 101 97 108 116 104 32 72 84 84 80 47 49 46 49
;;      13 10 72 111 115 116 58 32 101 120 97 109 112 108 101 46 116 101
;;      115 116 13 10 67 111 110 116 101 110 116 45 76 101 110 103 116 104
;;      58 32 48 13 10 13 10)
```
