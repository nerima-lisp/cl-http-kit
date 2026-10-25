# API Reference

This page documents the primary exported symbols of the registered ASDF
systems. Package definition files remain authoritative for lower-level and
auxiliary exports not described here.

The registered systems in this checkout are version 0.4.1.

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

Parse an absolute HTTP or HTTPS URI into an HTTP-URI. An authority is required;
an omitted path defaults to `/`, and fragments are rejected. The parsed URI
uses a slash-prefixed path.

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

`http-header-values (headers name)` returns all matching values in order.
HEADERS must be a list of HTTP-HEADER values, not the header pairs accepted by
message constructors. NAME must be a non-empty ASCII token. Matching is
case-insensitive.

### `http-header-value`

`http-header-value (headers name &optional default)` returns the first matching
value, or DEFAULT (NIL when omitted). Inputs follow `http-header-values`.

### `http-header-present-p`

`http-header-present-p (headers name)` returns true when a matching header is
present. Inputs follow `http-header-values`.

### `http-request`

The request structure type.

### `http-request-p`

Return true when the object is an HTTP-REQUEST.

### `make-http-request`

```lisp
(make-http-request &key method protocol uri request-target headers trailers body
                       (protocol-version "HTTP/1.1"))
```

METHOD is required and must be a non-empty ASCII token; its case is preserved.
URI is required and accepts an HTTP-URI or a URI string. PROTOCOL is an optional
token normalized to lower case. REQUEST-TARGET is an optional non-empty string
of visible ASCII characters. PROTOCOL-VERSION accepts `"HTTP/1.0"`, `"HTTP/1.1"`,
`"HTTP/2"`, or `"HTTP/3"`.

HEADERS and TRAILERS accept lists containing HTTP-HEADER values, dotted pairs
such as `("Accept" . "text/plain")`, or two-item lists such as
`("Accept" "text/plain")`. They are normalized and copied. BODY accepts NIL,
a one-dimensional non-string vector or a list, with every element an octet
(an integer from 0 to 255); it is copied to an octet vector. Strings are not
accepted as bodies.
An omitted body becomes an empty octet vector. A `"TRACE"` request cannot have
a non-empty body.

### `http-request-protocol-version`

Return the protocol version string.

### `http-request-method`

Return the method string with its original case.

### `http-request-protocol`

Return the lower-case protocol token or NIL.

### `http-request-uri`

Return the HTTP-URI value.

### `http-request-target`

Return the explicit request target or NIL.

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

```lisp
(make-http-response &key status reason headers trailers body
                        (protocol-version "HTTP/1.1"))
```

STATUS is required and must be an integer from 100 to 599. An omitted REASON
uses the known reason phrase for the status, or an empty string. HEADERS,
TRAILERS, BODY, and PROTOCOL-VERSION follow `make-http-request`.

### `http-response-protocol-version`

Return the protocol version string.

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

### `http-response-stream`

The streaming response type accepted by a `serve-http1-session` handler.

### `http-response-stream-p`

Return true when the object is an HTTP-RESPONSE-STREAM.

### `make-http-response-stream`

```lisp
(make-http-response-stream &key status reason headers trailers body-function
                               body-length (protocol-version "HTTP/1.1"))
```

Response metadata follows `make-http-response`. BODY-FUNCTION is NIL or a
zero-argument function. For a response that sends a body, supply a function
that returns a one-dimensional non-string octet vector for each chunk and NIL
to finish. Lists are not accepted as chunks; an empty vector does not finish
the response. BODY-LENGTH is NIL or the non-negative total number of body
octets. For a response that sends a body, a mismatch between that length and
the produced body signals an error.

The HTTP/1 server uses the request's protocol version for the response. For a
response that sends a body, an explicit Content-Length requires a matching
BODY-LENGTH. Without explicit framing or trailers, a known length produces
Content-Length; an unknown length uses chunked framing for HTTP/1.1 and
connection-close framing for HTTP/1.0. Trailers require HTTP/1.1 chunked
framing and cannot be combined with Content-Length or a bodyless response.
The producer is not called for HEAD or other bodyless responses, including a
successful CONNECT response. For HEAD, BODY-LENGTH may describe the body that
would have been sent and supply Content-Length without invoking the producer.
A positive BODY-LENGTH is rejected for status 204, 205, or 304 and for a
successful CONNECT response.

### `http-response-stream-protocol-version`

Return the protocol version string.

### `http-response-stream-status`

Return the numeric response status.

### `http-response-stream-reason`

Return the response reason string.

### `http-response-stream-headers`

Return the normalized header list.

### `http-response-stream-trailers`

Return the normalized trailer list.

### `http-response-stream-body-function`

Return the body producer function or NIL.

### `http-response-stream-body-length`

Return the declared body length or NIL.

### `serialize-http-request`

Serialize a request to a one-dimensional octet vector using HTTP/1.1 request
framing. The serializer derives Host from the URI and validates body framing.

### `parse-http-request`

Parse an HTTP/1.0 or HTTP/1.1 request from an octet vector or binary input
stream. The signature is parse-http-request (input &key timeout deadline
max-header-bytes max-fields max-body-bytes default-authority on-body-chunk
reserve-body-bytes release-body-bytes (collect-body-p t) on-expect-continue
clock-function allow-eof-p). MAX-FIELDS defaults to 256 and applies
independently to request headers and trailers.

ON-BODY-CHUNK is NIL or a function receiving each decoded body chunk as an
octet vector; its return value is ignored. With COLLECT-BODY-P NIL, the parser
still consumes and validates the body and calls this callback, but returns an
empty body vector. ON-EXPECT-CONTINUE is NIL or a function receiving a request
with empty body and trailers before reading an HTTP/1.1 body with
Expect: 100-continue. It is called only for chunked framing or a positive
Content-Length; its return value is ignored. Body callbacks can run before
chunk terminators and trailers have been validated, so receiving a chunk does
not establish that parsing succeeded. ALLOW-EOF-P defaults to NIL. When true,
EOF before a request line returns NIL; incomplete requests still signal an error.
RESERVE-BODY-BYTES and RELEASE-BODY-BYTES are optional paired accounting
callbacks. The parser reserves storage before allocating body chunks and the
final body vector, then releases temporary collector storage after copying. A
caller that uses these callbacks must release the returned body's charge when
it no longer retains the request.

### `parse-http-response`

Parse an HTTP/1.0 or HTTP/1.1 response from an octet vector or binary input stream. The signature is
parse-http-response (input &key timeout deadline max-header-bytes
max-fields max-body-bytes request-method on-body-chunk on-information
(collect-body-p t) clock-function). It handles fixed-length,
chunked, close-delimited, and no-body responses subject to the supplied byte
and field-count limits. MAX-FIELDS defaults to 256 and applies independently
to each informational, final, and trailer field section.

Supply REQUEST-METHOD as the exact upper-case string `"HEAD"` or `"CONNECT"`
when applicable so the parser recognizes a bodyless HEAD response or a
successful CONNECT tunnel response. Method matching is case-sensitive.

ON-BODY-CHUNK and COLLECT-BODY-P follow `parse-http-request`. ON-INFORMATION
is NIL or a function receiving each informational HTTP-RESPONSE in wire
order, with empty body and trailers. Its return value is ignored. Parsing
continues until a final response or 101 Switching Protocols, which is returned
without calling ON-INFORMATION.

### `send-http-request-over-stream`

Send a request through an injected binary stream. The signature is
send-http-request-over-stream (request &key open-stream close-stream timeout
deadline max-header-bytes max-fields max-body-bytes request-target on-body-chunk
on-information request-body-function request-body-length (collect-body-p t)
clock-function). The synchronous opener has the signature
`(request &key timeout deadline)` and returns a binary stream;
`close-stream` receives that stream.

### `send-http-request-over-stream/cps`

The CPS form of send-http-request-over-stream. It takes
send-http-request-over-stream/cps (request on-success &key on-error open-stream
close-stream timeout deadline max-header-bytes max-fields max-body-bytes
request-target on-body-chunk on-information request-body-function
request-body-length (collect-body-p t) clock-function). It uses the same
synchronous stream opener, not a CPS opener. `on-success` receives the response;
`on-error`, when supplied, receives the condition instead of resignaling it.

### serve-http1-session

Serve HTTP/1.0 and HTTP/1.1 requests on an already-open binary stream. The
signature is serve-http1-session (stream handler &key timeout deadline
max-header-bytes max-fields max-body-bytes default-authority on-body-chunk
reserve-body-bytes release-body-bytes collect-body-p on-expect-continue
max-requests on-error on-upgrade close-stream clock-function). The handler
receives each parsed request and
returns an HTTP response or response stream. Its optional second value is an
ordered list of HTTP/1.1 informational responses (status 100-199 except 101)
to send before the final response. Informational responses are not sent to
HTTP/1.0 requests. The session supports request-body streaming, Expect:
100-continue, trailers, protocol upgrades, and bounded request counts.
MAX-FIELDS defaults to 256 and applies independently to request headers and
request trailers. The optional RESERVE-BODY-BYTES and RELEASE-BODY-BYTES
callbacks must be supplied together; the session uses them to account for
collected request bodies and releases each charge after processing the request.
It returns the number of final responses written and one of `:eof`, `:close`,
`:upgrade`, or `:max-requests`.

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

The CPS form of send-recorded-http-request. Its signature is
`send-recorded-http-request/cps (session request on-success &key on-error
timeout deadline max-header-bytes max-body-bytes clock-function)`.

### `http-deadline`

Compute an absolute deadline from a timeout. The signature is
http-deadline (timeout &key deadline clock-function). A nil timeout leaves an
inherited deadline unchanged; timeout values must be non-negative.
Timeouts are durations in seconds. Deadlines are absolute seconds on the
clock returned by `clock-function`, which defaults to a monotonic clock.
When both bounds are supplied, the earlier deadline applies.

### `with-http-deadline`

Bind an effective deadline around body forms. The macro signature is
with-http-deadline ((deadline timeout &key inherited clock-function kind)
&body body). It checks the deadline before running the body. Body operations
must check the bound during execution; the macro does not interrupt them or
check the deadline after they return. The default timeout kind is `:connect`.

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
For `:http2-read` errors caused by DATA on a client stream that has never been
opened, the detail is the stream ID when another request on the connection is
still in progress and has not failed; otherwise it is a two-element list with
`0` (the DATA frame type) first and the stream ID second.

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

The condition type for failures from a native or custom transport boundary.

### `http-connection-error-cause`

Return the underlying transport cause.

### `http-connection-error-request-sent-p`

Return whether the request may have been sent. This defaults to true; a
transport must guarantee a failure before transmission before supplying
`:request-sent-p nil`. Alt-Svc fallback also requires that no request-body
producer or response callback has run.

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

The CPS form of send-http2-request. Its signature is
`send-http2-request/cps (client request on-success &key on-error timeout
deadline max-header-bytes max-fields max-body-bytes clock-function
request-body-function request-body-length on-body-chunk collect-body-p
huffman-p)`.

### Reusable HTTP/2 connections

`make-http2-connection (&key stream close-stream max-frame-size
max-header-bytes max-fields max-body-bytes max-control-frames
max-control-window clock-function)` creates a connection over an already
negotiated binary stream.
`send-http2-request-over-connection (connection request &key timeout deadline
max-header-bytes max-fields max-body-bytes clock-function
request-body-function request-body-length on-body-chunk on-information
on-stream-open collect-body-p huffman-p)` sends one request.
`send-http2-requests-over-connection (connection requests &key timeout deadline
max-header-bytes max-fields max-body-bytes clock-function
request-body-functions request-body-lengths on-body-chunk collect-body-p
huffman-p)` multiplexes a non-empty batch. The CPS signatures are
`send-http2-request-over-connection/cps (connection request on-success &key
on-error timeout deadline max-header-bytes max-fields max-body-bytes
clock-function request-body-function request-body-length on-body-chunk
on-stream-open collect-body-p huffman-p)` and
`send-http2-requests-over-connection/cps (connection requests on-success &key
on-error timeout deadline max-header-bytes max-fields max-body-bytes
clock-function request-body-functions request-body-lengths on-body-chunk
collect-body-p huffman-p)`. Callers must serialize operations on this low-level
connection. On SBCL, use a connection manager or the high-level client for
requests shared across threads. `make-http2-connection-transport (connection)` adapts a
connection to the high-level client transport protocol.
`http2-client-connection (client)` returns the reusable connection owned by a
stream-backed client, when present. These functions accept `max-fields`; the
default per decoded header or trailer section is 256.

`cancel-http2-stream` emits `RST_STREAM` for an opened client stream.
`ping-http2-connection` sends a PING and waits for the matching acknowledgement.
If it receives a malformed PING while waiting, it signals
`HTTP-PROTOCOL-ERROR` with operation `:http2-client` and the received frame as
detail.
`send-http2-priority-update` emits an RFC 9218 priority update.
`send-http2-goaway` and its convenience alias
`graceful-shutdown-http2-connection` make the connection drain without
discarding already-created streams. `close-http2-connection` closes it
idempotently. PING and GOAWAY require a started, open session and raise
`HTTP-PROTOCOL-ERROR` after the connection is closed. GOAWAY's payload,
including its eight-byte fixed portion, must fit the peer's advertised
maximum frame size.

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

`max-hpack-table-size` clamps the peer's HPACK dynamic-table capacity. The client connection path
also clamps peer HPACK table settings to 4096 bytes by default through
`*h2-max-peer-hpack-table-size*`.

### HTTP/2 connection manager

`make-http2-connection-manager` constructs a reusable connection pool with
`&key open-connection max-connections idle-timeout max-connection-age
clock-function on-evict on-transient-close`. `max-connections` defaults to `8`
and limits retained cache entries; request-scoped connections do not count
toward that limit, so the total number of open connections can exceed it. On
SBCL, the manager synchronizes its entry state for concurrent callers. Ordinary
single-request calls with the same key can share one retained connection; its
scheduler assigns distinct stream IDs and observes the peer's concurrent-stream
and flow-control limits. A batch call holds an exclusive lease on its selected
connection. A concurrent call that cannot share an active entry uses a
request-scoped connection.
There is no separate cap on total open connections. Enforcing one would require
the manager to define how concurrent calls wait or fail when all sockets are in
use, so `max-connections` remains a retained-cache limit.
By default, the cache key includes the request scheme, host, port, proxy plan,
and proxy.
Supplying `connection-key` to a send function replaces that default; keys are
compared with `EQUAL`, so choose them to keep requests on the same reusable
connection only when their origin and proxy routes are compatible.

The open callback receives a request and must return an `http2-connection`.
Each invocation passes `:timeout` and `:deadline`; proxy-routed requests also
pass `:proxy-plan` and `:proxy`. Caller-supplied `open-connection`,
`clock-function`, `on-evict`, and `on-transient-close` functions may run
concurrently with manager calls, so they must synchronize any shared state
they use. For a different key, the manager can create a retained entry while
below the cap, or evict the least-recently-used idle entry at capacity. If the
cache is full and every retained entry is active, a request for another key
uses a request-scoped connection.

`idle-timeout`, `max-connection-age`, and `on-evict` default to `nil`. Idle
expiry is based on the time since the last completed manager call; maximum age
is based on the time since the connection was opened. The manager checks these
policies during manager operations; it does not use a background timer. Either
policy may retire an entry while a call is active. Retired entries are removed
from reuse immediately, and active calls finish before their connections
close.

When a retained entry leaves the cache, a supplied `on-evict` callback receives
its key. This includes expiry, capacity eviction, clear, close, and loss of
connection usability. The callback runs without the manager lock and may run
concurrently with other callbacks. Callback errors propagate after the
retirement actions have been attempted. A supplied `on-transient-close`
callback receives the key after a request-scoped connection closes when no
retained usable connection for that key remains. It also runs without the
manager lock and may run concurrently with other callbacks.

`send-http2-request-over-connection-manager` takes `(manager request)` and
accepts `&key connection-key timeout deadline max-header-bytes max-fields
max-body-bytes clock-function request-body-function request-body-length
on-body-chunk on-information collect-body-p huffman-p proxy-plan proxy`. It
retries once for an unprocessed GOAWAY or REFUSED_STREAM failure only when no
`request-body-function` is supplied.
`send-http2-requests-over-connection-manager` takes `(manager requests)` and
accepts `&key connection-key timeout deadline max-header-bytes max-fields
max-body-bytes clock-function request-body-functions request-body-lengths
on-body-chunk collect-body-p huffman-p`. It selects one connection using the
first request's default cache key, or the supplied `connection-key`, and sends
every request in the non-empty batch on that connection. The manager does not
select or validate a connection separately for each request; callers must
supply requests that the selected connection may serve. The underlying
session multiplexes the streams.
`make-http2-connection-manager-transport` adapts a manager to the high-level
client transport protocol. The two send functions and the function returned by
`make-http2-connection-manager-transport` each accept and forward the per-section
`max-fields` limit. Per-request `max-header-bytes`, `max-fields`, and
`max-body-bytes` values can tighten the selected connection's limits; they
cannot raise them.

While dispatching received response frames, malformed headers and body-limit
violations fail the connection and every active request on it. RFC 9113
§5.4.1 permits treating a stream error as a connection error. A per-request
timeout, callback error, explicit stream cancellation, or peer `RST_STREAM`
uses the stream-level handling path. If writing a required `RST_STREAM` fails,
the connection also fails.

`clear-http2-connection-manager` is idempotent. It retires
all retained connections without closing the manager and returns it on success.
A call whose connection was still opening when clear began may finish using
that connection, but it closes after the call releases its lease instead of
being retained.
`close-http2-connection-manager`
prevents new calls and closes retained connections after their active leases
are released.

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
max-header-bytes max-fields max-body-bytes max-aggregate-body-bytes
max-concurrent-streams
max-reset-streams max-reset-window max-control-frames max-control-window
max-hpack-table-size default-authority collect-body-p on-body-chunk
reserve-body-bytes release-body-bytes max-requests
on-error close-stream clock-function huffman-p enable-connect-p). The session
owns the HTTP/2 preface, SETTINGS, HPACK, stream state, flow control, request
body collection, and response framing, and returns a request count and
termination reason. `max-fields` defaults to 256 for each request header or
trailer section. `max-control-frames` defaults to 100 frames in a one-second
`max-control-window`; RST_STREAM is excluded from this budget, while all other
control frames, including SETTINGS, PING, PRIORITY, WINDOW_UPDATE, GOAWAY, and
PRIORITY_UPDATE, are counted.
Exceeding the budget sends GOAWAY with ENHANCE_YOUR_CALM before closing. The
control-frame limit keywords are accepted by `make-http2-client` and
`make-http2-connection`. The handler's primary value is the final response. Its
optional second value is a list of informational responses, which are emitted
in order before the final response; informational responses must use a status
from 100 through 199 other than 101 and cannot carry a body or trailers.
MAX-AGGREGATE-BODY-BYTES limits body bytes retained across the connection's
streams and defaults to twice MAX-BODY-BYTES. The optional paired
RESERVE-BODY-BYTES and RELEASE-BODY-BYTES callbacks let an outer listener
account for those buffers across connections.

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
ATTACH-HTTP3-PEER-CONTROL-STREAM accepts one incoming control stream while the
client is open. Closing retains the attachment identity; a closed client
rejects both an initial attachment and a replacement. Rejected streams remain
caller-owned.

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
are caller-owned and are not synchronized automatically. Public accessors
include `http3-client-open-p`; callback slots `http3-client-open-stream`,
`http3-client-write-stream`, `http3-client-read-stream`,
`http3-client-close-stream`, and `http3-client-cancel-stream`; limits
`http3-client-max-frame-size`, `http3-client-max-header-bytes`,
`http3-client-max-fields`, `http3-client-max-request-streams`,
`http3-client-max-peer-unidirectional-streams`,
`http3-client-max-state-bytes`, `http3-client-qpack-max-instruction-bytes`, and
`http3-client-qpack-max-buffer-bytes`; and state accessors
`http3-client-qpack-settings`, `http3-client-max-push-id`,
`http3-client-promised-push-ids`, `http3-client-cancelled-push-ids`, and
`http3-client-consumed-push-ids`.

MAKE-HTTP3-CLIENT also accepts MAX-REQUEST-STREAMS,
MAX-PEER-UNIDIRECTIONAL-STREAMS, MAX-STATE-BYTES,
QPACK-MAX-INSTRUCTION-BYTES, and QPACK-MAX-BUFFER-BYTES. Their finite defaults
are 100, 16, 4 MiB, 65,536 bytes, and 262,144 bytes respectively. The
client, server-stream, push, and connection-manager body-limit entry points
default MAX-BODY-BYTES to the core 16 MiB default; an explicitly supplied NIL
disables that limit for the call.

`make-http3-connection-manager (&key open-client max-connections idle-timeout
max-connection-age clock-function)` pools clients by request origin. Its
MAX-CONNECTIONS policy evicts the least recently used origin, while
IDLE-TIMEOUT and MAX-CONNECTION-AGE expire retained clients using a monotonic
CLOCK-FUNCTION. `send-http3-request-over-connection-manager (manager request
&key connection-key on-body-chunk on-information collect-body-p
max-header-bytes max-fields max-body-bytes request-body-function
request-body-length proxy proxy-plan qpack-encoder-context
qpack-decoder-context qpack-http3-decoder-context await-qpack huffman-p
on-stream-open on-push-promise timeout deadline)` preserves the
`send-http3-request` options, including streaming request bodies, and
`make-http3-connection-manager-transport (manager)` adapts the manager to a
high-level transport callback. Informational-response callbacks receive each
validated 1xx response in wire order; proxy routing is rejected rather than silently
ignored. Per-request MAX-HEADER-BYTES and MAX-FIELDS values can tighten but
never weaken the retained client's configured limits.
CLOSE-HTTP3-CONNECTION-MANAGER closes all retained clients and rejects
subsequent sends.

MAKE-HTTP3-QUIC-ADAPTER connects the HTTP/3 client boundary to a caller-owned
cl-quic-kit client. Its HTTP3-OPTIONS argument is a flat property list of
MAKE-HTTP3-CLIENT keyword arguments; malformed or non-keyword lists are
rejected before the QUIC client is created. QUIC, TLS, certificate, socket,
and shutdown policy remain caller-owned.
HOSTNAME must be a non-empty string identifying the origin for TLS SNI and
certificate verification. SERVER-HOST and SERVER-PORT select the UDP peer,
which can differ from that origin. Native sockets require a numeric IPv4
SERVER-HOST; HOSTNAME supplies the separate certificate name. ALPN defaults
to `("h3")`.
TLS-TRUST-ANCHORS is forwarded to cl-quic-kit. With the default TLS driver,
omitting anchors loads the environment or platform trust store in cl-tls-kit.
The adapter does not call the HTTP TLS upgrader's CA-file loading path.
It configures packet protection through the declared cl-quic-kit and
cl-crypto-kit dependencies before starting QUIC. A protection setup failure
propagates without sending network data.

Construction starts QUIC and synchronously waits for the TLS handshake and
peer HTTP/3 SETTINGS before returning. TIMEOUT bounds each wait separately;
DEADLINE is an absolute cutoff shared by the waits. When both are supplied,
each wait uses the earlier cutoff. Request stream writes wait for queued
bytes to be sent, not for peer acknowledgement or application receipt.

HTTP3-QUIC-ADAPTER-START starts the QUIC client once and returns the adapter;
construction has normally already started it. Starting a closed adapter
signals a protocol error. HTTP3-QUIC-ADAPTER-POLL
starts an open adapter if needed, forwards its optional clock value to QUIC,
and returns the adapter. Polling a closed adapter does not drive QUIC.
CLOSE-HTTP3-QUIC-ADAPTER closes the HTTP/3 streams and then the QUIC client,
forwarding ERROR-CODE and REASON. Repeated close calls return T without
closing QUIC again. HTTP/3 stream-cleanup errors do not prevent QUIC shutdown.

RECEIVE-HTTP3-PUSH accepts per-receive MAX-HEADER-BYTES, MAX-FIELDS, and
MAX-BODY-BYTES limits. Header limits can tighten but never weaken the HTTP/3
client's configured limits, matching ordinary response handling.

### serve-http3-request-stream

Serve one HTTP/3 request stream over caller-supplied QUIC stream callbacks.
The signature is serve-http3-request-stream (stream handler &key read-stream
write-stream close-stream max-frame-size max-header-bytes max-fields
max-body-bytes collect-body-p on-body-chunk qpack-decoder-table
qpack-decoder-context stream-id await-qpack qpack-serialize qpack-encoder-table
huffman-p enable-connect-p timeout deadline on-error). The session owns
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
The listener closes the accepted stream if STREAM-WRAPPER, ON-ACCEPT, or
session argument validation exits before the session takes cleanup ownership,
including on non-local exits. After that transfer, only the session invokes
CLOSE-STREAM; NIL retains the stream, and a successful upgrade transfers
ownership to ON-UPGRADE.

### HTTPS server

The optional `cl-http-kit/tls` system provides an HTTPS listener on SBCL.
`make-http-server`, `start-http-server`, `stop-http-server`, and
`http-server-port` are exported from `HTTP-KIT`. `make-http-server` has this
signature: `(&key (transport :https) host port handler
certificate-chain private-key ticket-key-store max-connections
max-handshakes max-concurrent-streams handshake-timeout request-timeout
idle-timeout graceful-timeout max-header-bytes max-fields max-body-bytes
max-buffered-body-bytes on-event on-error)`. HOST, PORT, HANDLER,
CERTIFICATE-CHAIN, and PRIVATE-KEY
are required. `certificate-chain` is a list of DER-encoded certificate byte
vectors with the leaf first; `private-key` is the matching P-256 ECDSA or
Ed25519 key object. Construct it with `crypto-kit:make-ec-private-key` using
`:p256` and scalar octets, or `crypto-kit:make-ed25519-private-key` using a
32-octet seed. The listener does not parse PEM or DER private-key files. PORT
may be zero to request an ephemeral port. The only supported transport is
`:https`.

The server advertises `h2` and `http/1.1` through ALPN and dispatches each
connection to the matching session. A handshake without one of those selected
protocols is rejected. The handler receives each HTTP request and returns
`(values response informational-responses)`. The second value may be omitted;
the response may be an HTTP response or response stream.

Defaults are 128 concurrent connections, 32 concurrent handshakes, 100 HTTP/2
streams per connection, a 10-second handshake timeout, a 30-second request
timeout, a 60-second idle timeout, and a 30-second graceful-stop timeout.
Request headers are limited to 65,536 bytes and 100 fields; request bodies are
limited to 16,777,216 bytes per request. The aggregate buffered-body limit
defaults to the larger of 64 MiB and twice MAX-BODY-BYTES. It applies across
active HTTP/1 and HTTP/2 sessions, including temporary copies made while
collecting a request. It does not account for application-owned copies or
references retained after a handler returns. Limits are finite and cannot be
disabled with NIL.
The handshake timeout runs from accept through TLS negotiation. The request
timeout covers request reads, handler execution, and response writes; the idle
timeout bounds the initial request wait and each wait between requests,
including the HTTP/2 preface and first SETTINGS frame. A handler that finishes
after its request deadline does not send a response. The graceful timeout bounds
shutdown from its start until remaining connections are closed.

`on-event` receives a sanitized plist with `:type` and `:phase`. Listener
events use `:listener-started`, `:listener-stopping`, and `:listener-stopped`;
the start event also includes `:host` and `:port`. Connection events use
`:connection-accepted`, `:connection-rejected`, `:alpn-selected`, and
`:connection-closed`; they include `:peer-address` and `:peer-port`, with
`:reason-code` on rejection and `:protocol` on ALPN selection and close.
`on-error` receives a condition and a context plist with `:phase`, `:protocol`,
`:peer-address`, and `:peer-port`. The callback payloads omit request contents
and secret material. Callback failures are reported as
HTTP-SERVER-CALLBACK-ERROR conditions.

`start-http-server` binds the listener, starts accepting connections, and
returns the server handle. `http-server-port` returns the actual bound port,
including when PORT was zero. `stop-http-server` stops accepting new
connections and asks active HTTP/2 sessions to send GOAWAY at their next
frame-processing point. Active responses can drain until the configured
graceful timeout; sessions still waiting for a frame are closed when it
expires. If a synchronous handler is still running, `stop-http-server` may
return while shutdown remains in progress. The server does not cancel handler
code and emits `:listener-stopped` only after every connection worker exits. It
is safe to call more than once. `http-server-p` identifies a server handle;
`http-server-port` returns its bound port and `http-server-failure`
returns a fatal server failure, if one occurred. `http-server-error-reason-code`
and `http-server-error-phase` expose safe error metadata, while
`http-server-callback-error-callback` identifies whether `:on-event` or
`:on-error` failed.

Each server creates and owns a ticket-key store by default. On the first TLS
connection after each 12-hour interval, the server replaces its owned store
and, after all connections using it finish, zeroizes the previous store's keys
and destroys the store. TLS session tickets encrypted with the previous store
cannot resume after rotation. The server also zeroizes its owned store's keys
and destroys the store after shutdown completes, once all connection workers
have exited.

Pass a store created with `cl-tls-kit:make-tls13-server-ticket-key-store` to
multiple server instances to share their TLS 1.3 session-ticket keys. Each
instance still enforces its own connection and handshake limits. A server does
not rotate or destroy a supplied store. The caller must rotate it with
`cl-tls-kit:rotate-tls13-server-ticket-key-store`, coordinate that rotation
across every server that shares the store, and stop those servers before
destroying the store.

## TLS package

The optional package name is HTTP-KIT/TLS and is provided by the
`cl-http-kit/tls` system. MAKE-HTTP-TLS-UPGRADER returns a client upgrade
callback backed by cl-tls-kit. Its VERIFY argument accepts :OPTIONAL or
:REQUIRED and defaults to :REQUIRED; NIL signals HTTP-UNSUPPORTED-FEATURE
with feature `:tls-certificate-verification-disable` because the pinned driver
does not expose a supported verification bypass. The callback passes the
request host as TLS SNI and accepts ALPN-PROTOCOLS, TIMEOUT, and DEADLINE
values. Each ALPN
protocol name must contain 1 to 255 ASCII characters. CERTIFICATE, KEY, and
PASSWORD are reserved for a future client-certificate integration and currently
signal an unsupported-feature condition when supplied. The client wrapper
preserves the binary-stream boundary and applies handshake deadlines.
:OPTIONAL applies the same certificate-chain, hostname, and trust checks as
:REQUIRED. Certificate verification remains enabled for every supported value.
TRUST-ANCHORS accepts parsed X.509 certificate objects. When anchors are not
supplied and VERIFY is :OPTIONAL or :REQUIRED, the upgrader loads PEM
certificates from the CA file selected by cl-tls-kit. SSL_CERT_FILE takes
precedence over NIX_SSL_CERT_FILE, followed by an existing platform CA file.
The wrapper rejects transport EOF without a peer close_notify instead of
treating a truncated TLS stream as a completed HTTP response.
The lambda list for `make-http-tls-server-wrapper` is
`(make-http-tls-server-wrapper &key certificate-chain private-key
ticket-key-store session-tickets-p alpn-protocols clock-function)`. The
certificate chain and private key are required; the clock defaults to a
monotonic clock. `session-tickets-p` defaults to true. Set it to NIL to prevent
the cl-tls-kit driver from creating a ticket-key store, resumption secret, or
`NewSessionTicket` message. It returns a callback invoked as
`(funcall wrapper stream &key timeout deadline)`. The callback completes the
TLS 1.3 server handshake and returns the negotiated TLS stream; `timeout` and
`deadline` bound the handshake.
HTTP-TLS-SELECTED-ALPN-PROTOCOL returns the protocol negotiated on an upgraded
client or server stream, or NIL when no protocol was negotiated. The HTTPS
listener accepts only `h2` and `http/1.1`.

The TLS package is TLS 1.3 only through cl-tls-kit. TLS 1.2 and OCSP/CRL are
outside the current boundary.
The TLS server wrapper ignores the record `legacy_record_version` as required
by RFC 8446 section 5.1 while continuing to validate record content types and
lengths.

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

Return the error counter. It uses `method` and `kind` labels. The `kind`
values are `timeout`, `protocol`, `connection`, and `error`.

### `call-with-http-observability/cps`

Wrap an operation with metrics collection. The signature is
call-with-http-observability/cps (metrics request operation on-success &key
on-error). The operation receives two continuations: a success function taking
one response and a failure function taking one condition. Invoke exactly one
continuation once. Each continuation records the outcome before calling the
corresponding caller callback; callback errors propagate without recording a
second outcome.

## Client package

The optional package name is HTTP-KIT/CLIENT. It builds request policies around
the core HTTP-KIT request and response values, with a native TCP/TLS connection
pool by default. Injected transports or pools own their network setup.

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

`resolve-http-uri (base reference)` returns an HTTP-URI. BASE accepts an
HTTP-URI or an absolute HTTP(S) URI string. REFERENCE accepts an HTTP-URI or a
URI-reference string, including a relative path or query. Fragments are removed;
absolute references must use HTTP or HTTPS.

### `http-uri-origin`

Return the scheme-and-authority origin for a URI, omitting the default HTTP
port 80 or HTTPS port 443. Explicit and omitted default ports identify the
same origin.

### `http-same-origin-p`

Return true when two URIs have the same origin.

### `http-percent-encode`

`http-percent-encode (string &key (safe "-._~") (space-as-plus-p nil))`
percent-encodes the string's UTF-8 bytes. ASCII letters and digits remain
literal, along with ASCII characters in SAFE, which accepts a string or NIL.
SPACE-AS-PLUS-P encodes spaces as `+` for form encoding; otherwise they become
`%20`.

### `http-form-urlencode`

`http-form-urlencode (fields)` returns an application/x-www-form-urlencoded
string. FIELDS is a list of dotted name/value pairs or two-element lists. Names
must be strings; values must be strings or NIL, with NIL encoded as an empty
value. Field order and duplicate names are preserved. Names and values use
UTF-8 percent encoding with spaces encoded as `+`.

### `http-form-urlencoded-octets`

`http-form-urlencoded-octets (fields)` accepts the same FIELDS as
`http-form-urlencode` and returns the encoded form as an octet vector.

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

`make-http-multipart-part (&key name value filename content-type)` constructs
a part. NAME is a required nonempty string; VALUE is a string or a
one-dimensional octet vector. FILENAME and CONTENT-TYPE each accept a string
or NIL. When the part is encoded, string values become UTF-8 bytes; vector
values are validated as octets.

### `http-multipart-part-name`

Return the multipart part field name.

### `http-multipart-part-value`

Return the multipart part value.

### `http-multipart-part-filename`

Return the optional multipart part filename.

### `http-multipart-part-content-type`

Return the optional multipart part content type.

### `make-http-multipart-body`

`make-http-multipart-body (parts &key boundary)` accepts a list of
HTTP-MULTIPART-PART values. It returns the body octets and
the corresponding Content-Type value with a quoted boundary parameter. An
explicit boundary must satisfy the RFC 2046 boundary grammar: one through
seventy ASCII boundary characters, with no trailing space. A boundary that
collides with a delimiter line in a part's value is rejected. When BOUNDARY is
omitted, the encoder generates a noncolliding boundary.

### `parse-http-multipart-body`

`parse-http-multipart-body (body &key content-type boundary (max-parts 1000)
(max-header-bytes 65536) max-body-bytes (max-input-bytes (* 64 1024 1024)))`
accepts a one-dimensional octet vector and returns a list of multipart parts
in wire order. Parsed part values are octet vectors, including text fields.
Supply CONTENT-TYPE with a boundary parameter, an explicit BOUNDARY, or both
with matching values. The parser validates
delimiter lines, requires exactly one Content-Disposition per part, rejects
duplicate Content-Type fields, and applies the configured per-part header,
aggregate part-body, part-count, and complete encoded-input limits. The input
limit defaults to 64 MiB and is checked before the parser copies the body.
Each limit accepts a nonnegative integer or NIL to disable that limit;
MAX-BODY-BYTES defaults to NIL.

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

Construct a redirect policy:

```lisp
(make-http-redirect-policy &key (max-redirects 10)
  (statuses '(301 302 303 307 308))
  (allow-downgrade-p nil) (preserve-authorization-p nil))
```

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

Construct a retry policy:

```lisp
(make-http-retry-policy &key (max-attempts 1)
  (methods '("GET" "HEAD" "OPTIONS" "PUT" "DELETE" "TRACE"))
  (statuses '(408 425 429 500 502 503 504))
  (base-delay 0.25) (max-delay 30.0) (jitter-ratio 0.0)
  (respect-retry-after-p t) (retry-on-timeout-p t)
  (retry-on-connection-error-p t))
```

`max-attempts` includes the first send, so its default disables retries.

### `http-retry-policy-max-attempts`

Return the maximum total send attempts, including the first send.

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

Process the first Strict-Transport-Security field for an HTTPS URI.
Malformed fields, HTTP URIs, and IP literals are ignored. The caller must
ensure that TLS certificate verification succeeded: this function checks the
URI scheme, not the transport's verification result. When using a custom
transport without that guarantee, disable the client's store with
`:strict-transport-store nil`.
Quoted `max-age` values are decoded before validating their digits. Empty
directives and valid unknown directives are ignored; semicolons inside quoted
values do not separate directives. A field without `max-age` does not change
the stored policy.

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
`http-alternative-service-persist-p`. The client selects an unexpired `h3`
alternative for its configured HTTP/3 transport. That transport owns dialing,
authenticating the origin certificate, sending origin SNI, and retaining the
applicable proxy policy.

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
During revalidation, a 304 response must identify the cached validator with a
matching `ETag` or `Last-Modified`. When the cached response has no validator,
the 304 response must also omit both fields. A mismatch raises
`http-protocol-error` rather than merging unrelated metadata.

### `http-cache-store`

Store a request and response in a cache. Responses carrying `no-store` are
not stored, except when `must-understand` is also present and the response
status is understood by the cache, as recommended by RFC 9111. Unknown status
codes carrying `must-understand` are not stored.

### `http-cache-invalidate`

Invalidate entries matching a request or URI.
The client also invalidates the request URI after an unsafe method receives a
2xx or 3xx response. Valid same-origin `Location` and `Content-Location` targets
are invalidated as well; malformed and cross-origin targets are ignored.

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
Explicit port suffixes must contain ASCII decimal digits and identify a port
from 1 through 65535. Malformed suffixes do not match a destination. Use
brackets for IPv6 rules with a port, such as `[::1]:443`; an unbracketed IPv6
address has no port restriction.

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
HTTP/1 streams and clears every HTTP/2 manager registered with that pool. It
returns the number of idle HTTP/1 streams closed. Active HTTP/2 connections are
retired immediately and close when their current request completes. Pool
operations may be used concurrently from multiple SBCL threads.

### `http-client`

The high-level client structure type.

### `http-client-p`

Return true when the object is an HTTP-CLIENT.

### `make-http-client`

Construct a client. The signature is
make-http-client (&key transport-function open-stream close-stream
connection-pool http3-transport-function
default-headers cookie-jar cache strict-transport-store
alternative-service-store redirect-policy
retry-policy proxy tls-upgrade resolve-host resolve-host-style auth-provider
challenge-auth-provider proxy-challenge-auth-provider
stale-while-revalidate-scheduler clock-function
wall-clock-function sleep-function random-function default-timeout max-header-bytes max-fields max-body-bytes
automatic-decompression-p content-decoders on-request on-response). At most one
of transport-function, open-stream, or connection-pool may be supplied;
omitting all three and close-stream selects the native TCP/TLS connection pool.
The connection pool owns pooled stream opening and closing; close-stream is
optional for the direct stream transport boundary. Omitting
strict-transport-store creates a new RFC 6797 store; explicitly passing nil
disables HSTS. Omitting alternative-service-store creates a new RFC 7838 store;
explicitly passing nil disables alternative-service discovery.

### `http-client-clear-connections`

Clear CLIENT's connection pool and all HTTP/2 managers registered with that
pool. Active HTTP/2 connections close when their current request completes.
Return the number of idle HTTP/1 connections closed.

By default, each client has a new cookie jar, no cache, and automatic
gzip/deflate response decoding enabled. `cookie-jar` must be an
`http-cookie-jar`; nil is not accepted. Supply an `http-cache` to enable
caching. Automatic decoding applies to collected response bodies without
`on-body-chunk`, except for HEAD responses, statuses 204/205/304, and successful
CONNECT responses. Each content decoder is called as
`(decoder octets max-body-bytes)`; custom decoders must enforce the supplied
limit themselves.

`resolve-host-style` is `:keywords` by default. In that mode, the resolver
receives HOST, `:timeout`, and `:deadline`. Set it to `:host-only` for a
legacy resolver that accepts only HOST. The resolver is not retried when its
body signals an error, so the style must match the callback.

The native default requires the optional `cl-http-kit/network` and
`cl-http-kit/tls` systems for TCP and HTTPS respectively. An injected
transport-function or connection-pool owns its network and TLS setup;
`tls-upgrade` and `resolve-host` cannot be supplied with those boundaries.
A direct `open-stream` boundary can use these two callbacks for proxy/TLS
composition. Its synchronous opener receives
`(request &key timeout deadline)` and returns a binary stream. The library's
proxy wrapper handles `proxy` and `proxy-plan` before calling this raw endpoint
opener; the request passed to the opener can target the proxy endpoint.

The transport-function returns an HTTP response and must support the complete
contract for any request features the application uses:

```lisp
(request &key timeout deadline max-header-bytes max-fields max-body-bytes
  proxy proxy-plan request-body-function request-body-length
  on-body-chunk on-information (collect-body-p t))
```

The first seven keywords, through `proxy-plan`, are always passed. The body
producer and length keywords are passed together when either is supplied;
response callbacks are passed only when supplied; `collect-body-p` is passed
only when false. A legacy transport supporting only the always-passed keywords
can serve requests that do not use these features.

`http3-transport-function` has the same contract plus `alternative-service`.
That keyword is always passed: a selected `http-alternative-service` for
Alt-Svc, or nil for an explicit HTTP/3 request. It must preserve origin
authentication and proxy policy, not authenticate only the alternative host.

### Client and HTTP/1.1 defaults

| Setting | Default | Scope |
| --- | ---: | --- |
| Client request timeout | 30 seconds | One request deadline covering DNS, connect, TLS, write, read, redirects, and retries |
| Maximum header bytes | 65,536 | HTTP/1.1 headers and trailers |
| Maximum fields | 256 | Each HTTP/1.1 field section |
| Maximum wire body bytes | 16 MiB | HTTP/1.1 request or response body, including streaming with `collect-body-p nil` |
| Maximum decoded body bytes | 16 MiB | Automatic gzip/deflate response decoding, enforced during expansion |
| Connection-pool max idle | 16 | Idle streams retained per pool |
| Connection-pool idle timeout | 60 seconds | Idle stream expiry during pool operations |
| Connection-pool max connection age | 300 seconds | Absolute stream lifetime during pool operations |

Passing `:timeout nil` removes the relative request timeout; an explicit
deadline still applies. Passing `:idle-timeout nil` or
`:max-connection-age nil` disables the corresponding pool expiry. For HTTP/1.1
parsing, `:max-body-bytes nil` selects the finite default. Automatic response
decoding instead passes nil to the decoder; built-in decoders omit their
explicit output-limit argument. Use a finite integer to explicitly bound
built-in gzip/deflate expansion. The native origin client offers `h2` and
`http/1.1` through ALPN and dispatches requests to the negotiated protocol. TLS
to an HTTPS proxy offers only `http/1.1`; custom transports supply their own
ALPN policy.

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

Return the optional request hook, called as `(hook request attempt)` before a
transport attempt.

### `http-client-on-response`

Return the optional response hook, called as `(hook response request attempt)`.
Cache hits and synthesized only-if-cached responses use attempt zero.

### `http-client-request`

```lisp
(http-client-request client method uri
                     &key headers trailers body
                          (protocol-version "HTTP/1.1"))
```

Construct a validated request using the client's default headers. Explicit
headers replace defaults with the same case-insensitive name. BODY accepts
nil, a string encoded as UTF-8, or a one-dimensional octet array. Trailers are
sent after the request body when the selected transport supports request
trailers.

### `http-client-send`

```lisp
(http-client-send client request-or-method &rest arguments)
```

Accept an `http-request` followed by send keywords, a method and URI followed
by keywords, or a URI followed by keywords. The URI-only form defaults to GET;
`:method` overrides it. Convenience forms accept string or symbol methods and
uppercase them. They consume `:headers`, `:trailers`, `:body`, and
`:protocol-version` as request options; remaining keywords use the
request-object send contract. The function returns the response and effective
request after redirects or retries.

Send keywords are TIMEOUT, DEADLINE, REDIRECT-POLICY, RETRY-POLICY,
REQUEST-BODY-FUNCTION, REQUEST-BODY-FACTORY, REQUEST-BODY-LENGTH,
ON-BODY-CHUNK, ON-INFORMATION, COLLECT-BODY-P, COOKIE-SAME-SITE-P,
COOKIE-TOP-LEVEL-NAVIGATION-P, and COOKIE-PARTITION-KEY.

REQUEST-BODY-FUNCTION is called with a chunk-size argument and returns an
octet chunk, or nil at end of input. REQUEST-BODY-FACTORY takes no arguments
and returns a fresh producer for every attempt using the streamed body,
including the first. The producer and factory are mutually exclusive and
cannot accompany a nonempty in-memory body. REQUEST-BODY-LENGTH requires a
producer or factory. A non-replayable producer is sent once.

ON-BODY-CHUNK is called with one body chunk; ON-INFORMATION is called with one
informational response. COLLECT-BODY-P defaults to true and controls whether
the response body is retained.
COOKIE-SAME-SITE-P and COOKIE-TOP-LEVEL-NAVIGATION-P supply the request context
for SameSite cookie selection. COOKIE-PARTITION-KEY supplies the canonical
top-level-site identifier for Partitioned cookie acceptance and selection.

## Example

```lisp
(let ((request
        (http-kit:make-http-request
         :method "GET"
         :uri "https://example.test/health")))
  (http-kit:serialize-http-request request))
```
