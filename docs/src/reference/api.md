# API Reference

This page documents the primary exported symbols of the registered ASDF
systems. Package definition files remain authoritative for lower-level and
auxiliary exports not described here. Common arguments such as timeout,
deadline, byte limits, and clock-function are accepted by the direct and CPS
transport APIs where shown.

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

### `parse-http-response`

Parse an HTTP/1.1 response from an octet vector or binary input stream. The signature is
parse-http-response (input &key timeout deadline max-header-bytes
max-body-bytes request-method clock-function). It handles fixed-length,
chunked, close-delimited, and no-body responses subject to the supplied byte
limits.

### `send-http-request-over-stream`

Send a request through an injected binary stream. The signature is
send-http-request-over-stream (request &key open-stream close-stream timeout
deadline max-header-bytes max-body-bytes clock-function). The open callback
receives the request and effective timeout and deadline; close-stream receives
the returned stream.

### `send-http-request-over-stream/cps`

The CPS form of send-http-request-over-stream. It takes
send-http-request-over-stream/cps (request on-success &key on-error open-stream
close-stream timeout deadline max-header-bytes max-body-bytes clock-function).

### serve-http1-session

Serve HTTP/1.0 and HTTP/1.1 requests on an already-open binary stream. The
signature is serve-http1-session (stream handler &key timeout deadline
max-header-bytes max-body-bytes default-authority on-body-chunk
collect-body-p on-expect-continue max-requests on-error on-upgrade
close-stream clock-function). The handler receives each parsed request and
returns an HTTP response or response stream. The session supports request-body
streaming, Expect: 100-continue, trailers, protocol upgrades, and bounded
request counts. It returns the number of responses written and one of
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
close-stream max-frame-size max-header-bytes max-body-bytes clock-function).
Exactly one of exchange or open-stream must be supplied. Frame size must be
between 16384 and 16777215.

### `send-http2-request`

Send one request through an HTTP/2 client. The signature is
send-http2-request (client request &key timeout deadline max-header-bytes
max-body-bytes clock-function).

### `send-http2-request/cps`

The CPS form of send-http2-request. It takes client, request, on-success, and
optional on-error, timeout, deadline, byte-limit, and clock arguments.

### serve-http2-session

Serve HTTP/2 requests on a caller-supplied binary stream. The signature is
serve-http2-session (stream handler &key timeout deadline max-frame-size
max-header-bytes max-body-bytes default-authority collect-body-p
on-body-chunk max-requests on-error close-stream clock-function). The session
owns the HTTP/2 preface, SETTINGS, HPACK, stream state, flow control, request
body collection, and response framing, and returns a request count and
termination reason.

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
appropriate. CLOSE-HTTP3-CLIENT closes the control stream.

SEND-HTTP3-REQUEST takes an HTTP request and optional ON-BODY-CHUNK,
COLLECT-BODY-P, MAX-BODY-BYTES, QPACK-ENCODER-TABLE, QPACK-DECODER-TABLE,
HUFFMAN-P, TIMEOUT, and DEADLINE arguments. It opens a request stream, writes
request headers, DATA, and trailers, then parses the response. The QPACK
tables are caller-owned and are not synchronized automatically. HTTP3-CLIENT
accessors expose the callback slots, limits, settings, and open state.

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
the corresponding Content-Type value.

## Date and authentication

### `http-parse-date`

Parse an HTTP date string and return universal time, or nil for an invalid
value.

### `http-basic-authorization`

Construct a Basic authorization value from a username and password.

### `http-bearer-authorization`

Construct a Bearer authorization value from a token.

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

### `http-cookie-host-only-p`

Return true when the cookie is host-only.

### `http-cookie-creation-time`

Return the cookie creation time.

### `http-cookie-partitioned-p`

Return true when the cookie is partitioned.

### `http-cookie-partition-key`

Return the top-level site key associated with a partitioned cookie.

### `http-cookie-jar`

The cookie-jar structure type.

### `make-http-cookie-jar`

Construct a cookie jar. The optional clock-function is used for expiry and
ordering decisions.

### `http-cookie-jar-cookies`

Return the cookie snapshot held by a jar.

### `http-cookie-jar-accept-response`

Accept response cookies for a request URI. The signature is
http-cookie-jar-accept-response (jar request-uri response &key now
partition-key).

### `http-cookie-jar-cookie-header`

Return the Cookie header value applicable to a request URI. The request path can
also supply NOW, PARTITION-KEY, SAME-SITE-CONTEXT, and METHOD to enforce
partitioned and SameSite delivery rules.

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

Construct a retry policy with method and status allowlists, delay bounds, and
timeout and connection-error behavior.

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

### `http-retry-policy-respect-retry-after-p`

Return whether Retry-After is honored.

### `http-retry-policy-retry-on-timeout-p`

Return whether timeout conditions are retryable.

### `http-retry-policy-retry-on-connection-error-p`

Return whether connection conditions are retryable.

## Content coding and protocol helpers

### `http-content-coding`

The content-coding adapter structure type.

### `http-content-coding-p`

Return true when the object is an HTTP-CONTENT-CODING.

### `make-http-content-coding`

Construct an adapter with make-http-content-coding (&key name encoder decoder).
The application supplies encoder and decoder functions; this package does not
choose a compression implementation.

### `http-content-coding-name`

Return the normalized lower-case content-coding token.

### `http-content-coding-encoder`

Return the encoder function, when present.

### `http-content-coding-decoder`

Return the decoder function, when present.

### `parse-http-accept-encoding`

Parse an Accept-Encoding header into a list of (NAME . QUALITY) pairs.

### `http-select-content-coding`

Select the best supported content coding for an Accept-Encoding value. It
returns the original supported coding object, `"identity"`, or NIL.

### `http-content-coding-encode`

Encode octets with an HTTP-CONTENT-CODING adapter.

### `http-content-coding-decode`

Decode octets with an HTTP-CONTENT-CODING adapter.

### `http-alpn-protocol-name`

Normalize an ALPN protocol alias such as `:http1`, `:http2`, or `:http3` to
its wire token.

### `http-select-protocol`

Select the first offered ALPN protocol that is also supported locally.

## Cache

### `http-cache`

The cache structure type.

### `make-http-cache`

Construct a cache. The signature is
make-http-cache (&key (max-entries 256) (clock-function #'get-universal-time)).

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
response when available, a state of :FRESH, :STALE, :STALE-ALLOWED, or :MISS,
and the matching cache entry when available.

### `http-cache-store`

Store a request and response in a cache.

### `http-cache-invalidate`

Invalidate entries matching a request or URI.

### `http-cache-clear`

Remove all entries from a cache.

## Proxy

### `http-proxy`

The proxy structure type.

### `make-http-proxy`

Construct a proxy from a scheme, host, port, credentials, and no-proxy rules.

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

Return true when a URI matches a proxy bypass rule.

### `http-proxy-plan`

Construct the proxy plan for a URI.

## Client

### `http-client`

The high-level client structure type.

### `http-client-p`

Return true when the object is an HTTP-CLIENT.

### `make-http-client`

Construct a client. The signature is
make-http-client (&key transport-function open-stream close-stream
connection-pool
default-headers cookie-jar cache redirect-policy retry-policy proxy
auth-provider clock-function wall-clock-function sleep-function
max-header-bytes max-body-bytes cookie-partition-key
cookie-same-site-context on-request on-response). Exactly one
of transport-function, open-stream, or connection-pool must be supplied.
The connection pool owns pooled stream opening and closing; close-stream is
optional for the direct stream transport boundary.

The injected transport-function must accept the complete keyword boundary used
by the client, including timeout, deadline, limits, proxy context, request-body
callbacks, response callbacks, and collect-body-p. Partial legacy keyword
signatures are intentionally unsupported.

### `http-client-transport-function`

Return the injected client transport function.

### `http-client-default-headers`

Return the default request headers.

### `http-client-cookie-jar`

Return the client's cookie jar.

### `http-client-cache`

Return the client's cache.

### `http-client-cookie-partition-key`

Return the default top-level partition key used for cookie policy.

### `http-client-cookie-same-site-context`

Return the default SameSite request context used for cookie policy.

### `http-client-redirect-policy`

Return the client's redirect policy.

### `http-client-retry-policy`

Return the client's retry policy.

### `http-client-proxy`

Return the client's proxy configuration.

### `http-client-auth-provider`

Return the client's authentication provider.

### `http-client-clock-function`

Return the wall-clock function used for date-based policy decisions such as
Retry-After.

### `http-client-wall-clock-function`

Return the configured monotonic timing function.

### `http-client-sleep-function`

Return the sleep function used between retries.

### `http-client-max-header-bytes`

Return the client's maximum response header size.

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
on-body-chunk on-information collect-body-p). It returns the response and the
effective request after redirects or retries. REQUEST-BODY-FUNCTION produces
octet chunks; REQUEST-BODY-FACTORY returns a fresh producer for each retry or
same-method redirect. A non-replayable producer is sent once. ON-BODY-CHUNK
and ON-INFORMATION receive response body and informational responses, and
COLLECT-BODY-P controls whether the response body is retained.

## WebSocket package

The package name is HTTP-KIT/WEBSOCKET and it is included by the optional
`cl-http-kit/client` system.

### Frames and messages

`MAKE-WEBSOCKET-FRAME` constructs a validated frame. `SERIALIZE-WEBSOCKET-FRAME`
and `PARSE-WEBSOCKET-FRAME` convert between frames and wire octets;
`READ-WEBSOCKET-FRAME` and `WRITE-WEBSOCKET-FRAME` adapt the same operations to
binary streams. RSV bits, opcodes, control-frame finality and size, masking,
extended lengths, and configured payload limits are validated. Parsed masked
payloads are returned unmasked.

`READ-WEBSOCKET-MESSAGE` consumes control frames while assembling one text or
binary message, including continuation frames, and can deliver control frames
to `ON-CONTROL`. `WRITE-WEBSOCKET-MESSAGE` fragments a text string or octet
vector and can produce one masking key per frame through
`MASKING-KEY-FUNCTION`. `WEBSOCKET-PING` and `WEBSOCKET-PONG` write control
frames.

### Handshake and close helpers

`WEBSOCKET-ACCEPT-KEY` calculates the RFC 6455 accept value.
`WEBSOCKET-UPGRADE-REQUEST-P` validates the HTTP/1.1 request headers and
version, while `WEBSOCKET-UPGRADE-RESPONSE` constructs a 101 response and
only selects a subprotocol that the request offered.

`MAKE-WEBSOCKET-UPGRADE-REQUEST` creates a client request from a URI, an
already-generated Base64 `Sec-WebSocket-Key`, optional subprotocol tokens,
optional serialized extensions, and additional headers. The caller supplies
the key so its random source can be cryptographically secure.
`WEBSOCKET-CLIENT-HANDSHAKE` sends that request on an already-open binary
stream, validates the 101 response, and leaves the stream positioned for
WebSocket frames. It returns the response and the transport's reusable flag;
the caller owns dialing, TLS/ALPN, extension negotiation, and closing the
stream.

`WEBSOCKET-VALID-CLOSE-CODE-P`, `MAKE-WEBSOCKET-CLOSE-PAYLOAD`, and
`PARSE-WEBSOCKET-CLOSE-PAYLOAD` validate close codes and UTF-8 reasons;
`WEBSOCKET-CLOSE` writes a close control frame. Masking direction, extension
negotiation, Ping/Pong policy, and connection lifecycle remain caller policy.

SERVE-WEBSOCKET-SESSION serves messages on an already-upgraded stream. It
dispatches complete text and binary messages to a handler, requires masked
client frames by default, automatically answers Ping frames, echoes a valid
peer Close frame, and reports protocol, size, or handler errors through the
close frame and ON-ERROR callback.

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
max-data-bytes). INPUT is a UTF-8 string or an octet vector. The parser
recognizes CRLF, LF, and CR line endings, strips a leading UTF-8 BOM, and
dispatches a final event at end of input even without a trailing blank line.
Each limit is a safety bound; passing NIL for a limit disables it.

### `read-http-sse-events`

Read SSE events from an already-open stream. The signature is
read-http-sse-events (stream &key max-events max-line-bytes max-data-bytes
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
