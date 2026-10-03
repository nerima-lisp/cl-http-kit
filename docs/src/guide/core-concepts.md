# Core Concepts

cl-http-kit keeps the HTTP protocol model small and explicit. Applications
construct values, choose a transport boundary, and decide how deadlines and
limits should be inherited.

The examples and boundaries here describe the checked-in 0.4.0 systems. The
convenience client path is available without removing
the compatibility APIs described in the reference pages.

## Message values

An HTTP URI contains a scheme, authority, path, and optional query. The core
parser accepts HTTP and HTTPS URIs, requires an authority, and normalizes the
path shape needed for an origin-form request target.

Requests contain a method, URI, headers, and an optional body. Responses contain
a status, reason, headers, trailers, and an optional body. Constructors
validate input and copy mutable sequences at the boundary.

## Headers

Header names are ASCII tokens and lookups are case-insensitive. Header values
are strings; control characters and invalid line breaks are rejected. Duplicate
field values are retained, so callers can choose between all values and the
first value.

The serializer supplies a Host field from the request URI. It rejects
conflicting or duplicated Host fields and validates Content-Length against the
body before writing the request.

## Binary bodies

Bodies are one-dimensional octet vectors or lists of octets. Strings are not
implicitly converted to bodies. Keeping the body representation binary makes
wire serialization deterministic and prevents character encoding from being
hidden inside the HTTP layer.

```lisp
(let ((request
        (http-kit:make-http-request
         :method "POST"
         :uri (http-kit:make-http-uri
               :scheme "https"
               :authority "example.test"
               :path "/data")
         :body #(1 2 3))))
  (list (http-kit:http-request-method request)
        (http-kit:http-request-body request)))
;; => ("POST" #(1 2 3))
```

## Deadlines and limits

Timeouts and deadlines are measured in seconds. A nil timeout or deadline is
unbounded; timeout values must be non-negative. A deadline is an absolute
monotonic-clock value and can be inherited by nested operations.

Header and body limits are byte limits. The parser raises a structured
size-limit condition when a limit is exceeded, including the configured limit,
the observed size, and whether headers or the body caused the failure.

## Direct and callback APIs

The direct APIs return a response or signal a condition. The CPS variants take
an explicit success callback and an optional error callback. Both styles use
the same request model, parser, serializer, limits, and transport arguments.

The transport boundary is injected. An application can provide an open binary
stream for HTTP/1.x or HTTP/2 without making socket, DNS, TLS, proxy, pooling,
or retry policy part of the core package. serve-http1-session and
serve-http2-session provide the matching server-side protocol sessions on
caller-owned streams, including framing, limits, body handling, and response
serialization. The optional network system provides one native SBCL TCP/DNS
boundary and an HTTP/1 listener service without changing that core contract.

The optional cl-http-kit/http3 system applies the same separation to HTTP/3:
it handles client request streams and one injected server request stream,
including request-stream frames and static, literal, and Huffman QPACK
representations with caller-owned dynamic tables. The application supplies
QUIC stream creation, byte writes, reads, closure, instruction-stream
synchronization, and connection-level stream dispatch. QUIC packet processing,
TLS, ALPN, and native HTTP/3 acceptance remain outside this system.

## High-level client policies

The optional `cl-http-kit/client` system builds request orchestration on the
core values. It provides URI resolution, authentication headers, cookies,
cache entries, proxy planning, redirects, bounded retries, and an HTTP/1.1
connection pool. Its `http-client` object accepts an application-provided
transport function, stream callbacks, or pool; when none is supplied, the
current integration constructs a native default boundary.

The client can return a response together with the effective request after a
redirect or retry. Applications that need stricter behavior can pass explicit
redirect, retry, cache, body, and header policies when constructing the client.

The 0.4.0 integration provides the URL-and-method convenience form through
`http-client-send` and the native TLS 1.3 client wrapper. Server-side TLS is
outside the current boundary because cl-tls-kit does not expose a server
driver. Deploy an HTTPS reverse proxy or load balancer in front of the native
HTTP listener when the server must accept TLS connections. HTTP/3 remains a
framing boundary until cl-quic-kit provides the QUIC connection layer.
