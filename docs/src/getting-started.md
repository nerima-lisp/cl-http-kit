# Getting Started

cl-http-kit is loaded through ASDF. The core system has no built-in socket
dependency: an application supplies the binary stream or exchange boundary.
The instructions on this page describe the checked-in 0.4.0 API. The
migration notes are documented in the
[migration guide](project/migration.md).

## Load the core system

```lisp
(asdf:load-system "cl-http-kit")
```

The core package is named HTTP-KIT:

```lisp
(http-kit:make-http-uri
 :scheme "https"
 :authority "example.test"
 :path "/health")
```

## Make a deterministic request

A recording session supplies responses in order and is useful for examples and
tests:

```lisp
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
                 :body #(79 75))))))
  (let ((response (http-kit:send-recorded-http-request session request)))
    (list (http-kit:http-response-status response)
          (http-kit:http-response-body response))))
;; => (200 #(79 75))
```

The response body is a one-dimensional octet vector. Requests and responses are
copied at the model boundary, so later mutation of a caller-owned sequence does
not alter the stored message.

The recording session keeps the requests it accepted:

```lisp
(http-kit:recording-session-requests session)
```

## Connect a real transport

For HTTP/1.1, call http-kit:send-http-request-over-stream with an
:open-stream callback. The callback receives the request and :timeout and
:deadline keyword arguments; it should return a binary stream. :close-stream
receives that stream after the exchange. Socket creation, TLS, proxy setup, and
pooling can therefore be supplied by the surrounding application.

The parser and transport accept optional :max-header-bytes and
:max-body-bytes limits. Pass them explicitly when an application needs a
stable or different policy; the current values are documented with the core
parsing and transport APIs.

For the checked-in API, an SBCL-native TCP and DNS boundary is supplied
by the optional network system and can be connected to the client pool:

```lisp
(asdf:load-system "cl-http-kit/network")
(asdf:load-system "cl-http-kit/client")

(http-kit/client:make-http-client
 :open-stream (http-kit/network:make-http-network-stream-opener)
 :close-stream #'http-kit/network:close-http-tcp-stream)
```

The same optional system exposes a native listening boundary for servers:

```lisp
(let ((listener (http-kit/network:open-http-tcp-listener
                 :host "127.0.0.1" :port 8080)))
  (unwind-protect
       (http-kit/network:serve-http1-listener
        listener
        (lambda (request)
          (declare (ignore request))
          (http-kit:make-http-response
           :status 200
           :body #(79 75)))
        :max-connections 1
        :session-options (list :max-requests 1)))
    (http-kit/network:close-http-tcp-listener listener)))
```

Passing `:port 0` asks the operating system for an ephemeral port; read it
with `http-network-listener-port`. `serve-http1-listener` owns accept and
HTTP/1 session dispatch for the requested number of connections; applications
that need HTTP/2 or protocol selection can keep using
`accept-http-tcp-stream` and dispatch to `serve-http2-session` themselves.

The socket boundary itself does not negotiate TLS, ALPN, or proxies. Load
`cl-http-kit/tls` and compose `http-kit/tls:make-http-tls-upgrader` with the
client opener. The wrapper defaults to `:verify :required`, passes the request
host as TLS SNI, loads the platform trust store, and accepts an ALPN offer
list. Server TLS wrapping signals an explicit unsupported-feature condition
because cl-tls-kit does not expose a server driver. For an HTTPS server,
terminate TLS in a reverse proxy or load balancer and forward HTTP to the
listener above. Configure that proxy to preserve the original host and scheme
according to the application's trusted-forwarding policy; cl-http-kit does not
validate proxy headers automatically.

The high-level client creates this native TCP/TLS path automatically when no
custom transport is supplied.

## Optional systems

Load HTTP/2 support when the application owns an HTTP/2-capable exchange or
stream:

```lisp
(asdf:load-system "cl-http-kit/http2")
(asdf:load-system "cl-http-kit/client")
```

The client system adds URI, authentication, RFC 9110 challenge parsing and safe
single-retry origin and forward-proxy authentication, cookie, cache, proxy,
redirect, and retry policies around the core messages. Collected response bodies are
automatically decoded for `gzip` and `deflate` content codings, and eligible
requests advertise those codings with `Accept-Encoding`; pass
`:automatic-decompression-p nil` to `make-http-client` to retain the encoded
body. The body-size limit is enforced while decoding, so compressed responses
cannot allocate an unbounded expanded body. Streaming body callbacks
receive transport bytes and are not automatically decoded. The client still
receives an application-provided transport function or stream callback.

Load HTTP/3 request-stream support when the application supplies QUIC stream
callbacks with ASDF system cl-http-kit/http3.

This system writes the HTTP/3 control-stream prefix and SETTINGS, encodes and
decodes client request streams, and provides a server session for one injected
request stream. It provides static, literal, and Huffman QPACK representations
plus caller-owned dynamic tables. Dynamic-table instruction streams are exposed
to the caller and are not synchronized automatically. The system does not
implement QUIC packets, loss recovery, native sockets, or native HTTP/3 server
acceptance. The optional cl-quic-kit adapter supplies client connection and
stream setup when explicitly configured; certificate, socket, and server-accept
policy remain application-owned.

Load metrics integration when cl-observability-kit is available:

```lisp
(asdf:load-system "cl-http-kit/observability")
```

Continue with [Core Concepts](guide/core-concepts.md) for the value model, or
the [API Reference](reference/api.md) for the primary exported symbols. The
[migration guide](project/migration.md) records the 0.4.0 compatibility
boundary and the optional cl-quic-kit HTTP/3 client adapter.
