# Getting Started

cl-http-kit is loaded through ASDF. The core system has no built-in socket
dependency: an application supplies the binary stream or exchange boundary.

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

For an SBCL-native TCP and DNS boundary, load the optional network system and
connect it to the client pool:

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

This boundary deliberately does not negotiate TLS, ALPN, or proxies. Supply
those policies through the client's callbacks or use an application-owned
transport when they are required.

## Optional systems

Load HTTP/2 support when the application owns an HTTP/2-capable exchange or
stream:

```lisp
(asdf:load-system "cl-http-kit/http2")
(asdf:load-system "cl-http-kit/client")
```

The client system adds URI, authentication, cookie, cache, proxy, redirect,
and retry policies around the core messages. It still receives an
application-provided transport function or stream callback.

Load HTTP/3 request-stream support when the application supplies QUIC stream
callbacks with ASDF system cl-http-kit/http3.

This system writes the HTTP/3 control-stream prefix and SETTINGS, encodes and
decodes client request streams, and provides a server session for one injected
request stream. It provides static, literal, and Huffman QPACK representations
plus caller-owned dynamic tables. Dynamic-table instruction streams are exposed
to the caller and are not synchronized automatically. The system does not
implement QUIC packets, loss recovery, TLS, ALPN, native sockets, or native
HTTP/3 connection and stream acceptance; the QUIC layer must provide those
callbacks.

Load metrics integration when cl-observability-kit is available:

```lisp
(asdf:load-system "cl-http-kit/observability")
```

Continue with [Core Concepts](guide/core-concepts.md) for the value model, or
the [API Reference](reference/api.md) for the primary exported symbols.
