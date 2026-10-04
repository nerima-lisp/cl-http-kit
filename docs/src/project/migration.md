# Migration guide

This guide describes migration from the 0.3.x API to the 0.4.0 integration.
The current ASDF systems report version 0.4.0.

## Earlier 0.3.x boundary

The current high-level client permits `make-http-client` without a supplied
transport boundary and lazily creates a native TCP/TLS connection pool. Build a
request with `http-client-request` and execute it with `http-client-send`.
These compatibility entry points remain supported by 0.4.0, and
`http-client-send` also accepts a method and URL convenience form.

The current client already provides explicit policy helpers for proxy routing,
origin and proxy challenge authentication, gzip/deflate response decoding,
redirects, cookies, proxy environment lookup, and an HTTP/1.1 connection pool.
Pool and cookie state now has lock-backed access in the current integration.

Native TCP and DNS are available through `cl-http-kit/network`. The separate
`cl-http-kit/tls` source now adapts cl-tls-kit and cl-crypto-kit for the TLS 1.3
client driver. The client system uses cl-deflate-kit for gzip and deflate.
Server-side TLS remains an explicit unsupported boundary because the kit does
not expose a server driver. For HTTPS servers, terminate TLS in a reverse
proxy or load balancer and forward plain HTTP to `cl-http-kit/network`.
HTTP/2 and the base HTTP/3 APIs use injected stream or exchange boundaries.
The optional cl-quic-kit client adapter can open and drive a QUIC connection
when explicitly supplied; QUIC setup and server-side accept remain outside the
base framing API.

## 0.4.0 changes

The 0.4.0 client path accepts a URL and method as its essential inputs and
keeps native TCP/TLS, pooling, redirects, cookies, and content coding enabled
by default. Existing callers that need custom transports can continue to use
`make-http-client` and `http-client-send`.

The dependency and policy changes are:

- cl-tls-kit replaces the legacy external TLS integration;
- cl-deflate-kit replaces the legacy external compression integration;
- cl-crypto-kit supplies cryptographic primitives used by the TLS and
  authentication stack;
- proxy selection reads `HTTP_PROXY`, `HTTPS_PROXY`, and `NO_PROXY`, together
  with lowercase spellings, and supports HTTP CONNECT for HTTPS targets;
- Basic and Digest authentication are available for origin and proxy
  challenges, subject to replay safety;
- pool and cookie-jar state is safe for concurrent use;
- the native client offers only `http/1.1` through ALPN; HTTP/2 uses the
  explicit `cl-http-kit/http2` transport boundary until native dispatch exists.

Automatic HTTP/3 selection remains out of this migration. The existing HTTP/3
framing and injected-QUIC boundary remains the integration seam, and the
optional cl-quic-kit adapter provides explicit client connection setup.

## Migration checklist

1. Keep custom transport code on the complete callback keyword contract until
   the native client path is available.
2. Replace direct legacy TLS/compression setup with the four-kit systems when
   updating dependencies; do not mix the old wrappers with the new default
   path.
3. If an application shared a pool or cookie jar between threads, remove its
   external serialization only after the 0.4.0 thread-safe implementation is
   present and tested.
4. Treat 0.4.0 as a compatibility review point for redirects, authentication
   replay, proxy environment precedence, and HTTP/2 selection.
