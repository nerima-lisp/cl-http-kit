# Migration guide

This guide describes the planned migration from the checked-in 0.3.x API to
the unreleased 0.4.0 candidate. The current ASDF systems still report version
0.3.0, so apply the candidate steps only after the corresponding integration
code and dependency changes land.

## Current 0.3.x boundary

The current high-level client permits `make-http-client` without a supplied
transport boundary and lazily creates a native TCP/TLS connection pool. Build a
request with `http-client-request` and execute it with `http-client-send`.
These compatibility entry points remain supported by the candidate; a
separate public function accepting only a URL and method is still missing.

The current client already provides explicit policy helpers for proxy routing,
origin and proxy challenge authentication, gzip/deflate response decoding,
redirects, cookies, proxy environment lookup, and an HTTP/1.1 connection pool.
Pool and cookie state now has lock-backed access in the current integration.

Native TCP and DNS are available through `cl-http-kit/network`. The separate
`cl-http-kit/tls` source currently still calls cl+ssl, although its ASDF
dependency declaration has moved to cl-tls-kit and cl-crypto-kit. The client
system declares cl-deflate-kit rather than chipz. Treat this dependency change
as incomplete until the TLS source and its checks use the declared kits.
HTTP/2 and HTTP/3 use injected stream or exchange boundaries; HTTP/3 does not
open QUIC connections.

## 0.4.0 candidate changes

The target client path accepts a URL and method as its essential inputs and
keeps native TCP/TLS, pooling, redirects, cookies, and content coding enabled
by default. Existing callers that need custom transports can continue to use
`make-http-client` and `http-client-send`.

The dependency and policy changes are:

- cl-tls-kit replaces the cl+ssl TLS integration;
- cl-deflate-kit replaces chipz for gzip and deflate;
- cl-crypto-kit supplies cryptographic primitives used by the TLS and
  authentication stack;
- proxy selection reads `HTTP_PROXY`, `HTTPS_PROXY`, and `NO_PROXY`, together
  with lowercase spellings, and supports HTTP CONNECT for HTTPS targets;
- Basic and Digest authentication are available for origin and proxy
  challenges, subject to replay safety;
- pool and cookie-jar state is safe for concurrent use;
- HTTP/2 is selected through the native client protocol path with ALPN
  `h2`/`http/1.1` and the documented fallback rules.

Native HTTP/3 is deliberately out of this migration until cl-quic-kit is
complete. The existing HTTP/3 framing and injected-QUIC boundary remains the
integration seam; native H3 connection setup will be documented separately
when that transport is available.

## Migration checklist

1. Keep custom transport code on the complete callback keyword contract until
   the native client path is available.
2. Replace direct cl+ssl/chipz setup with the four-kit systems when updating
   dependencies; do not mix the old TLS/compression wrappers with the new
   default path.
3. If an application shared a pool or cookie jar between threads, remove its
   external serialization only after the 0.4.0 thread-safe implementation is
   present and tested.
4. Treat 0.4.0 as a compatibility review point for redirects, authentication
   replay, proxy environment precedence, and HTTP/2 selection.
