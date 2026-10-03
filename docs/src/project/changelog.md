# Changelog

This page records the current source version and the next integration
candidate. The ASDF systems in this checkout are version 0.4.0.

## 0.4.0

The candidate scope is a breaking integration release for the 0.x series:

- add a URL-and-method client path with native TCP/TLS, redirects, cookies,
  content coding, and pooling enabled by default;
- retain `make-http-client` and `http-client-send` as compatibility entry
  points;
- replace the direct legacy TLS and compression dependencies with cl-tls-kit
  and cl-deflate-kit, with the remaining crypto and QUIC boundaries supplied
  by the four-kit stack;
- support environment-based proxy selection (`HTTP_PROXY`, `HTTPS_PROXY`,
  and `NO_PROXY`, including lowercase names), proxy/origin authentication,
  and thread-safe pool and cookie state;
- keep HTTP/2 protocol selection in the native client path; defer native
  HTTP/3 connection selection until cl-quic-kit provides the QUIC transport.

These entries describe the integrated 0.4.0 source.

The current worktree has cl-deflate-kit and cl-tls-kit dependency declarations.
The missing QUIC connection and server-side TLS driver remain outside the
candidate boundary.

## Earlier 0.3.x source

The current source provides the callback and stream APIs documented in the
reference pages, including HTTP/1.1, injected HTTP/2, injected HTTP/3 framing,
client policy helpers, native TCP/DNS, and the cl-tls-kit TLS 1.3 client
wrapper.
