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
- keep HTTP/2 protocol selection in the native client path; HTTP/3 is not
  selected automatically, while an explicitly supplied cl-quic-kit adapter can
  drive the client QUIC connection and HTTP/3 stream lifecycle;

These entries describe the integrated 0.4.0 source.

The current worktree has cl-deflate-kit and cl-tls-kit dependency declarations.
The HTTP/3 client adapter uses cl-quic-kit; QUIC implementation details,
certificate and TLS policy, and the server-side TLS driver remain outside the
candidate boundary.

## Earlier 0.3.x source

The current source provides the callback and stream APIs documented in the
reference pages, including HTTP/1.1, injected HTTP/2, injected HTTP/3 framing,
client policy helpers, native TCP/DNS, and the cl-tls-kit TLS 1.3 client
wrapper.
