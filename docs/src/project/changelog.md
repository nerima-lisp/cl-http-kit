# Changelog

This page records the current source version. The ASDF systems in this
checkout are version 0.4.0.

## 0.4.0

The 0.4.0 release is a breaking integration release for the 0.x series:

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
- keep the native client path on HTTP/1.1; HTTP/2 uses its explicit transport
  boundary, while HTTP/3 is not selected automatically and an explicitly
  supplied cl-quic-kit adapter can drive the client QUIC connection and HTTP/3
  stream lifecycle;
- add HTTP/3 client loopback coverage through the cl-quic-kit adapter for
  Alt-Svc, fallback, and explicit HTTP/3 modes;
- update the dependency pins for cl-crypto-kit, cl-deflate-kit, cl-tls-kit,
  and cl-quic-kit to their release integration branches;
- verify the release gate on Ubuntu x86_64 with `nix flake check`, including
  the test and lint checks defined by the flake; run the separate strict MkDocs
  build for documentation when publishing the docs.

The documented runtime verification is limited to Ubuntu x86_64. The HTTP/3
connection path is verified through the cl-quic-kit adapter; native QUIC
sockets and server acceptance remain outside this release boundary.

The HTTP/3 client adapter uses cl-quic-kit; QUIC implementation details,
certificate and TLS policy, and the server-side TLS driver remain outside the
library boundary.

## Earlier 0.3.x source

The current source provides the callback and stream APIs documented in the
reference pages, including HTTP/1.1, injected HTTP/2, injected HTTP/3 framing,
client policy helpers, native TCP/DNS, and the cl-tls-kit TLS 1.3 client
wrapper.
