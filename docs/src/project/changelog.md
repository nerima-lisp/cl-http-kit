# Changelog

This page records the current source version and the next integration
candidate. It is not a version bump: the ASDF systems in this checkout remain
at 0.3.0.

## 0.4.0 candidate (unreleased)

The candidate scope is a breaking integration release for the 0.x series:

- add a URL-and-method client path with native TCP/TLS, redirects, cookies,
  content coding, and pooling enabled by default;
- retain `make-http-client` and `http-client-send` as compatibility entry
  points;
- replace the direct cl+ssl and chipz dependencies with cl-tls-kit and
  cl-deflate-kit, with the remaining crypto and QUIC boundaries supplied by
  the four-kit stack;
- support environment-based proxy selection (`HTTP_PROXY`, `HTTPS_PROXY`,
  and `NO_PROXY`, including lowercase names), proxy/origin authentication,
  and thread-safe pool and cookie state;
- keep HTTP/2 protocol selection in the native client path; defer native
  HTTP/3 connection selection until cl-quic-kit provides the QUIC transport.

These entries describe the integration target. They are not claims that the
0.3.0 systems already provide those defaults.

The current worktree has partial cl-deflate-kit and cl-tls-kit dependency
declarations. The cl+ssl calls in the TLS wrapper and the missing native
URL-and-method route keep the candidate unreleased.

## 0.3.0 (current source version)

The current source provides the callback and stream APIs documented in the
reference pages, including HTTP/1.1, injected HTTP/2, injected HTTP/3 framing,
client policy helpers, native TCP/DNS, and the cl+ssl TLS wrapper.
