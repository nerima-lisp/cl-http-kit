# Changelog

Unreleased entries describe pending changes. Versioned sections record
historical release behavior, not the current API contract; see the
[API reference](../reference/api.md) for that contract.

## Unreleased

### Breaking changes

- Remove the `http-kit/websocket` package and SSE exports from
  `http-kit/client`. Move WebSocket consumers to `websocket-kit` from
  `cl-websocket-kit` and SSE consumers to `sse-kit` from `cl-sse-kit`.
  See the
  [WebSocket and SSE migration guide](migration.md#websocket-and-sse-api-migration).
- reject `:verify nil` because the pinned cl-tls-kit client driver does not
  expose a supported way to disable certificate verification;

### Added

- add an HTTPS listener on SBCL with ALPN negotiation for HTTP/2 and HTTP/1.1,
  bounded connection and request resources, lifecycle callbacks, and graceful
  shutdown;

### Changed

- cap buffered request-body bytes across active HTTPS listener sessions;
- ignore TLS record `legacy_record_version` as required by RFC 8446 section 5.1
  while continuing to validate record content types and lengths;

### HTTP/2

- report malformed PING frames received while awaiting an acknowledgement as
  `http-protocol-error` with `:http2-client` operation and the received frame
  as detail.
- document `:http2-read` detail for DATA on a never-opened HTTP/2 client stream:
  the stream ID while another request is in progress and has not failed,
  otherwise a list with `0` (the DATA frame type) followed by the stream ID.

## 0.4.1 (historical)

- require certificate-chain, hostname, and trust verification by default on
  the native TLS client path; retain CertificateVerify signature verification
  even when `:verify nil` disables certificate-policy checks;
- reject TLS transport EOF without a peer close_notify;
- report expired HTTP/3 QUIC handshake, read, flow-control write, peer control,
  and peer settings waits as `http-timeout` with deadline metadata;
- support native IPv4 UDP-backed HTTP/3 through an explicitly supplied QUIC
  adapter, keeping the peer address separate from the TLS hostname;
- make QUIC adapter start and close operations idempotent, reject restarting
  a closed adapter, and leave closed adapters inactive during polling;
- require QUIC packet-protection setup before starting the HTTP/3 adapter
  and propagate setup failures before sending network data;
- protect each outstanding QPACK field section independently until its
  acknowledgement or cancellation releases its dynamic-table references;
- reject initial and replacement control-stream attachments after HTTP/3
  client close without taking ownership of the rejected stream;
- release accepted TCP streams when listener callbacks or session validation
  exit before cleanup ownership transfers, including non-local exits; invoke
  session cleanup once and preserve retained or upgraded streams;
- expose `http-connection-error-request-sent-p` and restrict automatic Alt-Svc
  TCP fallback to confirmed pre-send connection failures before a request-body
  producer or response callback has run;
- normalize explicit HTTP and HTTPS default ports in origin keys;
- decode uppercase and lowercase hexadecimal escapes in environment proxy
  credentials consistently;
- reject malformed explicit ports in proxy bypass rules instead of treating
  them as unrestricted host matches;
- reject HTTP/3 proxy routing before opening a connection; accept explicit
  direct-routing plans;
- reject cache revalidation responses whose validators do not identify the
  cached response, and invalidate same-origin `Location` and `Content-Location`
  targets after successful unsafe requests;
- percent-encode non-ASCII UTF-8 bytes even when a custom safe set contains
  matching non-ASCII characters;
- preserve leading spaces in serialized SSE event names, IDs, and data lines;
- reject dotted or cyclic SSE comment lists during construction and serialization;
- reject cyclic listener session options without blocking the accept loop;
- preserve explicit unsupported-feature conditions for native network operations
  on non-SBCL implementations;
- accept quoted HSTS `max-age` values, empty directives, and quoted extension
  values containing semicolons without accepting malformed directives;
- preserve HEAD response metadata for status 205;
- account for chunked payload and framing metadata under separate body and
  header limits;
- preserve queued HTTP/2 request ordering while handling stream flow-control
  updates, and reject frames that interrupt a HEADERS/CONTINUATION sequence;
- keep HTTP/3 PUSH_PROMISE header names and values independent from caller-owned
  strings, both when received and when retained by a server session;
- terminate HTTP/2 server connections explicitly on transport EOF and invoke
  the configured stream-close callback.

## 0.4.0 (historical)

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
- at 0.4.0, the native client path used HTTP/1.1 and HTTP/2 used its explicit
  transport boundary. HTTP/3 was not selected automatically; a supplied
  cl-quic-kit adapter could drive the client QUIC connection and HTTP/3 stream
  lifecycle;
- add HTTP/3 client loopback coverage through the cl-quic-kit adapter for
  Alt-Svc, fallback, and explicit HTTP/3 modes;
- update the dependency pins for cl-crypto-kit, cl-deflate-kit, cl-tls-kit,
  and cl-quic-kit to their release integration branches;
- the 0.4.0 release gate required `nix flake check` on Ubuntu x86_64, including
  the configured test and lint checks; documentation publication required a
  separate strict MkDocs build.

The 0.4.0 runtime verification was limited to Ubuntu x86_64. Its HTTP/3
connection path was verified through the cl-quic-kit adapter; native QUIC
sockets and server acceptance were outside that release boundary.

