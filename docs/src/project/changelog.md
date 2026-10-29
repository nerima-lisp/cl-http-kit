# Changelog

Unreleased entries describe pending changes. Versioned sections record
historical release behavior, not the current API contract; see the
[API reference](../reference/api.md) for that contract.

## Unreleased

## 0.5.0 - 2026-10-29

### Breaking changes

- Remove the `http-kit/websocket` package and SSE exports from
  `http-kit/client`. Move WebSocket consumers to `websocket-kit` from
  `cl-websocket-kit` and SSE consumers to `sse-kit` from `cl-sse-kit`.
  See the
  [WebSocket and SSE migration guide](migration.md#websocket-and-sse-api-migration).
- Reject `:verify nil` because the pinned cl-tls-kit client driver does not
  expose a supported way to disable certificate verification.
- Replace the local HTTP/3 QPACK API with cl-http-message-kit 2.0.1. See the
  [QPACK migration guide](migration.md#0-5-0-changes).

### Added

- Add an HTTPS listener on SBCL with ALPN negotiation for HTTP/2 and HTTP/1.1,
  bounded connection and request resources, lifecycle callbacks, and graceful
  shutdown.
- Add `:session-tickets-p` to `make-http-tls-server-wrapper`, allowing server
  ticket issuance to be disabled.

### Changed

- Cap buffered request-body bytes across active HTTPS listener sessions.
- Destroy per-connection ticket-key stores created by TLS server wrappers when
  their driver closes, while leaving supplied shared stores caller-owned for
  cross-connection resumption.
- Require cl-tls-kit 0.5.2 for TLS, cl-quic-kit 0.2.6 for HTTP/3, and
  cl-weave 1.5.0 for the test and coverage APIs.
- Keep cl-tls-kit and cl-http-message-kit test helpers in separate test-only
  ASDF systems.
- Ignore record `legacy_record_version` in the TLS server wrapper as required by
  RFC 8446 section 5.1 while continuing to validate record content types and
  lengths.

### HTTP/2

- Report malformed PING frames received while awaiting an acknowledgement as
  `http-protocol-error` with `:http2-client` operation and the received frame
  as detail.
- Document `:http2-read` detail for DATA on a never-opened HTTP/2 client stream:
  the stream ID while another request is in progress and has not failed,
  otherwise a list with `0` (the DATA frame type) followed by the stream ID.

### Client

- Expose final response headers before body delivery through
  `http-client-send :on-response-headers`; informational responses continue to
  use `:on-information`.
- Add `make-http-cancellation-token`, `http-client-cancel-request`, and the
  `http-client-request-cancelled` condition. Cancellation interrupts direct
  HTTP/1.1 reads and sends HTTP/2 `RST_STREAM`; synchronous HTTP/2 reads may not
  return until a frame arrives or the request deadline expires. HTTP/3 requires
  a backend `cancel-stream` callback, which the built-in cl-quic-kit adapter
  does not currently provide. With that adapter, cancellation marks the token
  cancelled and signals `http-protocol-error` with operation
  `:http3-transport` and detail `:unsupported`; it sends no stream reset or
  stop-sending signal, so active I/O can continue until completion or deadline.

### Known limitations

- A nonblocking HTTPS handshake write already in progress can complete after
  the handshake deadline expires, before the wrapper reports a timeout.
- The `cl-crypto-kit` 0.2.2 implementation does not explicitly zeroize
  HMAC/HKDF intermediate buffers. The `cl-tls-kit` 0.5.2 key schedule passes
  intermediate HKDF outputs directly into extract without explicit erasure.
  TLS drivers clear some retained secrets on close.

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

