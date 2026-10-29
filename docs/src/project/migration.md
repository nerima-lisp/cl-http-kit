# Migration guide

This guide describes the 0.4.0 integration and the planned migration from the
0.4.1 API to 0.5.0. See the [changelog](changelog.md) for pending changes and
historical release behavior.

The 0.4.0 sections describe upgrades from 0.3.x. If you are already on 0.4.1,
start with [0.5.0 changes](#050-changes); use the WebSocket/SSE section when
you use those APIs.

For the WebSocket and SSE API removal, see the
[WebSocket and SSE API migration](#websocket-and-sse-api-migration).

## Client boundary carried from 0.3.x into 0.4.0

In 0.4.0, the high-level client permitted `make-http-client` without a supplied
transport boundary and lazily created a native TCP/TLS connection pool. Build a
request with `http-client-request` and execute it with `http-client-send`.
These compatibility entry points are planned to remain supported in 0.5.0, and
`http-client-send` also accepts a method and URL convenience form.

The 0.4.0 client provided explicit policy helpers for proxy routing,
origin and proxy challenge authentication, gzip/deflate response decoding,
redirects, cookies, proxy environment lookup, and an HTTP/1.1 connection pool.
Pool and cookie state had lock-backed access in that integration.

The 0.4.0 integration provided native TCP and DNS through
`cl-http-kit/network`, and its separate `cl-http-kit/tls` source adapted
cl-tls-kit and cl-crypto-kit for the TLS 1.3 client driver. The client system
used cl-deflate-kit for gzip and deflate. Server-side TLS was unsupported in
0.4.0. The planned 0.5.0 release will add the optional `cl-http-kit/tls`
HTTPS listener; see the
[HTTPS server API](../reference/api.md#https-server).
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

## 0.5.0 changes

The planned 0.5.0 release will remove the local QPACK implementation from
`http-kit/http3`. The mapping below covers all 57 QPACK migration-table entries:
it names their replacement APIs and exports slated for removal in 0.5.0, and
marks the HTTP/3 transport operations that remain. The separate HTTP/3 client
QPACK table below lists six additional entries, outside those 57. The
target is `cl-http-message-kit` v2.0.1 at revision
`1bc39c8659ec282496366d24fab6e5c21096caa2`.
It includes the Section Ack callback, transactional encoder API, and the
`qpack-context-set-max-table-capacity` operation described below. It also
rejects a Section Ack when no field section is outstanding. This change
breaks the 0.4.1 API. cl-http-kit will not provide compatibility adapters.
The replacement QPACK functions are exported by the `http-message-kit`
package; qualify them as `http-message-kit:` in Lisp.

Paths in this section are repository-relative. Unqualified `http3/`, `t/`, and
`docs/` paths refer to cl-http-kit; paths under `src/` in the external-consumer
table refer to the named consumer repository.

Existing HTTP/3 consumer tests in `t/tests-http3.lisp` lines 920–949 and
1013–1055 require the QPACK encoder context to remain unchanged when a capacity
or insert write fails, and when a field-section write fails before its
outstanding-section registration can be committed. This includes table
capacity and contents, insertion state, and outstanding-section state. The
message-kit transaction API stages these changes and commits them only after a
successful transport write. The generic `write-stream` callback does not return
a byte count, and a signaled error alone does not prove that no bytes reached
the peer. Abort only when transport-specific information confirms zero bytes
were sent; that leaves the encoder context unchanged. If delivery is partial or
uncertain, close the connection and discard both encoder and decoder contexts;
aborting locally cannot undo bytes the peer may have received. Do not recreate
QPACK state or rollback in cl-http-kit.

The client request path continues to allow dynamic references when its QPACK
context and peer settings permit them. The migrated server encodes response
field sections with
`qpack-encode-header-block` and `:use-dynamic-p nil`. This preserves decoded
response headers. The 0.4.x API could reference entries already present in a
supplied dynamic table, but its response writers did not write QPACK
encoder-stream instructions. A caller using a prepopulated table had to deliver
the matching insertions separately. Static-table references and literals avoid
that transport dependency, though they may use more bytes.
`http3-server-request-response` checks final headers and trailers,
`http3-server-sends-informational-responses-before-final-response` checks
informational and final headers, and
`http3-reachable-server-response-header-preflight-contract` checks decoded
status and custom response headers.

`qpack-context-set-max-table-capacity` updates both the context maximum and its
backing table maximum. The SETF form of `qpack-context-max-table-capacity`
delegates to the same operation. The setter accepts values from zero through
`2^62-1` and rejects out-of-range values or a maximum below current capacity
before mutation. A changed maximum preserves current capacity and advances the
context revision, so a prepared encoding transaction based on the old revision
cannot commit. Use this operation when peer SETTINGS change the outbound
encoder's maximum table capacity. The cl-http-kit HTTP/3 client initializes
that maximum to zero and updates it after peer SETTINGS arrive; ordinary
requests can encode HEADERS before SETTINGS arrive (`http3/transport.lisp` lines 262–266,
685–701, 1623–1699).

### Dynamic table and entries

| 0.4.x export | 0.5.0 replacement or treatment |
| --- | --- |
| `qpack-dynamic-entry` | Removed. Dynamic entries are internal context state; the kit does not expose entry objects. |
| `qpack-dynamic-entry-p` | Removed with the raw entry type. |
| `qpack-dynamic-entry-absolute-index` | Removed. Absolute indexes are internal table state. |
| `qpack-dynamic-entry-name` | Removed. Read decoded headers through the header-block APIs instead of inspecting table entries. |
| `qpack-dynamic-entry-value` | Removed. Read decoded headers through the header-block APIs instead of inspecting table entries. |
| `qpack-dynamic-entry-size` | Removed. The kit does not expose per-entry size; use `qpack-context-table-size` only when you need the aggregate table size. |
| `qpack-dynamic-table` | Removed. Use `qpack-context` as the state boundary; the kit does not expose its table object. |
| `qpack-dynamic-table-p` | Removed with the raw table type. Use `qpack-context-p` to check the replacement context. |
| `make-qpack-dynamic-table` | Use `make-qpack-context`, mapping `max-capacity` to `:max-table-capacity` and `capacity` to `:table-capacity`. The message-kit defaults are 4096 for both values; the old constructor defaults both to 0, so pass the intended values explicitly. Pass `:huffman nil` to preserve cl-http-kit’s Huffman-off default; message-kit contexts default Huffman on. The context also owns QPACK stream state. |
| `qpack-dynamic-table-max-capacity` | Use `qpack-context-max-table-capacity` with the context. |
| `qpack-dynamic-table-capacity` | Use `qpack-context-table-capacity` with the context. |
| `qpack-dynamic-table-entries` | Removed. The kit does not expose the ordered entry list. |
| `qpack-dynamic-table-size` | Use `qpack-context-table-size` with the context. |
| `qpack-dynamic-table-insert-count` | Use `qpack-context-insert-count` with the context. |
| `qpack-dynamic-table-set-capacity` | Use `qpack-context-set-capacity` with the context and capacity. This changes the context's table state. |
| `qpack-dynamic-table-insert` | There is no raw table-insert API. Use `qpack-prepare-insert-with-literal-name` or `qpack-prepare-insert-with-name-reference` with a context; each returns instruction bytes and a transaction. See the transaction behavior below. |

### HTTP/3 decoding and blocked streams

| 0.4.x export | 0.5.0 replacement or treatment |
| --- | --- |
| `qpack-blocked-field-section` | Removed. The HTTP/3 decoder returns an opaque blocked-stream token when it cannot yet decode a field section. |
| `qpack-blocked-field-section-required-insert-count` | Removed. Use `qpack-take-ready-streams` to retrieve tokens whose required inserts have arrived. |
| `qpack-blocked-field-section-current-insert-count` | Removed. The kit does not expose a blocked token's internal counts. |
| `http3-qpack-decoder-context` | Use the opaque adapter context returned by `qpack-make-http3-decoder-context`. |
| `http3-qpack-decoder-context-p` | Removed. Pass the adapter context to its public operations; `qpack-context-p` checks the underlying QPACK context, not the adapter token. |
| `make-http3-qpack-decoder-context` | Use `qpack-make-http3-decoder-context` with a `qpack-context`; the adapter's `:max-blocked-streams` limit is configured there. |
| `http3-qpack-decoder-context-dynamic-table` | Removed. The adapter does not expose its dynamic table. Use context metrics such as `qpack-context-table-size` and `qpack-context-insert-count`. |
| `http3-qpack-decoder-context-blocked-stream-limit` | Removed as an adapter-state accessor. Supply the limit with `:max-blocked-streams` to `qpack-make-http3-decoder-context`; the adapter does not expose it afterward. |
| `http3-qpack-blocked-stream` | Removed as a constructible type. The decoder creates opaque blocked-stream tokens. |
| `http3-qpack-blocked-stream-p` | Use `qpack-blocked-stream-p` on a returned token. |
| `http3-qpack-blocked-stream-stream-id` | Removed. Pass the stream ID to the decode or cancellation operation; the token does not expose it. |
| `http3-qpack-blocked-stream-required-insert-count` | Removed. Readiness is reported through `qpack-take-ready-streams`; the token does not expose its required count. |
| `decode-http3-qpack-field-section` | Use `qpack-decode-http3-field-section`; see the decode behavior below. |
| `resume-http3-qpack-blocked-stream` | Use `qpack-resume-blocked-stream` with the opaque token. |

### Encoder state and codec operations

| 0.4.x export | 0.5.0 replacement or treatment |
| --- | --- |
| `qpack-decoder-stream-state` | Removed as a separately managed structure. The peer's decoder stream reports acknowledgments for field sections and inserts sent by this endpoint, so decoding it updates this endpoint's encoder state. The message-kit `qpack-context` owns that state; pass it to `qpack-decode-decoder-stream`. |
| `qpack-decoder-stream-state-p` | Removed with the raw state type; use `qpack-context-p` for a context. |
| `make-qpack-decoder-stream-state` | Removed. `make-qpack-context` creates the integrated table and stream state. |
| `qpack-decoder-stream-state-dynamic-table` | Removed. The context does not expose its table object. |
| `qpack-decoder-stream-state-sent-insert-count` | Use `qpack-context-insert-count` for the context's insertion count. |
| `qpack-decoder-stream-state-known-received-count` | Use `qpack-context-known-received-count` with the context. |
| `qpack-decoder-stream-state-blocked-stream-limit` | Use `qpack-context-max-blocked-streams` with the context. |
| `qpack-decoder-stream-state-outstanding-sections` | Removed. The context keeps outstanding-section state private and updates it when the prepared field-section transaction is committed. |
| `qpack-decoder-stream-state-note-insertions-sent` | Removed as a separate state mutation. Commit the prepared insert-instruction transaction after its write succeeds to update insertion state. |
| `qpack-decoder-stream-state-register-section` | Removed as a separate state mutation. Prepare the field section with `:context`, `:use-dynamic-p t`, and `:stream-id`, then commit its transaction after the write succeeds. |
| `qpack-encode-set-dynamic-table-capacity` | Use `qpack-prepare-set-capacity` with context and capacity. It returns encoder-stream instruction bytes and a transaction. |
| `qpack-encode-insert-with-name-reference` | Use `qpack-prepare-insert-with-name-reference` with context, name index, and value. It returns instruction bytes and a transaction; `:static-p` and `:huffman` control the reference and string encoding. |
| `qpack-encode-insert-with-literal-name` | Use `qpack-prepare-insert-with-literal-name` with context, name, and value. It returns instruction bytes and a transaction. |
| `qpack-encode-duplicate` | Use `qpack-prepare-duplicate` with context and relative index. It returns instruction bytes and a transaction. |
| `qpack-encode-section-acknowledgment` | Use `qpack-encode-section-ack` with the stream ID. |
| `qpack-encode-stream-cancellation` | Use the same-named message-kit function with the stream ID; it returns instruction bytes and does not update context state. For HTTP/3 adapter cleanup, use `qpack-cancel-blocked-stream`, which also removes blocked state and returns the release result and instruction bytes. |
| `qpack-encode-insert-count-increment` | Use the same-named message-kit function with the increment; it returns instruction bytes and does not update context state. |
| `qpack-encode-field-section` | Use `qpack-prepare-header-block`. It takes headers first and accepts `:context`, `:use-dynamic-p`, `:huffman`, and `:stream-id`; `:stream-id` is required when `:use-dynamic-p` is true. It returns field-section bytes and a transaction for any outstanding-section registration. |
| `qpack-decode-field-section` | Use `qpack-decode-header-block` for raw field-section decoding or `qpack-decode-http3-field-section` for HTTP/3. See the decode API, limits, and Section Ack details below. |
| `qpack-process-encoder-stream` | Use `qpack-decode-encoder-stream` with context first and octets second. It updates the context and returns decoded events and the number of bytes consumed. |
| `qpack-process-decoder-stream` | Use `qpack-decode-decoder-stream` with context first and octets second. It applies acknowledgments to the context and returns decoded events and the number of bytes consumed; section-acknowledgment events are reported as `(:section-ack stream-id)`. |

### HTTP/3 client QPACK exports

| 0.4.x export | 0.5.0 replacement or treatment |
| --- | --- |
| `http3-client-qpack-encoder-table` | Removed from exports. This exposes per-connection encoder state used only by the HTTP/3 transport; there is no public table replacement. |
| `http3-client-qpack-decoder-table` | Removed from exports. This exposes per-connection decoder state used only by the HTTP/3 transport; use HTTP/3 behavior rather than inspecting its table. |
| `http3-client-qpack-decoder-context` | Removed from exports. This exposes the transport's adapter context; there is no public accessor replacement. |
| `http3-client-qpack-decoder-state` | Removed from exports. This exposes per-connection encoder acknowledgment state; the HTTP/3 transport will keep it in the message-kit context. |
| `http3-client-set-qpack-capacity` | Retained as an HTTP/3 transport operation. Prepare capacity bytes with `qpack-prepare-set-capacity` and write them to the client's encoder stream. Commit after a successful write; abort only if the transport confirms zero bytes were sent. If delivery is partial or uncertain, close the connection and discard both QPACK contexts. |
| `http3-client-insert-qpack-field` | Retained as an HTTP/3 transport operation. Prepare the literal-name insertion with `qpack-prepare-insert-with-literal-name` and write it to the client's encoder stream. Commit after a successful write; abort only if the transport confirms zero bytes were sent. If delivery is partial or uncertain, close the connection and discard both QPACK contexts. After a successful write and commit, it returns the committed `qpack-context-insert-count`; a failed write returns no count. |

### Encoder and decoder APIs, limits, and acknowledgments

#### Encoder API and return values

Use these message-kit preparations for context-changing encoder writes:

- `(qpack-prepare-set-capacity context capacity)`
- `(qpack-prepare-insert-with-name-reference context name-index value &key (static-p t) (huffman nil huffman-supplied-p))`
- `(qpack-prepare-insert-with-literal-name context name value &key (huffman nil huffman-supplied-p))`
- `(qpack-prepare-duplicate context relative-index)`
- `(qpack-prepare-header-block headers &key context (use-dynamic-p nil) (huffman nil huffman-supplied-p) stream-id)`

Supply `:stream-id` whenever `:use-dynamic-p` is true so the context can track
outstanding references and protect their entries from eviction.

Each preparation returns `(values wire-octets transaction)` without changing
the context. Write the octets, then call
`(qpack-commit-encoding transaction)` after a successful transport write or
`(qpack-abort-encoding transaction)` after a transport-specific confirmation
that zero bytes were sent. The generic `write-stream` callback does not return
a byte count; a signaled error alone is not proof of a zero-byte write. Commit
returns the transaction; abort returns `t` and leaves the context unchanged.
The transaction type and predicate are
`qpack-encoding-transaction` and `qpack-encoding-transaction-p`. A stale
transaction, a repeated commit, a repeated abort, or a commit after abort signals
`qpack-encoding-transaction-error`; read the reason with
`(qpack-encoding-transaction-error-reason condition)`. It returns `:invalid`
when commit or abort receives a value that is not a
`qpack-encoding-transaction` object, `:stale`
for a transaction based on an older context revision, `:committed` for a
repeated commit, or `:aborted` for a repeated abort or commit after abort. The
header-block transaction also records outstanding-section registration. Hold
one connection serialization lock across preparation, transport write, and
commit or abort; message-kit does not provide this lock. Existing
immediate-commit `qpack-encode-*` wrappers cannot be aborted after a failed
transport write. Stateless instruction encoders such as
`qpack-encode-section-ack`, `qpack-encode-stream-cancellation`, and
`qpack-encode-insert-count-increment` return instruction octets directly and
do not change context state.

The encoder and decoder stream decoders take context followed by octets and return
`(values events bytes-consumed)`:

- `(qpack-decode-encoder-stream context octets &key allow-incomplete-p (max-instruction-bytes 65536) (max-buffer-bytes 262144))`
- `(qpack-decode-decoder-stream context octets &key allow-incomplete-p (max-instruction-bytes 65536) (max-buffer-bytes 262144))`

`bytes-consumed` counts complete instruction bytes from the beginning of the
supplied octets. With `:allow-incomplete-p t`, message-kit leaves a trailing
partial instruction unconsumed and does not buffer it. The HTTP/3 transport
reader owns the receive buffer. In cl-http-kit, that layer is
`read-http3-qpack-encoder-stream` or `read-http3-qpack-decoder-stream`; each
appends the next chunk to its saved buffer and passes the full buffer to the
decoder. Retain `(subseq buffer bytes-consumed)` after each call and submit it
again with the next chunk. If FIN arrives while the buffer still contains an
incomplete instruction, the corresponding reader reports `http-protocol-error`
with detail `:qpack-encoder-stream-error` or `:qpack-decoder-stream-error`.

The raw field-section decoder takes octets and returns
`(values headers bytes-consumed required-insert-count base)`:

- `(qpack-decode-header-block octets &key context (start 0) end stream-id max-header-bytes max-fields)`

#### HTTP/3 decoder and blocked streams

`qpack-decode-http3-field-section` takes field-section octets, adapter context,
stream ID, and a required `:role` of `:request`, `:response`, or `:trailers`.
Its call shape is `(qpack-decode-http3-field-section octets adapter-context stream-id
&key role serialize on-section-acknowledgment (max-header-bytes 65536)
(max-fields 256))`.
It returns one value: a list of decoded `http-header` objects or an opaque
blocked-stream token when required inserts have not arrived.
The blocked-stream call shapes use `qpack-context` for the underlying codec
state and `adapter-context` for the HTTP/3 decoder adapter:

- `(qpack-make-http3-decoder-context qpack-context &key max-blocked-streams)` creates the adapter context.
- `(qpack-take-ready-streams adapter-context)` returns a list of ready tokens.
- `(qpack-resume-blocked-stream token)` resumes one token and returns the decoded and validated header list.
- `(qpack-cancel-blocked-stream adapter-context stream-id &key serialize)` returns `(values released-p instruction-octets)`.

The `:serialize` callback must call its thunk exactly once, synchronously, while
holding the connection's QPACK lock. In cl-http-kit, the client supplies this
callback as `http3-client-serialize` and invokes it through
`%call-http3-serialized`; the server supplies `qpack-serialize` to
`serve-http3-request-stream`. These callbacks define the transport's lock
boundary; the QPACK code does not create a lock. Use the same lock while
decoding the peer's encoder stream and taking ready tokens.
`qpack-resume-blocked-stream` uses the callback saved in its token.
`qpack-take-ready-streams` removes ready tokens from the blocked set but the
adapter retains each token for its stream until it is resumed or cancelled.
Cancellation returns `released-p` as true when it removes a blocked or retained
token for that stream, and false when neither exists. It returns cancellation
instruction octets either way; send those octets on the decoder stream even
when `released-p` is false.

Tokens do not expose stream IDs, so retain each original stream ID alongside
its token if the caller needs to associate it with a transport stream. Pass
that retained ID to cancellation.

#### Limits

For `qpack-decode-http3-field-section`, omitted `:max-header-bytes` and
`:max-fields` default to 65536 and 256; passing `nil` explicitly for either
keyword is rejected. Pass existing custom header-byte and field-count limits as
`:max-header-bytes` and `:max-fields`; omit them to use the defaults. Set
`:max-blocked-streams` when creating the adapter to preserve an existing blocked
stream limit.
The raw `qpack-decode-header-block` accepts the same keywords but uses its
context/default limits when either keyword is omitted or explicitly `nil`.
`:max-header-bytes` limits the decoded
header-list size, including 32 bytes of overhead per field plus the name and
value octet lengths; `:max-fields` limits the number of decoded fields. The
HTTP/3 decoder has no separate compressed-input keyword. It caps compressed
input at `min(262144, max-header-bytes + 16 * max-fields + 16)` using the
effective header and field limits; omitted limits use their defaults, while an
explicit `nil` for either is rejected. `send-http3-request` passes per-call
limits to response decoding at `http3/transport.lisp` lines 1900–1914, and
`%h3-decode-response-field-section` applies the minimum of per-call and client
limits at lines 1448–1469. `serve-http3-request-stream` passes its configured
limits at lines 2888–2910.

#### Section Ack behavior

Pass `:on-section-acknowledgment` to `qpack-decode-http3-field-section` when
the transport must send a Section Ack. The callback receives the stream ID
only after decoding and role validation succeed, and only for a field section
with a nonzero required insert count. If decoding first returns a blocked
token, no callback runs then; after the token is resumed, the callback runs
once on successful decode and validation. `qpack-decode-decoder-stream`
handles acknowledgments received from the peer and reports them in its
returned events as `(:section-ack stream-id)`; it does not take that callback.

`send-http3-request` writes Section Ack instructions on the client decoder
stream after dynamic response sections pass decoding and role validation. It
uses the client-owned QPACK contexts and adapter by default. The
`:qpack-decoder-context` and `:qpack-http3-decoder-context` keywords may be
omitted or set to either or both corresponding client-owned objects. Each
supplied value must be the client-owned object because
incoming encoder instructions and blocked tokens update that shared state;
a different object signals an error. `receive-http3-push` applies the same
checks before reading the stream or consuming the promised push ID.
When the client's `SETTINGS_QPACK_BLOCKED_STREAMS` value is positive,
`:await-qpack` must be supplied before the client starts reading a response
stream, even if the field section does not block. This is a preflight check; it
does not invoke `:await-qpack`. If decoding returns a blocked token, the client
calls `:await-qpack` with that token. The callback must wait for the required
inserts, resume that same token with `qpack-resume-blocked-stream`, and return
the decoded headers.

For server Section Ack handling, `serve-http3-request-stream` accepts
`:on-qpack-section-acknowledgment`, which has the same stream-ID callback and
callback contract as `:on-section-acknowledgment` described above. Build the
adapter with `qpack-make-http3-decoder-context` from the same context passed as
`:qpack-decoder-context`, then pass the context as `:qpack-decoder-context`
and the returned adapter as `:qpack-http3-decoder-context`, with `:stream-id`;
supplying only one part of the decoder pair signals an error. The callback
receives the stream ID; call `qpack-encode-section-ack` with it and write the
resulting octets to the decoder stream.
When the adapter is supplied, the server requires both `:await-qpack` and
`:on-qpack-section-acknowledgment`. Static field sections invoke neither
callback. `:await-qpack` must be supplied before the server starts reading the
request stream, even if the field section does not block. This is a preflight
check; it does not invoke `:await-qpack`. If decoding returns a blocked token,
the server calls `:await-qpack` with that token. The `:await-qpack` callback
must wait for the required inserts, resume that same token with
`qpack-resume-blocked-stream`, and return the decoded headers. After resume and
validation succeed, `:on-qpack-section-acknowledgment` runs once with the
stream ID.

If an external caller needs public state or behavior that the message-kit API
does not expose, add that API to message-kit before migrating the caller. Do
not reimplement QPACK state or codec operations in cl-http-kit.

### External consumers

Read-only source-archive scans searched all 57 QPACK-related exports in the
preceding mapping tables as fixed strings with `rg --hidden --no-ignore -n -F
-f`, including hidden and ignored text source files. In `nerima-lisp/cl-websocket-kit`
`main` revision `cf3dfb4ac796d396e4d7fededaa3effefbc642d4`, the scan found 18
references to 10 exports; the other 47 names had no matches. The six additional
client exports had no matches. The cl-http-kit v0.4.0 pin at that revision
remains compatible; the referenced old exports remain available until the
0.5.0 update.

| Export | cl-websocket-kit references |
| --- | --- |
| `qpack-dynamic-table-p` | `src/http2-3-connection.lisp:2059, 2062` |
| `make-qpack-dynamic-table` | `src/http2-3-connection.lisp:2050, 2056` |
| `qpack-dynamic-table-max-capacity` | `src/http2-3-connection.lisp:1729, 2068`; `src/http2-3.lisp:773` |
| `qpack-dynamic-table-capacity` | `src/http2-3-connection.lisp:1730` |
| `qpack-dynamic-table-set-capacity` | `src/http2-3-connection.lisp:1735, 2073, 2074` |
| `qpack-encode-set-dynamic-table-capacity` | `src/http2-3-connection.lisp:1733` |
| `qpack-encode-field-section` | `src/http2-3.lisp:749` |
| `qpack-decode-field-section` | `src/http2-3.lisp:768` |
| `qpack-process-encoder-stream` | `src/http2-3-connection.lisp:1589, 1684` |
| `qpack-process-decoder-stream` | `src/http2-3-connection.lisp:1625, 1702` |

In `nerima-lisp/cl-sse-kit` `main` revision
`161951de39e73544b9e7a05fb6385d7d68364fae`, the same fixed-string scan found
no references to any of the 57 exports.

### Downstream cl-websocket-kit migration

The downstream cl-websocket-kit migration is separate from this cl-http-kit
migration. Prepare it against the cl-http-kit 0.5.0 pin:

1. Replace the 10 old QPACK exports at the call sites in the table above.
   Use `make-qpack-context` and context operations for encoder state, and the
   HTTP/3 decoder context and blocked-stream operations for decoding.
2. Adapt callers to the context arguments, `http-header` results, additional
   decode values, and opaque blocked-stream tokens described above. Do not
   inspect or construct the removed table and state objects.
3. Run the cl-websocket-kit test suite and require its GitHub PR checks to pass
   against the 0.5.0 pin before landing its dependency update.

### WebSocket and SSE API migration

The 0.4.1 release includes the `http-kit/websocket` package and SSE exports
from `http-kit/client`. The planned 0.5.0 release will remove them. When
upgrading,
add the relevant kit to your application's dependencies: use `cl-websocket-kit`
for `websocket-kit` and `cl-sse-kit` for `sse-kit`. Neither kit is a dependency
of `cl-http-kit`. The replacement names below were verified against the
[cl-websocket-kit API at revision
`cf3dfb4ac796d396e4d7fededaa3effefbc642d4`](https://github.com/nerima-lisp/cl-websocket-kit/tree/cf3dfb4ac796d396e4d7fededaa3effefbc642d4)
and the
[cl-sse-kit API at revision
`161951de39e73544b9e7a05fb6385d7d68364fae`](https://github.com/nerima-lisp/cl-sse-kit/tree/161951de39e73544b9e7a05fb6385d7d68364fae).

| Removed symbol | Replacement |
| --- | --- |
| `http-kit/websocket:websocket-frame` | `websocket-kit:websocket-frame` |
| `http-kit/websocket:websocket-frame-p` | `websocket-kit:websocket-frame-p` |
| `http-kit/websocket:websocket-frame-fin-p` | `websocket-kit:websocket-frame-fin-p` |
| `http-kit/websocket:websocket-frame-rsv1-p` | `websocket-kit:websocket-frame-reserved-bits` |
| `http-kit/websocket:websocket-frame-opcode` | `websocket-kit:websocket-frame-opcode` |
| `http-kit/websocket:websocket-frame-mask-p` | `websocket-kit:websocket-frame-mask-p` |
| `http-kit/websocket:websocket-frame-masking-key` | `websocket-kit:websocket-frame-masking-key` |
| `http-kit/websocket:websocket-frame-payload` | `websocket-kit:websocket-frame-payload` |
| `http-kit/websocket:make-websocket-frame` | `websocket-kit:make-websocket-frame` |
| `http-kit/websocket:serialize-websocket-frame` | `websocket-kit:serialize-websocket-frame` |
| `http-kit/websocket:parse-websocket-frame` | `websocket-kit:parse-websocket-frame` |
| `http-kit/websocket:read-websocket-frame` | `websocket-kit:read-websocket-frame` |
| `http-kit/websocket:write-websocket-frame` | `websocket-kit:write-websocket-frame` |
| `http-kit/websocket:read-websocket-message` | `websocket-kit:read-websocket-message` |
| `http-kit/websocket:write-websocket-message` | `websocket-kit:write-websocket-message` |
| `http-kit/websocket:websocket-ping` | `websocket-kit:websocket-ping` |
| `http-kit/websocket:websocket-pong` | `websocket-kit:websocket-pong` |
| `http-kit/websocket:websocket-close` | `websocket-kit:websocket-close` |
| `http-kit/websocket:serve-websocket-session` | `websocket-kit:serve-websocket-session` |
| `http-kit/websocket:make-websocket-client-key` | `websocket-kit:generate-websocket-key` |
| `http-kit/websocket:websocket-accept-key` | `websocket-kit:websocket-accept-key` |
| `http-kit/websocket:websocket-extension` | No direct replacement |
| `http-kit/websocket:websocket-extension-p` | No direct replacement |
| `http-kit/websocket:websocket-extension-name` | No direct replacement |
| `http-kit/websocket:websocket-extension-parameters` | No direct replacement |
| `http-kit/websocket:parse-websocket-extensions` | No direct replacement |
| `http-kit/websocket:websocket-upgrade-request-p` | `websocket-kit:websocket-upgrade-request-p` |
| `http-kit/websocket:websocket-upgrade-response` | `websocket-kit:websocket-upgrade-response` |
| `http-kit/websocket:make-websocket-upgrade-request` | `websocket-kit:make-websocket-upgrade-request` |
| `http-kit/websocket:websocket-client-handshake` | `websocket-kit:websocket-client-handshake` |
| `http-kit/websocket:websocket-valid-close-code-p` | `websocket-kit:websocket-valid-close-code-p` |
| `http-kit/websocket:make-websocket-close-payload` | `websocket-kit:make-websocket-close-payload` |
| `http-kit/websocket:parse-websocket-close-payload` | `websocket-kit:parse-websocket-close-payload` |
| `http-kit/client:http-sse-event` | `sse-kit:http-sse-event` |
| `http-kit/client:http-sse-event-p` | `sse-kit:http-sse-event-p` |
| `http-kit/client:make-http-sse-event` | `sse-kit:make-http-sse-event` |
| `http-kit/client:http-sse-event-event` | `sse-kit:http-sse-event-event` |
| `http-kit/client:http-sse-event-data` | `sse-kit:http-sse-event-data` |
| `http-kit/client:http-sse-event-id` | `sse-kit:http-sse-event-id` |
| `http-kit/client:http-sse-event-retry` | `sse-kit:http-sse-event-retry` |
| `http-kit/client:http-sse-event-comments` | `sse-kit:http-sse-event-comments` |
| `http-kit/client:parse-http-sse-events` | `sse-kit:parse-http-sse-events` |
| `http-kit/client:read-http-sse-events` | `sse-kit:read-http-sse-events` |
| `http-kit/client:serialize-http-sse-event` | `sse-kit:serialize-http-sse-event` |

For RSV1, test bit `#x40` in `websocket-frame-reserved-bits`. Construct frames
with `:reserved-bits` instead of `:rsv1-p`, setting bit `#x40` for RSV1.
The generic WebSocket extension structure, accessors, and parser have no direct
replacement. Consumers of other extensions must supply their own
extension-specific negotiation and framing or stop negotiating those
extensions. Upstream exports only `websocket-permessage-deflate-extension`,
which is specific to permessage-deflate.

## Migration checklist

### Checklist for the 0.4.0 integration

Use this checklist only when upgrading from 0.3.x to 0.4.0.

1. Keep a custom `transport-function` compatible with
   `(request &key timeout deadline max-header-bytes max-fields max-body-bytes
   proxy proxy-plan request-body-function request-body-length on-body-chunk
   on-information (collect-body-p t))`.
   The first seven keywords are always passed; the body producer and length
   are passed together when either is supplied; callbacks are passed only when
   supplied; and `collect-body-p` is passed only when false. A custom
   `http3-transport-function` also accepts the always-passed
   `alternative-service` keyword.
2. Use `cl-http-kit/network` and `cl-http-kit/tls` with cl-tls-kit and
   cl-crypto-kit for native networking and TLS, and cl-deflate-kit for
   compression. Remove the legacy external TLS and compression integrations
   from the default path.
3. If an application shared a pool or cookie jar between threads, remove its
   external serialization only after the 0.4.0 thread-safe implementation is
   present and tested.
4. Treat 0.4.0 as a compatibility review point for redirects, authentication
   replay, proxy environment precedence, and HTTP/2 selection.

### Checklist for the 0.5.0 QPACK API change

When upgrading to 0.5.0, replace each old QPACK export using the mapping above.
Configure the outbound encoder context from the peer's
`SETTINGS_QPACK_MAX_TABLE_CAPACITY` and `SETTINGS_QPACK_BLOCKED_STREAMS`; those
values limit the dynamic table and blocked streams used in field sections sent
to that peer. Configure the inbound decoder context and its HTTP/3 adapter from
the locally advertised settings; those values limit dynamic references and
blocked sections accepted from the peer. Start table capacity at zero and pass
`:huffman nil` where needed to preserve cl-http-kit 0.4.x's Huffman-off default.

For every state-changing encoder write, prepare the instruction or field section,
write its octets while holding the connection QPACK lock. On success, commit.
If the transport confirms that zero bytes were sent, abort. If delivery is
partial or uncertain, close the connection and discard both contexts. For
inbound HTTP/3, use one decoder context with its adapter, preserve the
unconsumed suffix of fragmented stream instructions, retain each blocked
token's stream ID, and send Section Ack only after successful decode and role
validation. Preserve the configured HTTP/3 field-count and byte limits, the
peer's `SETTINGS_QPACK_BLOCKED_STREAMS` limit for outbound sections, the local
`SETTINGS_QPACK_BLOCKED_STREAMS` limit and decoder adapter's
`:max-blocked-streams` limit for inbound sections, and the 62-bit QUIC
variable-integer bound.
