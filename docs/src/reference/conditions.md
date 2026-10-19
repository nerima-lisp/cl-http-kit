# Conditions

The public conditions describe protocol, input, transport, timeout, and limit
failures without requiring callers to parse error strings.

## Condition hierarchy

Conditions defined by this library inherit from HTTP-ERROR. The common readers are
HTTP-ERROR-MESSAGE and HTTP-ERROR-OPERATION. More specific conditions add the
field that identifies the rejected input or protocol detail.

Caller-supplied callbacks and lower-level libraries can propagate other
conditions. In particular, TLS stream writes and graceful close can propagate
conditions from `cl-tls-kit`; handling HTTP-ERROR alone does not catch them.

The core condition symbols and readers belong to `HTTP-KIT`. The table lists
direct parents; readers from parent conditions are inherited.

| Condition | Direct parent | Additional readers | Typical cause |
| --- | --- | --- | --- |
| HTTP-ERROR | ERROR | HTTP-ERROR-MESSAGE, HTTP-ERROR-OPERATION | Common base for library-defined conditions |
| HTTP-PROTOCOL-ERROR | HTTP-ERROR | HTTP-PROTOCOL-ERROR-DETAIL | Invalid or contradictory wire framing |
| HTTP-INVALID-URI | HTTP-PROTOCOL-ERROR | HTTP-INVALID-URI-INPUT | URI syntax or component validation failed |
| HTTP-INVALID-HEADER | HTTP-PROTOCOL-ERROR | HTTP-INVALID-HEADER-NAME, HTTP-INVALID-HEADER-REASON | Invalid name, value, or line content |
| HTTP-INVALID-STATUS | HTTP-PROTOCOL-ERROR | HTTP-INVALID-STATUS-LINE, HTTP-INVALID-STATUS-CODE | Malformed or unsupported status line |
| HTTP-CONNECTION-ERROR | HTTP-ERROR | HTTP-CONNECTION-ERROR-CAUSE, HTTP-CONNECTION-ERROR-REQUEST-SENT-P | A native or custom transport failed |
| HTTP-TIMEOUT | HTTP-ERROR | HTTP-TIMEOUT-KIND | A deadline or timeout expired |
| HTTP-SIZE-LIMIT-EXCEEDED | HTTP-PROTOCOL-ERROR | HTTP-SIZE-LIMIT-EXCEEDED-LIMIT, HTTP-SIZE-LIMIT-EXCEEDED-OBSERVED, HTTP-SIZE-LIMIT-EXCEEDED-KIND | A header or body byte limit was exceeded |
| HTTP-UNSUPPORTED-FEATURE | HTTP-PROTOCOL-ERROR | HTTP-UNSUPPORTED-FEATURE-NAME | The requested protocol feature is outside the implementation |

## Handle failures

Use HANDLER-CASE or HANDLER-BIND with the package-qualified condition names:

```lisp
(handler-case
    (http-kit:parse-http-uri "https://example.test/a b")
  (http-kit:http-invalid-uri (condition)
    (http-kit:http-invalid-uri-input condition)))
```

Conditions carry structured data, so logging policy can decide which fields are
safe to expose. The library does not turn transport causes or request values
into a universal logging format.

## Client policy failures

The optional `cl-http-kit/client` system adds policy-specific conditions and
readers in `HTTP-KIT/CLIENT`. `HTTP-CLIENT-ERROR` directly inherits from
`HTTP-KIT:HTTP-ERROR`; every other condition below directly inherits from
`HTTP-CLIENT-ERROR`.

| Condition | Additional readers | Typical cause |
| --- | --- | --- |
| HTTP-CLIENT-ERROR | HTTP-CLIENT-ERROR-DETAIL | Common base for client-policy failures |
| HTTP-REDIRECT-LIMIT-EXCEEDED | HTTP-REDIRECT-LIMIT-EXCEEDED-URI, HTTP-REDIRECT-LIMIT-EXCEEDED-REDIRECTS | The redirect policy exhausted its limit |
| HTTP-RETRY-EXHAUSTED | HTTP-RETRY-EXHAUSTED-ATTEMPTS, HTTP-RETRY-EXHAUSTED-LAST-CONDITION, HTTP-RETRY-EXHAUSTED-LAST-RESPONSE | The retry policy exhausted its attempts |
| HTTP-COOKIE-ERROR | None | A cookie could not be accepted or rendered |
| HTTP-CACHE-ERROR | None | A cache operation failed |
| HTTP-PROXY-ERROR | None | Proxy selection or planning failed |

## Resource failures

HTTP-TIMEOUT identifies the timeout kind, including the default deadline kind.

HTTP-CONNECTION-ERROR-REQUEST-SENT-P defaults to true, conservatively treating
the request as possibly sent. A transport may supply `:request-sent-p nil`
only when it guarantees that the failure occurred before request transmission.
Alt-Svc fallback additionally requires that no request-body producer or
response callback has run; the flag alone does not authorize a replay.
