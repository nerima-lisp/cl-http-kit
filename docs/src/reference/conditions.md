# Conditions

The public conditions describe protocol, input, transport, timeout, and limit
failures without requiring callers to parse error strings.

## Condition hierarchy

All public failures inherit from HTTP-ERROR. The common readers are
HTTP-ERROR-MESSAGE and HTTP-ERROR-OPERATION. More specific conditions add the
field that identifies the rejected input or protocol detail.

| Condition | Additional readers | Typical cause |
| --- | --- | --- |
| HTTP-ERROR | HTTP-ERROR-MESSAGE, HTTP-ERROR-OPERATION | Common base for library failures |
| HTTP-PROTOCOL-ERROR | HTTP-PROTOCOL-ERROR-DETAIL | Invalid or contradictory wire framing |
| HTTP-INVALID-URI | HTTP-INVALID-URI-INPUT | URI syntax or component validation failed |
| HTTP-INVALID-HEADER | HTTP-INVALID-HEADER-NAME, HTTP-INVALID-HEADER-REASON | Invalid name, value, or line content |
| HTTP-INVALID-STATUS | HTTP-INVALID-STATUS-LINE, HTTP-INVALID-STATUS-CODE | Malformed or unsupported status line |
| HTTP-CONNECTION-ERROR | HTTP-CONNECTION-ERROR-CAUSE | The injected transport failed |
| HTTP-TIMEOUT | HTTP-TIMEOUT-KIND | A deadline or timeout expired |
| HTTP-SIZE-LIMIT-EXCEEDED | HTTP-SIZE-LIMIT-EXCEEDED-LIMIT, HTTP-SIZE-LIMIT-EXCEEDED-OBSERVED, HTTP-SIZE-LIMIT-EXCEEDED-KIND | A header or body byte limit was exceeded |
| HTTP-UNSUPPORTED-FEATURE | HTTP-UNSUPPORTED-FEATURE-NAME | The requested protocol feature is outside the implementation |

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

## Protocol failures

HTTP-PROTOCOL-ERROR covers contradictory or malformed framing, such as a
conflicting transfer length. HTTP-INVALID-STATUS is used for status-line
validation, while HTTP-INVALID-HEADER identifies invalid header input. The
detail and input readers are intended for diagnostics and programmatic
handling.

## Client policy failures

The optional `cl-http-kit/client` system adds policy-specific conditions. They
retain the core HTTP condition hierarchy and expose the relevant state:

| Condition | Additional readers | Typical cause |
| --- | --- | --- |
| HTTP-CLIENT-ERROR | HTTP-CLIENT-ERROR-DETAIL | Common base for client-policy failures |
| HTTP-REDIRECT-LIMIT-EXCEEDED | HTTP-REDIRECT-LIMIT-EXCEEDED-URI, HTTP-REDIRECT-LIMIT-EXCEEDED-REDIRECTS | The redirect policy exhausted its limit |
| HTTP-RETRY-EXHAUSTED | HTTP-RETRY-EXHAUSTED-ATTEMPTS, HTTP-RETRY-EXHAUSTED-LAST-CONDITION, HTTP-RETRY-EXHAUSTED-LAST-RESPONSE | The retry policy exhausted its attempts |
| HTTP-COOKIE-ERROR | — | A cookie could not be accepted or rendered |
| HTTP-CACHE-ERROR | — | A cache operation failed |
| HTTP-PROXY-ERROR | — | Proxy selection or planning failed |

## Resource failures

HTTP-TIMEOUT identifies the timeout kind, including the default deadline kind.
HTTP-SIZE-LIMIT-EXCEEDED reports the configured limit, the observed size, and
the affected area. HTTP-CONNECTION-ERROR preserves the cause supplied by the
transport boundary.
