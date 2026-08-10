(in-package #:asdf-user)

(asdf:defsystem "cl-http-kit"
  :description "A portable, binary-safe HTTP client substrate."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.2.0"
  :depends-on ()
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "model-declarations")
               (:file "status-reasons")
               (:file "defaults")
               (:file "utilities")
               (:file "control-macros")
               (:file "uri")
               (:file "header-data")
               (:file "headers")
               (:file "model")
               (:file "http1-serialize")
               (:file "http1-source")
               (:file "http1-response-headers")
               (:file "http1-response-body")
               (:file "http1-parse")
               (:file "http1-server")
               (:file "transport-declarations")
               (:file "transport")
               (:file "recording-data")
               (:file "recording-transport"))
  :in-order-to ((test-op (test-op "cl-http-kit/test"))))

(asdf:defsystem "cl-http-kit/observability"
  :description "Optional cl-observability-kit metrics for cl-http-kit."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.2.0"
  :depends-on ("cl-http-kit" "cl-observability-kit")
  :pathname "src"
  :serial t
  :components ((:file "observability-package")
               (:file "observability-declarations")
               (:file "observability")))

(asdf:defsystem "cl-http-kit/http2"
  :description "The optional HTTP/2 transport for cl-http-kit."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.2.0"
  :depends-on ("cl-http-kit")
  :pathname "http2"
  :serial t
  :components ((:file "package")
               (:file "hpack-data")
               (:file "hpack-huffman-declarations")
               (:file "hpack-huffman")
               (:file "hpack-huffman-data")
               (:file "hpack-context")
               (:file "hpack")
               (:file "frame-data")
               (:file "frames")
               (:file "transport-declarations")
               (:file "transport-core")
               (:file "transport-data")
               (:file "transport-headers")
               (:file "transport-request")
               (:file "transport-write")
               (:file "transport-read-headers")
               (:file "transport-read-body")
               (:file "transport-read-response")
               (:file "transport-connection")
               (:file "transport-manager")
               (:file "transport-server")))

(asdf:defsystem "cl-http-kit/client"
  :description "The high-level HTTP client policies and request orchestration layer."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.2.0"
  :depends-on ("cl-http-kit")
  :pathname "client"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "data")
               (:file "multipart")
               (:file "websocket-frame")
               (:file "websocket-crypto")
               (:file "websocket-handshake")
               (:file "websocket-message")
               (:file "sse-data")
               (:file "sse-parser")
               (:file "sse-serialize")
               (:file "uri")
               (:file "date")
               (:file "auth")
               (:file "cookies")
               (:file "cache")
               (:file "proxy")
               (:file "proxy-transport")
               (:file "connection")
               (:file "client")))

(asdf:defsystem "cl-http-kit/network"
  :description "Optional native TCP and DNS boundary for cl-http-kit."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.2.0"
  :depends-on ("cl-http-kit")
  :pathname "network"
  :serial t
  :components ((:file "package")
               (:file "socket")))

(asdf:defsystem "cl-http-kit/http3"
  :description "Optional HTTP/3 framing, QPACK, and injected QUIC transport boundary."
  :author "nerima-lisp"
  :license "MIT"
  :version "0.2.0"
  :depends-on ("cl-http-kit" "cl-http-kit/http2")
  :pathname "http3"
  :serial t
  :components ((:file "package")
               (:file "varint")
               (:file "qpack")
               (:file "frames")
               (:file "transport")))

(asdf:defsystem "cl-http-kit/test"
  :description "Tests for cl-http-kit and its optional HTTP/2 transport."
  :depends-on ("cl-http-kit/client"
               "cl-http-kit/http2"
               "cl-http-kit/http3"
               "cl-http-kit/network"
               "cl-http-kit/observability"
               "cl-weave")
  :pathname "t"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "tests-utilities-boundaries")
               (:file "tests-model-header-boundaries")
               (:file "tests-stream-transport")
               (:file "tests-transport-cps")
               (:file "tests-observability")
               (:file "tests-core")
               (:file "tests-uri-boundaries")
               (:file "tests-http1-boundaries")
               (:file "tests-http1-parse-boundaries")
               (:file "tests-http1-server-boundaries")
               (:file "tests-hpack-boundaries")
               (:file "tests-boundaries")
               (:file "tests-http2-control-frame-boundaries")
               (:file "tests-http2-request-wire-boundaries")
               (:file "tests-http2-transport-settings-boundaries")
               (:file "tests-http2-frame-wire-boundaries")
               (:file "tests-http2-header-errors")
               (:file "tests-http2-response-errors")
               (:file "tests-http2-read-errors")
               (:file "tests-http2-open-stream")
               (:file "tests-http2-response-boundaries")
               (:file "tests-http2-frame-boundaries")
               (:file "tests-http2-failure-boundaries")
               (:file "tests-http2-manager")
               (:file "tests-http2-server")
               (:file "tests-http3")
               (:file "tests-properties")
               (:file "tests-client")
               (:file "tests-network")
               (:file "runner"))
  :perform (asdf:test-op (op c)
             (declare (ignore op c))
             (uiop:symbol-call "HTTP-KIT/TEST" "RUN-TESTS")))
