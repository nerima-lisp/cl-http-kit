package main

import (
	"bufio"
	"bytes"
	"context"
	"crypto/tls"
	"crypto/x509"
	"errors"
	"flag"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/signal"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
	"time"

	"github.com/quic-go/qpack"
	quic "github.com/quic-go/quic-go"
	"github.com/quic-go/quic-go/http3"
	"github.com/quic-go/quic-go/qlog"
	"github.com/quic-go/quic-go/qlogwriter"
)

type stringList []string

func (values *stringList) String() string {
	return strings.Join(*values, ",")
}

func (values *stringList) Set(value string) error {
	*values = append(*values, value)
	return nil
}

func run() error {
	mode := flag.String("mode", "", "peer mode")
	rawAction := flag.String("raw-action", "", "raw HTTP/3 peer action")
	expectGracefulDrainClose := flag.Bool(
		"expect-graceful-drain-close", false,
		"expect a graceful listener drain after the error callback fails")
	cert := flag.String("cert", "", "certificate PEM")
	key := flag.String("key", "", "private key PEM")
	checkPort := flag.Int("check-port", 0, "verify UDP port release")
	targetPort := flag.Int("target-port", 0, "HTTP/3 server UDP port")
	targetPath := flag.String("path", "/", "HTTP request path")
	method := flag.String("method", http.MethodGet, "HTTP request method")
	body := flag.String("body", "", "HTTP request body")
	requests := flag.Int("requests", 2, "number of concurrent HTTP/3 requests")
	warmUp := flag.Bool("warm-up", false, "establish a connection before concurrent requests")
	var headers stringList
	flag.Var(&headers, "header", "HTTP request header; may be repeated")
	holdAfterHeaders := flag.Bool("hold-after-headers", false, "wait for stdin before reading the response body")
	holdAfterResponse := flag.Bool("hold-after-response", false, "wait for stdin after reading the response")
	slowBodyInterval := flag.Duration("slow-body-interval", 0, "delay between request body bytes")
	ca := flag.String("ca", "", "trusted CA certificate PEM")
	timeout := flag.Duration("timeout", 5*time.Second, "client handshake/request timeout")
	flag.Parse()
	if *checkPort != 0 {
		p, err := net.ListenPacket("udp4", fmt.Sprintf("127.0.0.1:%d", *checkPort))
		if err != nil {
			return err
		}
		return p.Close()
	}
	if *mode == "http3-client" {
		return runHTTP3Client(clientOptions{
			port: *targetPort, path: *targetPath, method: *method, body: *body,
			headers: headers, caPath: *ca, timeout: *timeout,
			holdAfterHeaders: *holdAfterHeaders, holdAfterResponse: *holdAfterResponse,
			slowBodyInterval: *slowBodyInterval,
		})
	}
	if *mode == "http3-concurrent" {
		return runHTTP3Concurrent(clientOptions{
			port: *targetPort, path: *targetPath, method: *method, body: *body,
			headers: headers, caPath: *ca, timeout: *timeout,
			slowBodyInterval: *slowBodyInterval,
		}, *requests, *warmUp)
	}
	if *mode == "alpn-mismatch" {
		return runALPNMismatch(*targetPort, *ca, *timeout)
	}
	if *mode == "pending-handshake" {
		return runPendingHandshake(*targetPort, *ca, *timeout)
	}
	if *mode == "http3-raw" {
		return runRawHTTP3Client(
			*targetPort, *ca, *timeout, *rawAction, *expectGracefulDrainClose)
	}
	if *mode != "control-none" && *mode != "control-prefix-no-settings" && *mode != "normal-delayed-settings" && *mode != "normal-delayed-stream-batch" {
		return fmt.Errorf("invalid peer mode")
	}
	pair, err := tls.LoadX509KeyPair(*cert, *key)
	if err != nil {
		return err
	}
	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt)
	defer cancel()
	go func() { _, _ = io.Copy(io.Discard, os.Stdin); cancel() }()
	listener, err := quic.ListenAddr("127.0.0.1:0", &tls.Config{
		Certificates: []tls.Certificate{pair}, NextProtos: []string{"h3"},
		MinVersion: tls.VersionTLS13,
	}, &quic.Config{KeepAlivePeriod: time.Second})
	if err != nil {
		return err
	}
	defer listener.Close()
	fmt.Printf("READY %d\n", listener.Addr().(*net.UDPAddr).Port)
	conn, err := listener.Accept(ctx)
	if err != nil {
		return err
	}
	defer conn.CloseWithError(0, "fixture cleanup")
	state := conn.ConnectionState().TLS
	if state.NegotiatedProtocol != "h3" || state.ServerName != "localhost" || state.Version != tls.VersionTLS13 {
		return fmt.Errorf("unexpected TLS negotiation")
	}
	fmt.Printf("ACCEPT %s %s TLS13\n", state.NegotiatedProtocol, state.ServerName)
	if *mode == "normal-delayed-stream-batch" {
		select {
		case <-time.After(250 * time.Millisecond):
		case <-ctx.Done():
			return ctx.Err()
		}
		// Queue all three critical stream prefixes before waiting for the client.
		for _, data := range [][]byte{{0, 4, 0}, {2}, {3}} {
			stream, err := conn.OpenUniStream()
			if err != nil {
				return err
			}
			if _, err := stream.Write(data); err != nil {
				return err
			}
			fmt.Printf("STREAM %d TYPE %d\n", stream.StreamID(), data[0])
		}
		fmt.Println("PREFIX 0")
		fmt.Println("SETTINGS 4 0")
	}
	if *mode != "control-none" && *mode != "normal-delayed-stream-batch" {
		stream, err := conn.OpenUniStream()
		if err != nil {
			return err
		}
		if _, err := stream.Write([]byte{0}); err != nil {
			return err
		}
		fmt.Println("PREFIX 0")
		if *mode == "normal-delayed-settings" {
			select {
			case <-time.After(250 * time.Millisecond):
			case <-ctx.Done():
				return ctx.Err()
			}
			if _, err := stream.Write([]byte{4, 0}); err != nil {
				return err
			}
			fmt.Println("SETTINGS 4 0")
		}
		// Keep the control stream open: FIN would test a different contract.
	}
	select {
	case <-ctx.Done():
	case <-conn.Context().Done():
		fmt.Println("CLIENT-CLOSED")
		<-ctx.Done()
	}
	fmt.Println("CLEANUP")
	return nil
}

func clientAddress(port int) (string, error) {
	if port < 1 || port > 65535 {
		return "", fmt.Errorf("target port must be between 1 and 65535")
	}
	return net.JoinHostPort("127.0.0.1", strconv.Itoa(port)), nil
}

func clientTLSConfig(caPath string, protocols []string) (*tls.Config, error) {
	var roots *x509.CertPool
	if caPath == "" {
		var err error
		roots, err = x509.SystemCertPool()
		if err != nil {
			return nil, err
		}
		if roots == nil {
			roots = x509.NewCertPool()
		}
	} else {
		pem, err := os.ReadFile(caPath)
		if err != nil {
			return nil, err
		}
		roots = x509.NewCertPool()
		if !roots.AppendCertsFromPEM(pem) {
			return nil, fmt.Errorf("CA file contains no certificates")
		}
	}
	return &tls.Config{
		MinVersion: tls.VersionTLS13,
		RootCAs:    roots,
		ServerName: "localhost",
		NextProtos: protocols,
	}, nil
}

type retryTrace struct {
	sawRetry atomic.Bool
}

func (t *retryTrace) AddProducer() qlogwriter.Recorder {
	return &retryRecorder{trace: t}
}

func (*retryTrace) SupportsSchemas(string) bool {
	return true
}

type retryRecorder struct {
	trace *retryTrace
}

func (r *retryRecorder) RecordEvent(event qlogwriter.Event) {
	if packet, ok := event.(qlog.PacketReceived); ok && packet.Header.PacketType == qlog.PacketTypeRetry {
		r.trace.sawRetry.Store(true)
	}
}

func (*retryRecorder) Close() error {
	return nil
}

type clientOptions struct {
	port              int
	path              string
	method            string
	body              string
	headers           []string
	caPath            string
	timeout           time.Duration
	holdAfterHeaders  bool
	holdAfterResponse bool
	slowBodyInterval  time.Duration
}

func (options clientOptions) validate() (string, *tls.Config, error) {
	if options.timeout <= 0 {
		return "", nil, fmt.Errorf("timeout must be positive")
	}
	addr, err := clientAddress(options.port)
	if err != nil {
		return "", nil, err
	}
	requestPath, err := url.ParseRequestURI(options.path)
	if err != nil || !strings.HasPrefix(options.path, "/") {
		return "", nil, fmt.Errorf("path must be an absolute request path")
	}
	if options.method == "" {
		return "", nil, fmt.Errorf("method must not be empty")
	}
	if options.slowBodyInterval < 0 {
		return "", nil, fmt.Errorf("slow body interval must not be negative")
	}
	tlsConfig, err := clientTLSConfig(options.caPath, []string{http3.NextProtoH3})
	if err != nil {
		return "", nil, err
	}
	return (&url.URL{
		Scheme:   "https",
		Host:     addr,
		Path:     requestPath.Path,
		RawPath:  requestPath.RawPath,
		RawQuery: requestPath.RawQuery,
	}).String(), tlsConfig, nil
}

func makeRequest(options clientOptions, requestURL string, ctx context.Context) (*http.Request, error) {
	var body io.Reader
	if options.slowBodyInterval > 0 && options.body != "" {
		reader, writer := io.Pipe()
		body = reader
		go func() {
			defer writer.Close()
			for index := 0; index < len(options.body); index++ {
				if _, err := writer.Write([]byte{options.body[index]}); err != nil {
					return
				}
				time.Sleep(options.slowBodyInterval)
			}
		}()
	} else if options.body != "" {
		body = strings.NewReader(options.body)
	}
	request, err := http.NewRequestWithContext(ctx, options.method, requestURL, body)
	if err != nil {
		return nil, err
	}
	if request.Body != nil && options.slowBodyInterval > 0 {
		request.ContentLength = int64(len(options.body))
	}
	for _, field := range options.headers {
		name, value, ok := strings.Cut(field, ":")
		if !ok || strings.TrimSpace(name) == "" {
			return nil, fmt.Errorf("invalid header argument")
		}
		request.Header.Add(strings.TrimSpace(name), strings.TrimSpace(value))
	}
	return request, nil
}

func waitForRelease() error {
	_, err := bufio.NewReader(os.Stdin).ReadByte()
	if err != nil {
		return fmt.Errorf("waiting for fixture release: %w", err)
	}
	return nil
}

func runHTTP3Client(options clientOptions) error {
	requestURL, tlsConfig, err := options.validate()
	if err != nil {
		return err
	}
	trace := &retryTrace{}
	transport := &http3.Transport{
		TLSClientConfig: tlsConfig,
		QUICConfig: &quic.Config{
			Tracer: func(context.Context, bool, quic.ConnectionID) qlogwriter.Trace {
				return trace
			},
		},
	}
	defer transport.Close()
	ctx, cancel := context.WithTimeout(context.Background(), options.timeout)
	defer cancel()
	req, err := makeRequest(options, requestURL, ctx)
	if err != nil {
		return err
	}
	resp, err := transport.RoundTrip(req)
	if err != nil {
		return fmt.Errorf("HTTP/3 request failed (Retry=%t): %w", trace.sawRetry.Load(), err)
	}
	fmt.Printf("HTTP3 HEADERS STATUS=%d RETRY=%t\n", resp.StatusCode, trace.sawRetry.Load())
	if options.holdAfterHeaders {
		if err := waitForRelease(); err != nil {
			_ = resp.Body.Close()
			return err
		}
	}
	body, readErr := io.ReadAll(resp.Body)
	closeErr := resp.Body.Close()
	if readErr != nil {
		return readErr
	}
	if closeErr != nil {
		return closeErr
	}
	fmt.Printf("HTTP3 STATUS=%d RETRY=%t BODY=%q\n", resp.StatusCode, trace.sawRetry.Load(), string(body))
	if options.holdAfterResponse {
		if err := waitForRelease(); err != nil {
			return err
		}
	}
	return nil
}

func runHTTP3Concurrent(options clientOptions, count int, warmUp bool) error {
	if count < 2 || count > 32 {
		return fmt.Errorf("request count must be between 2 and 32")
	}
	requestURL, tlsConfig, err := options.validate()
	if err != nil {
		return err
	}
	trace := &retryTrace{}
	transport := &http3.Transport{
		TLSClientConfig: tlsConfig,
		QUICConfig: &quic.Config{
			Tracer: func(context.Context, bool, quic.ConnectionID) qlogwriter.Trace {
				return trace
			},
		},
	}
	defer transport.Close()
	if warmUp {
		warmUpURL, err := url.Parse(requestURL)
		if err != nil {
			return err
		}
		warmUpURL.Path = "/__cl_http_kit_warmup"
		warmUpURL.RawPath = ""
		warmUpURL.RawQuery = ""
		ctx, cancel := context.WithTimeout(context.Background(), options.timeout)
		defer cancel()
		request, err := http.NewRequestWithContext(ctx, http.MethodGet, warmUpURL.String(), nil)
		if err != nil {
			return err
		}
		response, err := transport.RoundTrip(request)
		if err != nil {
			return err
		}
		if _, err := io.Copy(io.Discard, response.Body); err != nil {
			_ = response.Body.Close()
			return err
		}
		if err := response.Body.Close(); err != nil {
			return err
		}
		if response.StatusCode != http.StatusNoContent {
			return fmt.Errorf("unexpected HTTP/3 warm-up status: %d", response.StatusCode)
		}
	}
	type result struct {
		index  int
		status int
		body   string
		err    error
	}
	results := make(chan result, count)
	var writers sync.WaitGroup
	for index := 0; index < count; index++ {
		writers.Add(1)
		go func(index int) {
			defer writers.Done()
			ctx, cancel := context.WithTimeout(context.Background(), options.timeout)
			defer cancel()
			request, err := makeRequest(options, requestURL, ctx)
			if err != nil {
				results <- result{index: index, err: err}
				return
			}
			response, err := transport.RoundTrip(request)
			if err != nil {
				results <- result{index: index, err: err}
				return
			}
			body, readErr := io.ReadAll(response.Body)
			closeErr := response.Body.Close()
			if readErr != nil {
				results <- result{index: index, err: readErr}
				return
			}
			if closeErr != nil {
				results <- result{index: index, err: closeErr}
				return
			}
			results <- result{index: index, status: response.StatusCode, body: string(body)}
		}(index)
	}
	go func() {
		writers.Wait()
		close(results)
	}()
	for result := range results {
		if result.err != nil {
			fmt.Printf("HTTP3 RESULT INDEX=%d ERROR=%q\n", result.index, result.err.Error())
		} else {
			fmt.Printf("HTTP3 RESULT INDEX=%d STATUS=%d RETRY=%t BODY=%q\n",
				result.index, result.status, trace.sawRetry.Load(), result.body)
		}
	}
	return nil
}

type pendingHandshakePacketConn struct {
	net.PacketConn
	retryTrace        *retryTrace
	afterRetryInitial atomic.Bool
	retryInitialSent  chan struct{}
	retryInitialOnce  sync.Once
}

func (conn *pendingHandshakePacketConn) SetReadBuffer(size int) error {
	return conn.PacketConn.(*net.UDPConn).SetReadBuffer(size)
}

func (conn *pendingHandshakePacketConn) SetWriteBuffer(size int) error {
	return conn.PacketConn.(*net.UDPConn).SetWriteBuffer(size)
}

func (conn *pendingHandshakePacketConn) WriteTo(packet []byte, address net.Addr) (int, error) {
	afterRetry := conn.retryTrace.sawRetry.Load()
	if afterRetry {
		conn.afterRetryInitial.Store(true)
	}
	written, err := conn.PacketConn.WriteTo(packet, address)
	if afterRetry && err == nil {
		conn.retryInitialOnce.Do(func() { close(conn.retryInitialSent) })
	}
	return written, err
}

func (conn *pendingHandshakePacketConn) ReadFrom(packet []byte) (int, net.Addr, error) {
	for {
		read, address, err := conn.PacketConn.ReadFrom(packet)
		if err != nil {
			return read, address, err
		}
		if conn.afterRetryInitial.Load() {
			continue
		}
		return read, address, nil
	}
}

func runPendingHandshake(port int, caPath string, timeout time.Duration) error {
	if timeout <= 0 {
		return fmt.Errorf("timeout must be positive")
	}
	address, err := net.ResolveUDPAddr("udp4", net.JoinHostPort("127.0.0.1", strconv.Itoa(port)))
	if err != nil {
		return err
	}
	tlsConfig, err := clientTLSConfig(caPath, []string{http3.NextProtoH3})
	if err != nil {
		return err
	}
	packetSocket, err := net.ListenPacket("udp4", "127.0.0.1:0")
	if err != nil {
		return err
	}
	trace := &retryTrace{}
	packetConn := &pendingHandshakePacketConn{
		PacketConn:       packetSocket,
		retryTrace:       trace,
		retryInitialSent: make(chan struct{}),
	}
	transport := &quic.Transport{Conn: packetConn}
	defer transport.Close()
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	dialResult := make(chan error, 1)
	go func() {
		connection, dialErr := transport.Dial(ctx, address, tlsConfig, &quic.Config{
			Tracer: func(context.Context, bool, quic.ConnectionID) qlogwriter.Trace {
				return trace
			},
		})
		if connection != nil {
			_ = connection.CloseWithError(0, "fixture cleanup")
		}
		dialResult <- dialErr
	}()
	select {
	case <-packetConn.retryInitialSent:
		fmt.Printf("PENDING READY RETRY=%t\n", trace.sawRetry.Load())
	case err := <-dialResult:
		return fmt.Errorf("handshake completed before becoming pending: %w", err)
	case <-ctx.Done():
		return ctx.Err()
	}
	if err := waitForRelease(); err != nil {
		_ = packetConn.Close()
		return err
	}
	cancel()
	_ = packetConn.Close()
	select {
	case <-dialResult:
		fmt.Println("PENDING CLOSED")
		return nil
	case <-time.After(time.Second):
		return fmt.Errorf("pending handshake did not stop after fixture release")
	}
}

func runALPNMismatch(port int, caPath string, timeout time.Duration) error {
	if timeout <= 0 {
		return fmt.Errorf("timeout must be positive")
	}
	addr, err := clientAddress(port)
	if err != nil {
		return err
	}
	tlsConfig, err := clientTLSConfig(caPath, []string{"h2"})
	if err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	conn, err := quic.DialAddr(ctx, addr, tlsConfig, &quic.Config{})
	if err != nil {
		if errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded) {
			return err
		}
		message := strings.ToLower(err.Error())
		if !strings.Contains(message, "application protocol") &&
			!strings.Contains(message, "crypto_error 0x178") {
			return err
		}
		fmt.Println("ALPN_MISMATCH REJECTED=true")
		return nil
	}
	defer conn.CloseWithError(0, "fixture cleanup")
	fmt.Printf("ALPN_MISMATCH REJECTED=false ALPN=%q\n", conn.ConnectionState().TLS.NegotiatedProtocol)
	return nil
}

func runRawHTTP3Client(
	port int, caPath string, timeout time.Duration, action string, expectGracefulDrainClose bool,
) error {
	if timeout <= 0 {
		return fmt.Errorf("timeout must be positive")
	}
	addr, err := clientAddress(port)
	if err != nil {
		return err
	}
	tlsConfig, err := clientTLSConfig(caPath, []string{http3.NextProtoH3})
	if err != nil {
		return err
	}
	ctx, cancel := context.WithTimeout(context.Background(), timeout)
	defer cancel()
	conn, err := quic.DialAddr(ctx, addr, tlsConfig, &quic.Config{})
	if err != nil {
		return fmt.Errorf("raw HTTP/3 QUIC connection failed: %w", err)
	}
	defer conn.CloseWithError(0, "fixture cleanup")

	openControl := func(payload []byte, finish bool) (*quic.SendStream, error) {
		stream, err := conn.OpenUniStreamSync(ctx)
		if err != nil {
			return nil, err
		}
		if _, err := stream.Write(payload); err != nil {
			return nil, err
		}
		if finish {
			if err := stream.Close(); err != nil {
				return nil, err
			}
		}
		return stream, nil
	}
	waitForStreamReset := func(stream *quic.Stream, label string) error {
		if err := stream.SetReadDeadline(time.Now().Add(timeout)); err != nil {
			return err
		}
		_, err := stream.Read(make([]byte, 1))
		if err == nil {
			return fmt.Errorf("server unexpectedly wrote to raw %s stream", label)
		}
		var streamError *quic.StreamError
		if !errors.As(err, &streamError) {
			return fmt.Errorf("raw %s stream ended without reset: %w", label, err)
		}
		fmt.Printf("RAW STREAM RESET action=%s code=%#x\n", label, streamError.ErrorCode)
		return nil
	}
	waitForConnectionClose := func(label string) error {
		select {
		case <-conn.Context().Done():
			cause := context.Cause(conn.Context())
			var closeError *quic.ApplicationError
			if !errors.As(cause, &closeError) {
				var transportError *quic.TransportError
				if expectGracefulDrainClose &&
					label == "malformed-control" &&
					errors.As(cause, &transportError) &&
					transportError.Remote &&
					transportError.ErrorCode == quic.NoError &&
					transportError.ErrorMessage == "HTTP/3 request drain complete" {
					fmt.Printf(
						"RAW CONNECTION CLOSED action=%s remote=true code=%#x reason=%q\n",
						label, uint64(transportError.ErrorCode), transportError.ErrorMessage)
					return nil
				}
				fmt.Printf("RAW CONNECTION CLOSED action=%s cause-type=%T cause=%q\n", label, cause, cause)
				return fmt.Errorf("raw %s connection closed with unexpected QUIC cause %T: %v", label, cause, cause)
			}
			fmt.Printf("RAW CONNECTION CLOSED action=%s remote=%t code=%#x reason=%q\n", label, closeError.Remote, closeError.ErrorCode, closeError.ErrorMessage)
			if !closeError.Remote {
				return fmt.Errorf("server did not remotely close raw %s connection: code=%#x reason=%q", label, closeError.ErrorCode, closeError.ErrorMessage)
			}
			return nil
		case <-ctx.Done():
			return fmt.Errorf("server did not close raw %s connection: %w", label, ctx.Err())
		}
	}
	switch action {
	case "streams":
		if _, err := openControl([]byte{0, 4, 0}, false); err != nil {
			return err
		}
		for _, streamType := range []byte{2, 3, 33} {
			stream, err := conn.OpenUniStreamSync(ctx)
			if err != nil {
				return err
			}
			if _, err := stream.Write([]byte{streamType}); err != nil {
				return err
			}
			// Only the unknown stream is finished; FIN on a critical QPACK stream is invalid.
			if streamType == 33 {
				if err := stream.Close(); err != nil {
					return err
				}
			}
		}
		fmt.Println("RAW STREAMS READY")
		return waitForConnectionClose(action)
	case "greasing-streams":
		if _, err := openControl([]byte{0, 4, 0, 0x21, 0, 0x40, 0x40, 0}, false); err != nil {
			return err
		}
		for _, streamType := range []byte{2, 3, 0x22} {
			stream, err := conn.OpenUniStreamSync(ctx)
			if err != nil {
				return err
			}
			if _, err := stream.Write([]byte{streamType}); err != nil {
				return err
			}
			if streamType == 0x22 {
				if err := stream.Close(); err != nil {
					return err
				}
			}
		}
		fmt.Println("RAW GREASING STREAMS READY")
		if err := waitForRelease(); err != nil {
			return err
		}
		var fieldSection bytes.Buffer
		encoder := qpack.NewEncoder(&fieldSection)
		for _, field := range []qpack.HeaderField{
			{Name: ":method", Value: "GET"},
			{Name: ":scheme", Value: "https"},
			{Name: ":authority", Value: "localhost"},
			{Name: ":path", Value: "/"},
		} {
			if err := encoder.WriteField(field); err != nil {
				return err
			}
		}
		if err := encoder.Close(); err != nil {
			return err
		}
		if fieldSection.Len() >= 64 {
			return fmt.Errorf("fixture request field section exceeds one-byte frame length")
		}
		request, err := conn.OpenStreamSync(ctx)
		if err != nil {
			return err
		}
		if _, err := request.Write(append([]byte{1, byte(fieldSection.Len())}, fieldSection.Bytes()...)); err != nil {
			return err
		}
		if err := request.Close(); err != nil {
			return err
		}
		if err := request.SetReadDeadline(time.Now().Add(timeout)); err != nil {
			return err
		}
		responseFrameType := make([]byte, 1)
		if _, err := io.ReadFull(request, responseFrameType); err != nil {
			return fmt.Errorf("server did not serve a request after greasing streams: %w", err)
		}
		if responseFrameType[0] != 1 {
			return fmt.Errorf("unexpected HTTP/3 response frame after greasing streams: %#x", responseFrameType[0])
		}
		fmt.Println("RAW GREASING STREAMS RESPONSE")
		if err := waitForRelease(); err != nil {
			return err
		}
		select {
		case <-conn.Context().Done():
			return fmt.Errorf("server closed raw greasing connection before action completed: %v", context.Cause(conn.Context()))
		default:
		}
		fmt.Println("RAW GREASING STREAMS SENT")
		return nil
	case "partial-unidirectional-prefix":
		partialStream, err := conn.OpenUniStreamSync(ctx)
		if err != nil {
			return err
		}
		if _, err := partialStream.Write([]byte{0x40}); err != nil {
			return err
		}
		fmt.Println("RAW PARTIAL UNI SENT")
		if err := waitForRelease(); err != nil {
			return err
		}
		if _, err := openControl([]byte{0, 4, 0}, false); err != nil {
			return err
		}
		fmt.Println("RAW PARTIAL UNI CONTROL SENT")
		return waitForRelease()
	case "peer-cancels-streams":
		if _, err := openControl([]byte{0, 4, 0}, false); err != nil {
			return err
		}
		peerStream, err := conn.OpenStreamSync(ctx)
		if err != nil {
			return err
		}
		if _, err := peerStream.Write([]byte{1}); err != nil {
			return err
		}
		time.Sleep(100 * time.Millisecond)
		peerStream.CancelWrite(0x123)
		peerStream.CancelRead(0x124)
		fmt.Println("RAW PEER RESET_STREAM STOP_SENDING")
		time.Sleep(250 * time.Millisecond)

		followupConn, err := quic.DialAddr(ctx, addr, tlsConfig, &quic.Config{})
		if err != nil {
			return fmt.Errorf("server stopped after peer stream cancellation: %w", err)
		}
		defer followupConn.CloseWithError(0, "fixture cleanup")
		control, err := followupConn.OpenUniStreamSync(ctx)
		if err != nil {
			return err
		}
		if _, err := control.Write([]byte{0, 4, 0}); err != nil {
			return err
		}

		var fieldSection bytes.Buffer
		encoder := qpack.NewEncoder(&fieldSection)
		for _, field := range []qpack.HeaderField{
			{Name: ":method", Value: "GET"},
			{Name: ":scheme", Value: "https"},
			{Name: ":authority", Value: "localhost"},
			{Name: ":path", Value: "/"},
		} {
			if err := encoder.WriteField(field); err != nil {
				return err
			}
		}
		if err := encoder.Close(); err != nil {
			return err
		}
		if fieldSection.Len() >= 64 {
			return fmt.Errorf("fixture request field section exceeds one-byte frame length")
		}
		request, err := followupConn.OpenStreamSync(ctx)
		if err != nil {
			return err
		}
		if _, err := request.Write(append([]byte{1, byte(fieldSection.Len())}, fieldSection.Bytes()...)); err != nil {
			return err
		}
		if err := request.Close(); err != nil {
			return err
		}
		if err := request.SetReadDeadline(time.Now().Add(timeout)); err != nil {
			return err
		}
		responseFrameType := make([]byte, 1)
		if _, err := io.ReadFull(request, responseFrameType); err != nil {
			return fmt.Errorf("server did not serve a request after peer cancellation: %w", err)
		}
		if responseFrameType[0] != 1 {
			return fmt.Errorf("unexpected follow-up HTTP/3 response frame type: %#x", responseFrameType[0])
		}
		fmt.Println("RAW FOLLOWUP RESPONSE")
		return nil
	case "push":
		if _, err := openControl([]byte{0, 4, 0}, false); err != nil {
			return err
		}
		stream, err := conn.OpenUniStreamSync(ctx)
		if err != nil {
			return err
		}
		if _, err := stream.Write([]byte{1, 0}); err != nil {
			return err
		}
		if err := stream.Close(); err != nil {
			return err
		}
		return waitForConnectionClose(action)
	case "malformed-control":
		if _, err := openControl([]byte{0, 4, 1, 0}, true); err != nil {
			return err
		}
		return waitForConnectionClose(action)
	case "http2-reserved-frame-02", "http2-reserved-frame-06", "http2-reserved-frame-08", "http2-reserved-frame-09":
		frameType, err := strconv.ParseUint(strings.TrimPrefix(action, "http2-reserved-frame-"), 16, 8)
		if err != nil {
			return fmt.Errorf("invalid HTTP/2 reserved frame action %q: %w", action, err)
		}
		if _, err := openControl([]byte{0, 4, 0, byte(frameType), 0}, false); err != nil {
			return err
		}
		return waitForConnectionClose(action)
	case "http2-reserved-request-frame-02", "http2-reserved-request-frame-06", "http2-reserved-request-frame-08", "http2-reserved-request-frame-09":
		frameType, err := strconv.ParseUint(strings.TrimPrefix(action, "http2-reserved-request-frame-"), 16, 8)
		if err != nil {
			return fmt.Errorf("invalid HTTP/2 reserved request frame action %q: %w", action, err)
		}
		if _, err := openControl([]byte{0, 4, 0}, false); err != nil {
			return err
		}
		stream, err := conn.OpenStreamSync(ctx)
		if err != nil {
			return err
		}
		if _, err := stream.Write([]byte{byte(frameType), 0}); err != nil {
			return err
		}
		if err := stream.Close(); err != nil {
			return err
		}
		return waitForConnectionClose(action)
	case "malformed-request":
		if _, err := openControl([]byte{0, 4, 0}, false); err != nil {
			return err
		}
		stream, err := conn.OpenStreamSync(ctx)
		if err != nil {
			return err
		}
		// Reference dynamic-table entry 0 with a Required Insert Count of zero.
		if _, err := stream.Write([]byte{1, 3, 0, 0, 0x80}); err != nil {
			return err
		}
		if err := stream.Close(); err != nil {
			return err
		}
		return waitForConnectionClose(action)
	case "no-settings", "slow-request":
		if action == "slow-request" {
			if _, err := openControl([]byte{0, 4, 0}, false); err != nil {
				return err
			}
		}
		stream, err := conn.OpenStreamSync(ctx)
		if err != nil {
			return err
		}
		switch action {
		case "no-settings":
			if _, err := stream.Write([]byte{1, 0}); err != nil {
				return err
			}
		case "slow-request":
			if _, err := stream.Write([]byte{1, 10, 0}); err != nil {
				return err
			}
		}
		return waitForStreamReset(stream, action)
	default:
		return fmt.Errorf("invalid raw HTTP/3 action")
	}
}

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
