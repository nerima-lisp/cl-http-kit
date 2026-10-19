package main

import (
	"context"
	"crypto/tls"
	"flag"
	"fmt"
	"io"
	"net"
	"os"
	"os/signal"
	"time"

	quic "github.com/quic-go/quic-go"
)

func run() error {
	mode := flag.String("mode", "", "peer mode")
	cert := flag.String("cert", "", "certificate PEM")
	key := flag.String("key", "", "private key PEM")
	checkPort := flag.Int("check-port", 0, "verify UDP port release")
	flag.Parse()
	if *checkPort != 0 {
		p, err := net.ListenPacket("udp4", fmt.Sprintf("127.0.0.1:%d", *checkPort))
		if err != nil {
			return err
		}
		return p.Close()
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

func main() {
	if err := run(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
