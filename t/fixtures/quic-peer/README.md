# QUIC peer fixture

This fixture is a test-only raw QUIC peer for the HTTP/3 listener tests. The pinned Nix package builds it with Go 1.26.5 through `pkgs.go_1_26`.

## Known standard-library vulnerabilities

The `govulncheck` 1.9.0 scan of this fixture with Go 1.27rc2 reported nine Go standard-library vulnerabilities reachable from the fixture's code. The pinned Go 1.26.5 toolchain predates the fixes listed below. The Go Vulnerability Database reports that GO-2026-6090 and GO-2026-5972 were first fixed in Go 1.27.0-rc.3; both fixes are included in stable Go 1.27.0.

| Go vulnerability | First fixed in Go 1.26 | First fixed in Go 1.27 |
| --- | --- | --- |
| [GO-2026-6809](https://pkg.go.dev/vuln/GO-2026-6809) | 1.26.10 | 1.27.3 |
| [GO-2026-6617](https://pkg.go.dev/vuln/GO-2026-6617) | 1.26.9 | 1.27.2 |
| [GO-2026-6613](https://pkg.go.dev/vuln/GO-2026-6613) | 1.26.9 | 1.27.2 |
| [GO-2026-6611](https://pkg.go.dev/vuln/GO-2026-6611) | 1.26.9 | 1.27.2 |
| [GO-2026-6608](https://pkg.go.dev/vuln/GO-2026-6608) | 1.26.9 | 1.27.2 |
| [GO-2026-6607](https://pkg.go.dev/vuln/GO-2026-6607) | 1.26.9 | 1.27.2 |
| [GO-2026-6603](https://pkg.go.dev/vuln/GO-2026-6603) | 1.26.9 | 1.27.2 |
| [GO-2026-6090](https://pkg.go.dev/vuln/GO-2026-6090) | 1.26.6 | 1.27.0-rc.3 |
| [GO-2026-5972](https://pkg.go.dev/vuln/GO-2026-5972) | 1.26.6 | 1.27.0-rc.3 |

See the [Go release history](https://go.dev/doc/devel/release) for stable release details.
