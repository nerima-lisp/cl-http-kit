{
  description = "A production-oriented, binary-safe Common Lisp HTTP client substrate.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    cl-weave = {
      url = "github:nerima-lisp/cl-weave/v1.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-observability-kit = {
      url = "github:nerima-lisp/cl-observability-kit/v0.1.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-weave.follows = "cl-weave";
    };

    cl-concurrent-kit = {
      url = "github:nerima-lisp/cl-concurrent-kit/v0.6.1";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-boundary-kit.follows = "cl-boundary-kit";
      inputs.cl-date-kit.follows = "cl-date-kit";
    };

    cl-boundary-kit = {
      url = "github:nerima-lisp/cl-boundary-kit/v2.3.0";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.cl-host-kit.follows = "cl-host-kit";
    };

    cl-date-kit = {
      url = "github:nerima-lisp/cl-date-kit/v1.0.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-host-kit = {
      url = "github:nerima-lisp/cl-host-kit/v0.3.1";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    cl-codec-kit = {
      url = "github:nerima-lisp/cl-codec-kit/v0.5.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    paredit-cli = {
      url = "github:takeokunn/paredit-cli/v1.6.0";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      nixpkgs,
      cl-weave,
      cl-observability-kit,
      cl-concurrent-kit,
      cl-boundary-kit,
      cl-date-kit,
      cl-host-kit,
      cl-codec-kit,
      paredit-cli,
      treefmt-nix,
      ...
    }:
    let
      systems = [
        "aarch64-darwin"
        "x86_64-linux"
      ];
      forEachSystem =
        function:
        nixpkgs.lib.genAttrs systems (system: function system (import nixpkgs { inherit system; }));
    in
    {
      formatter = forEachSystem (
        system: pkgs:
        (treefmt-nix.lib.evalModule pkgs {
          projectRootFile = "flake.nix";
          programs.nixfmt.enable = true;
        }).config.build.wrapper
      );

      devShells = forEachSystem (
        system: pkgs:
        let
          clWeave = cl-weave.packages.${system}.default;
          observability = cl-observability-kit.packages.${system}.default;
          concurrent = cl-concurrent-kit.packages.${system}.default;
          boundary = cl-boundary-kit.packages.${system}.default;
          date = cl-date-kit.packages.${system}.default;
          host = cl-host-kit.packages.${system}.default;
          codec = cl-codec-kit.packages.${system}.default;
          chipz = pkgs.sbclPackages.chipz;
          paredit = paredit-cli.packages.${system}.default;
        in
        {
          default = pkgs.mkShell {
            packages = [
              clWeave
              observability
              concurrent
              boundary
              date
              host
              codec
              chipz
              paredit
              pkgs.sbcl
              pkgs.coreutils
              pkgs.perl
              pkgs.ripgrep
              pkgs.python3Packages.mkdocs
              pkgs.python3Packages.mkdocs-material
              pkgs.python3Packages.pymdown-extensions
            ];
            shellHook = ''
              export CL_SOURCE_REGISTRY="''${CL_SOURCE_REGISTRY:-$PWD//:${
                pkgs.lib.concatStringsSep ":" [
                  "${clWeave}/share/common-lisp/source//"
                  "${observability}//"
                  "${concurrent}//"
                  "${boundary}//"
                  "${date}//"
                  "${host}//"
                  "${codec}//"
                ]
              }}"
            '';
          };
        }
      );

      apps = forEachSystem (
        system: pkgs:
        let
          clWeave = cl-weave.packages.${system}.default;
          observability = cl-observability-kit.packages.${system}.default;
          concurrent = cl-concurrent-kit.packages.${system}.default;
          boundary = cl-boundary-kit.packages.${system}.default;
          date = cl-date-kit.packages.${system}.default;
          host = cl-host-kit.packages.${system}.default;
          codec = cl-codec-kit.packages.${system}.default;
          chipz = pkgs.sbclPackages.chipz;
          paredit = paredit-cli.packages.${system}.default;
          sourceRegistry = pkgs.lib.concatStringsSep ":" [
            "${clWeave}/share/common-lisp/source//"
            "${observability}//"
            "${concurrent}//"
            "${boundary}//"
            "${date}//"
            "${host}//"
            "${codec}//"
            "${chipz}//"
          ];
          appMeta = {
            description = "Common Lisp HTTP client quality gate";
          };

          testCore = pkgs.writeShellApplication {
            name = "cl-http-kit-test-core";
            runtimeInputs = [
              clWeave
              observability
              concurrent
              boundary
              date
              host
              codec
            ];
            text = ''
              test_home="''${TMPDIR:-/tmp}/cl-http-kit-test-core-$$"
              mkdir -p "$test_home"
              export HOME="$test_home"
              export XDG_CACHE_HOME="$test_home/.cache"
              export CL_SOURCE_REGISTRY="''${CL_SOURCE_REGISTRY:-$PWD//:${sourceRegistry}}"
              test_timeout_ms="''${CL_WEAVE_TEST_TIMEOUT_MS:-30000}"
              max_workers="''${CL_WEAVE_MAX_WORKERS:-1}"
              exec cl-weave run cl-http-kit/test-core \
                --test-timeout-ms "$test_timeout_ms" \
                --max-workers "$max_workers" \
                --bail true \
                --fail-with-no-tests "$@"
            '';
          };

          test = pkgs.writeShellApplication {
            name = "cl-http-kit-test";
            runtimeInputs = [
              clWeave
              observability
              concurrent
              boundary
              date
              host
              codec
              chipz
            ];
            text = ''
              test_home="''${TMPDIR:-/tmp}/cl-http-kit-test-$$"
              mkdir -p "$test_home"
              export HOME="$test_home"
              export XDG_CACHE_HOME="$test_home/.cache"
              export CL_SOURCE_REGISTRY="''${CL_SOURCE_REGISTRY:-$PWD//:${sourceRegistry}}"
              test_timeout_ms="''${CL_WEAVE_TEST_TIMEOUT_MS:-30000}"
              max_workers="''${CL_WEAVE_MAX_WORKERS:-1}"
              exec cl-weave run cl-http-kit/test \
                --test-timeout-ms "$test_timeout_ms" \
                --max-workers "$max_workers" \
                --bail true \
                --fail-with-no-tests "$@"
            '';
          };

          coverage = pkgs.writeShellApplication {
            name = "cl-http-kit-coverage";
            runtimeInputs = [
              clWeave
              observability
              concurrent
              boundary
              date
              host
              chipz
              pkgs.coreutils
              pkgs.findutils
              pkgs.perl
            ];
            text = ''
              coverage_home="''${TMPDIR:-/tmp}/cl-http-kit-coverage-$$"
              mkdir -p "$coverage_home"
              export HOME="$coverage_home"
              export XDG_CACHE_HOME="$coverage_home/.cache"
              export CL_SOURCE_REGISTRY="''${CL_SOURCE_REGISTRY:-$PWD//:${sourceRegistry}}"
              coverage_root="$(realpath -m -- "$PWD")"
              coverage_report_directory="$(realpath -m -- "''${CL_HTTP_KIT_COVERAGE_DIR:-coverage}")"
              case "$coverage_report_directory" in
                "$coverage_root")
                  echo "Coverage directory must not be the repository root." >&2
                  exit 1
                  ;;
                "$coverage_root"/*) ;;
                *)
                  echo "Coverage directory must be inside the repository." >&2
                  exit 1
                  ;;
              esac
              test_timeout_ms="''${CL_WEAVE_TEST_TIMEOUT_MS:-30000}"
              max_workers="''${CL_WEAVE_MAX_WORKERS:-1}"
              coverage_timeout_seconds="''${CL_HTTP_KIT_COVERAGE_TIMEOUT_SECONDS:-900}"
              mkdir -p "$coverage_report_directory"
              find "$coverage_report_directory" \
                -mindepth 1 \
                -maxdepth 1 \
                -type f \
                -name '*.html' \
                -delete
              CL_HTTP_KIT_COVERAGE_TIMEOUT_SECONDS="$coverage_timeout_seconds" \
                perl -e 'alarm $ENV{CL_HTTP_KIT_COVERAGE_TIMEOUT_SECONDS}; exec @ARGV' \
                cl-weave run cl-http-kit/test \
                --coverage \
                --coverage-system cl-http-kit \
                --coverage-system cl-http-kit/http2 \
                --coverage-system cl-http-kit/http3 \
                --coverage-system cl-http-kit/observability \
                --coverage-system cl-http-kit/client \
                --coverage-exclude src/package.lisp \
                --coverage-exclude src/conditions.lisp \
                --coverage-exclude src/model-declarations.lisp \
                --coverage-exclude src/status-reasons.lisp \
                --coverage-exclude src/defaults.lisp \
                --coverage-exclude src/control-macros.lisp \
                --coverage-exclude src/observability-package.lisp \
                --coverage-exclude src/observability-declarations.lisp \
                --coverage-exclude http2/package.lisp \
                --coverage-exclude http2/hpack-data.lisp \
                --coverage-exclude http2/hpack-huffman-declarations.lisp \
                --coverage-exclude src/header-data.lisp \
                --coverage-exclude src/recording-data.lisp \
                --coverage-exclude src/transport-declarations.lisp \
                --coverage-exclude http2/frame-data.lisp \
                --coverage-exclude http2/hpack-huffman-data.lisp \
                --coverage-exclude http2/transport-data.lisp \
                --coverage-exclude http2/transport-declarations.lisp \
                --coverage-exclude client/package.lisp \
                --coverage-exclude client/conditions.lisp \
                --coverage-exclude client/data.lisp \
                --coverage-min-expression 100 \
                --coverage-min-branch 100 \
                --test-timeout-ms "$test_timeout_ms" \
                --max-workers "$max_workers" \
                --bail true \
                --coverage-report-directory "$coverage_report_directory" \
                --fail-with-no-tests "$@"
              perl scripts/check-coverage.pl "$coverage_report_directory"
              html_count=$(find "$coverage_report_directory" -maxdepth 1 -type f -name '*.html' | wc -l)
              if [ "$html_count" -eq 0 ] || [ ! -s "''${coverage_report_directory}cover-index.html" ]; then
                echo "Coverage report is missing or empty: ''${coverage_report_directory}cover-index.html" >&2
                exit 1
              fi
            '';
          };

          lint = pkgs.writeShellApplication {
            name = "cl-http-kit-lint";
            runtimeInputs = [
              paredit
              pkgs.coreutils
              pkgs.ripgrep
            ];
            text = ''
              mapfile -t lisp_files < <(rg --files -g '*.lisp' | sort)
              if (( ''${#lisp_files[@]} == 0 )); then
                echo "No Common Lisp files found." >&2
                exit 1
              fi
              exec paredit inspect lint \
                --output json \
                --stats \
                --fail-on error \
                --require-suppression-reason \
                --timeout-ms "''${PAREDIT_TIMEOUT_MS:-30000}" \
                "''${lisp_files[@]}"
            '';
          };
        in
        {
          default = {
            type = "app";
            program = "${test}/bin/cl-http-kit-test";
            meta = appMeta;
          };
          test-core = {
            type = "app";
            program = "${testCore}/bin/cl-http-kit-test-core";
            meta = appMeta;
          };
          test = {
            type = "app";
            program = "${test}/bin/cl-http-kit-test";
            meta = appMeta;
          };
          coverage = {
            type = "app";
            program = "${coverage}/bin/cl-http-kit-coverage";
            meta = appMeta;
          };
          lint = {
            type = "app";
            program = "${lint}/bin/cl-http-kit-lint";
            meta = appMeta;
          };
        }
      );

      checks = forEachSystem (
        system: pkgs:
        let
          clWeave = cl-weave.packages.${system}.default;
          observability = cl-observability-kit.packages.${system}.default;
          concurrent = cl-concurrent-kit.packages.${system}.default;
          boundary = cl-boundary-kit.packages.${system}.default;
          date = cl-date-kit.packages.${system}.default;
          host = cl-host-kit.packages.${system}.default;
          codec = cl-codec-kit.packages.${system}.default;
          chipz = pkgs.sbclPackages.chipz;
          paredit = paredit-cli.packages.${system}.default;
          source = pkgs.lib.cleanSource ./.;
          sourceRegistry = pkgs.lib.concatStringsSep ":" [
            "${clWeave}/share/common-lisp/source//"
            "${observability}//"
            "${concurrent}//"
            "${boundary}//"
            "${date}//"
            "${host}//"
            "${codec}//"
            "${chipz}//"
          ];
        in
        {
          test-core =
            pkgs.runCommand "cl-http-kit-test-core"
              {
                nativeBuildInputs = [
                  clWeave
                  observability
                  concurrent
                  boundary
                  date
                  host
                  codec
                ];
              }
              ''
                export HOME="$TMPDIR/home"
                export XDG_CACHE_HOME="$HOME/.cache"
                mkdir -p "$HOME"
                work="$TMPDIR/cl-http-kit"
                mkdir -p "$work"
                cp -R ${source}/. "$work/"
                cd "$work"
                export CL_SOURCE_REGISTRY="$PWD//:${sourceRegistry}"
                cl-weave run cl-http-kit/test-core \
                  --test-timeout-ms "''${CL_WEAVE_TEST_TIMEOUT_MS:-30000}" \
                  --max-workers "''${CL_WEAVE_MAX_WORKERS:-1}" \
                  --bail true \
                  --fail-with-no-tests
                touch "$out"
              '';

          test =
            pkgs.runCommand "cl-http-kit-test"
              {
                nativeBuildInputs = [
                  clWeave
                  observability
                  concurrent
                  boundary
                  date
                  host
                  chipz
                ];
              }
              ''
                export HOME="$TMPDIR/home"
                export XDG_CACHE_HOME="$HOME/.cache"
                mkdir -p "$HOME"
                work="$TMPDIR/cl-http-kit"
                mkdir -p "$work"
                cp -R ${source}/. "$work/"
                cd "$work"
                export CL_SOURCE_REGISTRY="$PWD//:${sourceRegistry}"
                cl-weave run cl-http-kit/test \
                  --test-timeout-ms "''${CL_WEAVE_TEST_TIMEOUT_MS:-30000}" \
                  --max-workers "''${CL_WEAVE_MAX_WORKERS:-1}" \
                  --bail true \
                  --fail-with-no-tests
                touch "$out"
              '';

          lint =
            pkgs.runCommand "cl-http-kit-lint"
              {
                nativeBuildInputs = [
                  paredit
                  pkgs.coreutils
                  pkgs.ripgrep
                ];
              }
              ''
                cd ${source}
                mapfile -t lisp_files < <(rg --files -g '*.lisp' | sort)
                if (( ''${#lisp_files[@]} == 0 )); then
                  echo "No Common Lisp files found." >&2
                  exit 1
                fi
                paredit inspect lint \
                  --output json \
                  --stats \
                  --fail-on error \
                  --require-suppression-reason \
                  --timeout-ms 30000 \
                  "''${lisp_files[@]}"
                touch "$out"
              '';
        }
      );
    };
}
