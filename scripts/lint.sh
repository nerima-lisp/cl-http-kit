#!/usr/bin/env bash
set -euo pipefail

mapfile -t lisp_files < <(rg --files -g '*.lisp' | sort)
if (( ${#lisp_files[@]} == 0 )); then
  echo "No Common Lisp files found." >&2
  exit 1
fi
printf 'Linting %d Common Lisp files.\n' "${#lisp_files[@]}" >&2

lint_report=$(mktemp)
trap 'rm -f "$lint_report"' EXIT
lint_status=0
paredit inspect lint \
  --output json \
  --fail-on error \
  --require-suppression-reason \
  --timeout-ms "${PAREDIT_TIMEOUT_MS:-30000}" \
  "${lisp_files[@]}" >"$lint_report" || lint_status=$?
cat "$lint_report"
if (( lint_status != 0 )); then
  exit "$lint_status"
fi
if ! jq -e '.partial_failures | type == "array" and length == 0' "$lint_report" >/dev/null; then
  echo "Lint did not complete for every input file." >&2
  exit 1
fi
