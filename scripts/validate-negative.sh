#!/usr/bin/env bash
set -euo pipefail

if [ -z "${CABAL_CONFIG:-}" ]; then
  export CABAL_CONFIG="$PWD/.cabal-local/config"
fi

print_header() {
  echo "Running negative validation tests..."
}

print_header

failed=0

for file in examples/invalid/*.lune; do
  echo "Expecting failure: $file"
  if cabal run lune -- --validate "$file"; then
    echo "ERROR: $file unexpectedly validated!"
    failed=1
  else
    echo "OK: $file failed as expected"
  fi
done

if [ "$failed" -ne 0 ]; then
  echo "Negative validation tests FAILED."
  exit 1
fi

echo "All negative validation tests passed."
