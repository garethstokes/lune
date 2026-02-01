#!/usr/bin/env bash
set -euo pipefail

if [ -z "${CABAL_CONFIG:-}" ]; then
  export CABAL_CONFIG="$PWD/.cabal-local/config"
fi

echo "Running negative typecheck tests..."

failed=0

files=(
  "examples/invalid/05_BadMonadArgKind.lune"
  "examples/invalid/06_OverlappingInstance.lune"
  "examples/invalid/07_OrphanInstance.lune"
)

for file in "${files[@]}"; do
  echo "Expecting typecheck failure: $file"
  if cabal run lune -- --typecheck "$file"; then
    echo "ERROR: $file unexpectedly typechecked!"
    failed=1
  else
    echo "OK: $file failed as expected"
  fi
done

if [ "$failed" -ne 0 ]; then
  echo "Negative typecheck tests FAILED."
  exit 1
fi

echo "All negative typecheck tests passed."
