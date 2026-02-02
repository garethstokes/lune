#!/usr/bin/env bash
set -euo pipefail

if [ -z "${CABAL_CONFIG:-}" ]; then
  export CABAL_CONFIG="$PWD/.cabal-local/config"
fi

echo "Running typeclass negative tests..."

failed=0

expect_fail() {
  local file="$1"
  echo "Expecting failure: $file"
  if cabal run lune -- --typecheck "$file"; then
    echo "ERROR: $file unexpectedly typechecked!"
    failed=1
  else
    echo "OK: $file failed as expected"
  fi
}

expect_fail examples/09_Typeclasses_Modules/C.MissingInstance.lune
expect_fail examples/09_Typeclasses_Modules/C.OrphanMain.lune
expect_fail examples/09_Typeclasses_Modules/C.OverlapMain.lune

if [ "$failed" -ne 0 ]; then
  echo "Typeclass negative tests FAILED."
  exit 1
fi

echo "All typeclass negative tests passed."

