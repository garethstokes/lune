#!/usr/bin/env bash
set -euo pipefail

if [ -z "${CABAL_CONFIG:-}" ]; then
  export CABAL_CONFIG="$PWD/.cabal-local/config"
fi

echo "Running Prelude smoke tests..."

required_files=(
  "prelude/Lune/Prelude.lune"
  "prelude/Lune/Int.lune"
  "prelude/Lune/Bool.lune"
  "prelude/Lune/String.lune"
  "prelude/Lune/IO.lune"
  "prelude/Lune/Result.lune"
  "prelude/Lune/Maybe.lune"
  "prelude/Lune/List.lune"
  "prelude/Lune/Atomic.lune"
  "prelude/Lune/Task.lune"
)

for file in "${required_files[@]}"; do
  if [ ! -f "$file" ]; then
    echo "ERROR: missing required prelude module: $file"
    exit 1
  fi
done

extract_module_header() {
  awk '
    BEGIN { inHeader=0; seenExposing=0; depth=0 }
    /^module / { inHeader=1 }
    inHeader {
      print

      if ($0 ~ /exposing[[:space:]]*[(]/) {
        seenExposing=1
      }

      if (seenExposing) {
        opens = gsub(/[(]/, "&")
        closes = gsub(/[)]/, "&")
        depth += (opens - closes)
        if (depth == 0) {
          exit
        }
      }
    }
  ' "$1"
}

extract_exports() {
  local header
  header="$(extract_module_header "$1" | tr '\n' ' ')"

  local body
  body="$(echo "$header" | sed -E 's/^.*exposing [(]//; s/[)][[:space:]]*$//')"

  IFS=',' read -ra raw <<< "$body"
  for item in "${raw[@]}"; do
    item="$(echo "$item" | sed -E 's/^[[:space:]]+|[[:space:]]+$//g')"
    if [ -n "$item" ]; then
      echo "$item"
    fi
  done
}

contains() {
  local needle="$1"
  shift
  for x in "$@"; do
    if [ "$x" = "$needle" ]; then
      return 0
    fi
  done
  return 1
}

check_no_banned_exports() {
  local file="$1"
  shift

  local item
  for item in "$@"; do
    if [[ "$item" == prim_* ]]; then
      echo "ERROR: $file exports backend hook: $item"
      exit 1
    fi
    if [[ "$item" == extern ]]; then
      echo "ERROR: $file exports internal name: extern"
      exit 1
    fi
    if [[ "$item" == runMain ]]; then
      echo "ERROR: $file exports internal name: runMain"
      exit 1
    fi
    if [[ "$item" == '$dict'* || "$item" == '$prim'* ]]; then
      echo "ERROR: $file exports internal name: $item"
      exit 1
    fi
  done
}

check_exact_exports() {
  local file="$1"
  local expected_str="$2"

  mapfile -t actual < <(extract_exports "$file")
  read -ra expected <<< "$expected_str"

  check_no_banned_exports "$file" "${actual[@]}"

  local item
  for item in "${expected[@]}"; do
    if ! contains "$item" "${actual[@]}"; then
      echo "ERROR: $file missing export: $item"
      exit 1
    fi
  done

  for item in "${actual[@]}"; do
    if ! contains "$item" "${expected[@]}"; then
      echo "ERROR: $file exports unexpected name: $item"
      exit 1
    fi
  done
}

check_exact_exports "prelude/Lune/Prelude.lune" "Unit unit Bool(..) Int Char String List(..) Maybe(..) Result(..) IO Atomic Shared Task Functor(..) Applicative(..) Monad(..)"
check_exact_exports "prelude/Lune/Int.lune" "add sub mul eq gte lte"
check_exact_exports "prelude/Lune/Bool.lune" "and or not"
check_exact_exports "prelude/Lune/String.lune" "append fromInt toInt"
check_exact_exports "prelude/Lune/IO.lune" "println readLine readInt sleepMs readFile writeFile Error"
check_exact_exports "prelude/Lune/Result.lune" "map mapError andThen withDefault"
check_exact_exports "prelude/Lune/Maybe.lune" "map andThen withDefault"
check_exact_exports "prelude/Lune/List.lune" "map foldl"
check_exact_exports "prelude/Lune/Atomic.lune" "Atomic Shared commit new read write wait orElse"
check_exact_exports "prelude/Lune/Task.lune" "Task start await yield"

for file in "${required_files[@]}"; do
  echo "Typechecking: $file"
  cabal run lune -- --typecheck "$file" >/dev/null
done

echo "Prelude smoke tests passed."
