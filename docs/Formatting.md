# Source Formatting (`lune --fmt`)

Lune includes a deterministic source formatter, similar in spirit to `gofmt` / `elm-format`.

## CLI

Format a file in place:

```bash
lune --fmt path/to/File.lune
```

Check formatting (CI-friendly; exits non-zero if changes would be made):

```bash
lune --fmt --check path/to/File.lune
```

Print formatted output to stdout (does not modify the file):

```bash
lune --fmt --stdout path/to/File.lune
```

## Guarantees

- Deterministic and idempotent: formatting the same file twice produces identical output.
- Semantics-preserving: output parses to the same AST as input.
- Parseable output: the formatter only emits syntax accepted by the current parser.
- Canonical style only: no configuration knobs.
- Default line width: 80 columns (groups are rendered “flat”, then expanded if they overflow).

## Style Summary (v0.1)

The formatter is AST-driven and does not consult existing whitespace (beyond comment capture).

- Indentation: 2 spaces per block level.
- Trailing whitespace: removed.
- File ending: exactly one final newline.
- Top-level spacing:
  - Imports have no blank lines between them.
  - One blank line between the import section and the first non-import declaration.
  - One blank line between unrelated top-level declarations.
  - A type signature is kept immediately above its matching value definition:
    ```
    main : Task Unit Unit
    main =
      ...
    ```
- Application chains:
  - If it fits: `f a b c`
  - Otherwise, vertical application:
    ```
    f
      a
      b
      c
    ```
- Block forms:
  - `do` always renders as a block:
    ```
    x =
      do
        ...
    ```
  - `let/in` always renders as:
    ```
    let
      x = ...
    in
      ...
    ```
  - `case/of` always renders as:
    ```
    case expr of
      Pat ->
        ...
    ```
  - When `let`/`case`/`do` appear as subexpressions, they are parenthesized as a block:
    ```
    f (
        let
          x = 1
        in
          x
      )
    ```
- Lists / tuples / record literals:
  - Multi-line lists use Elm-style leading commas:
    ```
    [ a
    , b
    ]
    ```
  - Multi-line records use leading commas, and record updates use `|` on its own line.

## Comments (best-effort)

Lune v0.1 does not yet attach comments to the AST in the parser, so the formatter uses a minimal
scanner to preserve comments at top-level boundaries (module header/imports/decls).

- Line comments and nested block comments are preserved.
- Comment placement may shift to the nearest stable boundary if the original position was ambiguous.
- Block comment contents are preserved exactly (only surrounding indentation may change).

