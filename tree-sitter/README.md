# tree-sitter-lune

Tree-sitter grammar for the Lune programming language.

## Features

- Full support for module declarations and imports
- Type declarations (ADTs, type aliases, newtypes)
- Class and instance declarations with constraints
- Function declarations and type signatures
- Expressions: let, case, if-then-else, lambdas, do-notation
- Patterns: constructor, literal, list, tuple, record, wildcard
- Literals: integers (dec/hex/octal/binary), floats, chars, strings with interpolation
- Multiline template strings
- Operators and operator sections
- Record types with field annotations (@derive, @primaryKey, etc.)
- Foreign import declarations
- Comments (line and nested block)

## Building

```bash
tree-sitter generate
tree-sitter test
```

## Usage with Editors

### Neovim

Copy or symlink the `queries/` directory to your nvim-treesitter queries path.

### Helix

Add to your `languages.toml`:
```toml
[[language]]
name = "lune"
scope = "source.lune"
file-types = ["lune"]
roots = ["lune.cabal", "cabal.project"]
```

## Known Limitations

This grammar does **not** implement layout-sensitive parsing. Lune, like Haskell and Elm, uses indentation to determine where declarations and case alternatives begin and end. Without an external scanner to track indentation, the grammar has these limitations:

### 1. Multi-line case expressions

Case expressions with multiple alternatives on separate lines will not parse correctly:

```lune
-- Will NOT parse correctly:
f x = case x of
  Just y -> y
  Nothing -> 0

-- Workaround: use parentheses or single-line alternatives
f x = case x of Just y -> y  -- single alternative works
```

### 2. Type signatures followed by function declarations

Type signatures may consume tokens from the following function declaration:

```lune
-- May have issues:
main : Task Unit Unit
main = ...

-- Workaround: combine on single line where possible
```

### 3. Multi-argument function application at declaration level

Function application with multiple bare identifiers may not parse all arguments:

```lune
-- May only parse two of three arguments:
f = foo bar baz

-- Workaround: use parentheses
f = (foo bar baz)
```

### Future Work

Proper layout support requires implementing an external scanner in C that:
1. Tracks indentation levels on a stack
2. Emits virtual `LAYOUT_SEMICOLON` tokens between declarations at the same level
3. Emits virtual `LAYOUT_END` tokens when dedenting

See [tree-sitter-haskell](https://github.com/tree-sitter/tree-sitter-haskell) for a reference implementation.

## Query Files

- `queries/highlights.scm` - Syntax highlighting queries

## License

MIT
