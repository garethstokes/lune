# Lune LSP Architecture

Internal documentation for understanding and debugging the LSP implementation.

## Overview

The Lune LSP server provides:
- **Diagnostics**: Parse errors, resolve errors, type errors on `didOpen`/`didChange`
- **Formatting**: Full-document formatting via `textDocument/formatting`
- **Hover**: Type information and documentation on `textDocument/hover`

The server uses `TextDocumentSyncKind_Full` - the client sends the entire document on each change.

## Module Map

```
src/Lune/LSP/
├── Server.hs      # Entry point, capabilities, server definition
├── Handlers.hs    # Request/notification handlers (diagnostics, formatting, hover)
├── State.hs       # LspState data model, cache operations
└── Convert.hs     # Span/Range/URI conversions, UTF-16 handling
```

### Dependencies on Compiler Pipeline

```
Handlers.hs
    │
    ├── Parser.parseTextBundle          # Parse source to AST
    ├── Derive.expandDerives            # Expand derive clauses (via ModuleGraph)
    ├── ModuleGraph.loadProgramWithEntryModuleUsing  # Load imports with VFS
    ├── Resolve.resolveProgram          # Name resolution
    ├── Desugar.desugarModule           # Desugar combined module
    ├── Validate.validateModule         # Validation pass
    ├── Check.typecheckModuleEnv        # Type checking → Map Text TypeScheme
    └── Docs.extractDocTable            # Extract doc comments
```

## Data Model (LspState)

```haskell
-- src/Lune/LSP/State.hs

data LspState = LspState
  { openDocs        :: Map FilePath OpenDocInfo     -- VFS: open document contents
  , lastDiagnostics :: Map FilePath [Diagnostic]    -- Last published diagnostics
  , semanticByModule :: Map Text SemanticInfo       -- Indexed by module name
  , semanticByPath   :: Map FilePath SemanticInfo   -- Indexed by file path
  , checkVersion    :: Int                          -- Monotonic version counter
  }

data OpenDocInfo = OpenDocInfo
  { odiText         :: Text              -- Current buffer contents
  , odiModuleName   :: Maybe Text        -- Parsed module name (Nothing if parse failed)
  , odiImports      :: Set Text          -- Set of imported module names
  , odiImportAliases :: Map Text Text    -- alias -> qualified module name
  }

data SemanticInfo = SemanticInfo
  { siModuleName :: Text
  , siPath       :: FilePath
  , siVersion    :: Int                  -- checkVersion when computed (last-good)
  , siTypesAt    :: [(Span, TypeScheme)]    -- **ALWAYS EMPTY** - span-indexed types
  , siValueEnv   :: Map Text TypeScheme     -- Qualified name -> type scheme
  , siDocs       :: Map Text Text           -- Decl name -> doc comment
  , siModuleDoc  :: Maybe Text              -- Module-level doc comment
  }
```

### Data Sources

| Data | Source | Updated When |
|------|--------|--------------|
| `openDocs` | Client buffer | `didOpen`, `didChange` |
| `semanticByPath/Module` | Typecheck result | After successful `checkFileWithSemantic` |
| `siValueEnv` | `Check.typecheckModuleEnv` | Per-module, filtered by prefix |
| `siTypesAt` | **Never populated** | - |

## Diagnostics Pipeline

```
didOpen/didChange
    │
    ▼
parseModuleInfo(path, contents)
    │ Extract module name, imports, aliases
    │ Store in openDocs[path]
    ▼
publishWorkspaceDiagnostics(path)
    │
    ├── getAffectedFiles(path)  # Changed file + files that import it
    │
    └── For each affected file:
        │
        ▼
    checkFileWithSemantic(path, contents)
        │
        ├── Parse (parseModuleFromText)
        ├── Load imports (ModuleGraph with VFS loader)
        ├── Resolve (resolveProgram)
        ├── Desugar (desugarModule on combined module)
        ├── Validate (validateModule)
        ├── Typecheck (typecheckModuleEnv)
        │     └── Returns Map Text TypeScheme (qualified names)
        └── Extract docs (extractDocTable per module)
        │
        ▼
    If check succeeded:
        │ bump checkVersion
        │ stamp siVersion = checkVersion
        ▼
    Store SemanticInfo for each module in program (replaces last-good)
        │
        └── If check failed: do NOT overwrite semantic info (hover can use last-good)
        │
        ▼
    publishDiagnostics to client
```

### VFS Integration

The `mkLspModuleLoader` creates a `ModuleLoader` that:
1. First checks `openDocs` for the requested path
2. Falls back to disk read if not in VFS

```haskell
-- src/Lune/LSP/Handlers.hs:669-680
mkLspModuleLoader :: IORef LspState -> ModuleLoader
mkLspModuleLoader stateRef = ModuleLoader
  { mlReadFileText = \path -> do
      st <- readIORef stateRef
      case lookupOpenDocText path st of
        Just txt -> pure (Right txt)
        Nothing -> ... -- read from disk
  }
```

## Formatting Pipeline

```
textDocument/formatting
    │
    ▼
uriToFilePath'(uri)
    │
    ▼
Get contents:
    │
    ├── lookupOpenDocText(path) → VFS
    └── TIO.readFile(path)      → disk fallback
    │
    ▼
Fmt.formatText(path, contents)
    │
    ├── Success → TextEdit(fullDocumentRange, formatted)
    └── Failure → publishFormattingFailure (info diagnostic)
```

## Hover Pipeline

```
textDocument/hover(uri, position)
    │
    ▼
uriToFilePath'(uri)
    │
    ▼
Get document info:
    │
    ├── lookupOpenDoc(path) → (text, moduleName, importAliases)
    └── disk fallback → parseModuleInfo for metadata
    │
    ▼
wordAtPosition(docText, position)
    │ Extract token under cursor (alphanumeric + _ + .)
    │ Convert LSP UTF-16 columns → Text codepoint indices
    │ Compute hover Range via Span → Range (UTF-16-safe)
    │
    ▼
resolveHoverTarget(st, path, modName, importAliases, tok)
    │
    ├── Qualified ("Foo.bar") → lookupQualifiedSemantic
    │     └── Find SemanticInfo for module "Foo" or alias
    │
    └── Unqualified ("bar") → lookupUnqualifiedSemantic
          └── lookupSemanticInfoByPath OR lookupSemanticInfoByModule
    │
    ▼
lookupSchemeInSemantic(sem, name)
    │ Try: [moduleName + "." + name, name]
    │ Lookup in siValueEnv
    │
    ▼
Render hover:
    │
    ├── Type found → "### tok\n\n```lune\ntok : <type>\n```"
    ├── Doc found → append doc comment
    └── Neither → "### tok\n\n_Module: <name>_"
```

## Hover Failure Modes

| # | Symptom | Trigger | Root Cause | Location | How to Confirm | Fix Idea |
|---|---------|---------|------------|----------|----------------|----------|
| 1 | **No type shown for local binding** | Hover on `let x = ...` or function parameter | `siValueEnv` only contains top-level declarations with qualified names | `Handlers.hs:612` | Log `siValueEnv` keys; won't include local names | Implement span-indexed types in `siTypesAt` |
| 2 | **Type temporarily stale after edit** | Edit function body, hover immediately | Hover uses last-good semantic info until the next successful check finishes | `Handlers.hs:handleHover` | Enable `LUNE_LSP_DEBUG`, observe `semanticVersion` lagging `checkVersion` | Wait for diagnostics to refresh, then re-hover |
| 3 | **UTF-16 position mismatch** | Hover on line with emoji/CJK before cursor | (Fixed) `wordAtPosition` now converts UTF-16 → codepoints before indexing | `Convert.hs:utf16ColToCodepoints` + `Handlers.hs:wordAtPosition` | Run `HoverUtf16Unicode` LSP test | - |
| 4 | **Qualified name not found** | Hover on `Module.foo` where alias differs | `lookupQualifiedSemantic` checks module name OR alias, but order matters | `Handlers.hs:412-417` | Hover on aliased import like `import Foo as F`, then `F.bar` | Ensure both lookups succeed |
| 5 | **Type missing for derived code** | Hover on field accessor from `deriving (Table)` | Derive expansion happens before resolve; generated names may not match source positions | `ModuleGraph.hs:82-85` | Hover on derived accessor, check if found | Map derived spans back to source |
| 6 | **Type missing for constructors** | Hover on `Just` or custom ADT constructor | `siValueEnv` from `typecheckModuleEnv` may not include constructors in expected format | `Check.hs` | Log valueEnv keys for module with ADT | Ensure constructors are in valueEnv with correct qualified names |
| 7 | **Hover fails on syntax error** | Parse error anywhere in file | (Mostly fixed) hover keeps using last-good semantic info; only fails if the file has never typechecked successfully | `Handlers.hs:publishWorkspaceDiagnostics` + `Handlers.hs:handleHover` | Introduce syntax error, hover a previously-checked top-level | Ensure file checks successfully at least once |
| 8 | **Hover on imported but not typechecked module** | Open file A, hover on B.foo without opening B | SemanticInfo for B may not be cached if B was loaded from disk during A's check | `Handlers.hs:606-618` | Check if B's info is in `semanticByModule` | Ensure all program modules get SemanticInfo |

### Critical Issue: `siTypesAt` is Never Populated

```haskell
-- src/Lune/LSP/Handlers.hs:611
siTypesAt = []  -- ALWAYS EMPTY
```

The design includes `siTypesAt :: [(Span, TypeScheme)]` for span-indexed type lookups, but it's never populated. This means:
- Local bindings (`let`, `case` patterns, lambda params) cannot have types displayed
- Only top-level declarations in `siValueEnv` can be looked up
- Type information is lost for sub-expressions

### UTF-16 Position Handling

The codebase now does UTF-16 ↔︎ codepoint conversion in both directions:
- `spanToRange` converts codepoint `Span` columns into UTF-16 `Position.character` for LSP.
- `wordAtPosition` converts LSP UTF-16 `Position.character` into a codepoint index before using `Text` operations.

## Recommended Fixes

### 1. Populate `siTypesAt` with Span-Indexed Types (High Impact, High Effort)

**Problem**: Local bindings have no type information.

**Solution**: Modify the type checker to emit `(Span, TypeScheme)` pairs for all typed AST nodes.

```haskell
-- In Check.hs, during inference:
-- Accumulate (span, inferredType) pairs in a Writer monad or similar

-- Then in Handlers.hs:
siTypesAt = collectTypedSpans typedAST
```

**Lookup change**:
```haskell
handleHover = do
  -- First try span-indexed lookup
  case findSpanContaining pos (siTypesAt sem) of
    Just scheme -> renderType scheme
    Nothing -> -- fall back to siValueEnv lookup
```

### 2. Fix UTF-16 Position Handling (Implemented)

`Convert.utf16ColToCodepoints` walks a line counting UTF-16 units (BMP=1, non-BMP=2) and clamps safely. `Handlers.wordAtPosition` uses it before token extraction, and uses `spanToRange` to return UTF-16-correct hover ranges.

### 3. Cache Last-Good Semantic Info (Implemented)

The LSP server no longer clears `semanticByPath`/`semanticByModule` on `didChange`. Semantics are only replaced on successful checks; failures publish diagnostics but keep the last-good semantic info for hover. `checkVersion` increments only when semantics are stored, and each `SemanticInfo` is stamped with `siVersion`.

## Known Limitations / TODOs

1. **No incremental checking**: Full reparse/recheck on every change
2. **No debouncing**: Each keystroke triggers full check
3. **No cancellation**: Long checks block subsequent requests
4. **No progress reporting**: User gets no feedback during long checks
5. **No go-to-definition**: Only hover is implemented
6. **No completion**: No autocomplete support
7. **TextDocumentSyncKind_Full**: Inefficient for large files
8. **Single-threaded**: All requests processed sequentially

## Debugging

### Enable Debug Logging

For LSP tests:
```bash
LUNE_LSP_TEST_DEBUG=1 cabal test
```

### Hover Debug Logging

Set `LUNE_LSP_DEBUG=1` when launching the server to log hover requests to stderr (uri, pos, extracted token, whether semantic info was found, `semanticVersion`, and current `checkVersion`).

### Manual Reproduction

1. Start LSP server: `cabal run lune -- lsp --stdio`
2. Use an LSP client (VS Code with generic LSP extension, or `lsp-devtools`)
3. Open a file, observe diagnostics
4. Hover on various tokens, check stderr for debug output

### Test for UTF-16 Bug

Run the regression test `HoverUtf16Unicode` (fixture: `tests/lsp/test_unicode.lune`).
