# Comment Preservation: AST-Attached Formatter Migration — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the formatter's text-based comment scanner with AST-attached comment rendering (leading/trailing/inner), driven by a post-parse attachment pass.

**Architecture:** Parser captures every comment into a flat list with spans → a new pure pass `attachComments :: Module -> [Comment] -> Module` assigns each comment to a node slot (`Leading`/`Trailing`/`Inner` on `Located` nodes; top-level standalone comments go in a new `Module.modComments` field) → `Format.hs` renders comments from this attached data and the old scanner in `Fmt.hs` is deleted. Big-bang cutover on branch `feat/fmt-comment-ast-migration`.

**Tech Stack:** Haskell, megaparsec (parser), `Lune.Fmt.Doc` pretty-printer, tasty / tasty-golden / tasty-hunit (tests), Nix flake dev shell.

**Reference:** Design spec at `docs/superpowers/specs/2026-06-07-comment-preservation-fmt-migration-design.md`.

---

## Conventions for this plan

- All `cabal` commands run **inside the Nix dev shell**. Either prefix with `nix develop --command ` or run them from a shell already inside `nix develop`. The plan writes the bare `cabal …` form; prefix as needed.
- The golden test runner (`tests/Main.hs:140`) invokes `cabal run -v0 lune -- <flags> <file>`, so once Task 0 makes `cabal` work in the dev shell, the runner works unchanged.
- Run a single fmt golden with: `cabal test golden --test-options="-p '/Fmt/'"`. Accept new golden output with `--test-options="--accept"` (only after visually confirming the diff is correct).
- Commit after every green step. Keep the branch green at each Task boundary even though the project is a big-bang cutover internally.

---

## File structure

| File | Responsibility | Change |
|------|----------------|--------|
| `src/Lune/Syntax.hs` | Surface AST | Add `modComments :: [Comment]` to `Module` |
| `src/Lune/Parser.hs` | Parse → AST | Flat comment capture; return `(Module, [Comment])`; stop per-node leading attachment; delete `attachTrailing` stub |
| `src/Lune/Syntax/Comments/Attach.hs` | **NEW** — comment attachment heuristics | `attachComments :: Module -> [Comment] -> Module` |
| `src/Lune/Fmt.hs` | Fmt entry point | Delete text-scanner (`formatModuleTextWithComments`, `computeDoLayouts`, `scanDoLayouts`, `extractTrivia`, `BlankLineMap`); rewire `formatText` to parse → attach → render |
| `src/Lune/Fmt/Format.hs` | AST → Doc | Render `locComments` per node + `modComments` at top level; blank lines from spans |
| `lune.cabal` | Build/test config | Expose new `Attach` module; add `tasty-hunit` dep already present in test-suite |
| `tests/Main.hs` | Test harness | Add `Attach` unit-test group |
| `tests/fmt/*.lune` | Golden fixtures | Rewrite comment goldens; add new fixtures |
| `docs/Formatting.md` | User docs | Replace stale "Comments (best-effort)" section |

---

## Task 0: Make `cabal test golden` run in the Nix dev shell

**Why:** The RED/GREEN loop is impossible until the test suite runs. Currently `nix develop --command cabal build` fails with `unknown package: parser-combinators` because the dev-shell cabal has no package index / is not pointed at the Nix-provided package set.

**Files:**
- Investigate: `flake.nix`, `cabal.project`
- Possibly modify: `cabal.project` (add `with-compiler` / index settings) or document a `cabal update` step

- [ ] **Step 1: Reproduce the failure**

Run: `nix develop --command bash -c 'cabal build lune 2>&1 | tail -20'`
Expected: `unknown package: parser-combinators` (or similar dependency-resolution failure).

- [ ] **Step 2: Determine the cause**

Run: `nix develop --command bash -c 'ghc-pkg list 2>/dev/null | grep -i parser-combinators; echo ---; cabal --version'`
- If `parser-combinators` **is** in the GHC package db, the dev-shell `ghc`/package set already provides it and cabal just needs `cabal update` to build a usable index, OR a `cabal.project` that tells cabal to use the global package db (e.g. `package *` / `--package-db`).
- If it is **not** listed, the Nix `mkHaskellShell` package set is missing it; fix by adding it to the dev-shell inputs in `flake.nix` (the `haskell.flake` `additionalPackages`/Haskell deps).

- [ ] **Step 3: Apply the smallest fix that makes resolution succeed**

Try in order, stopping at the first that works:
1. `nix develop --command bash -c 'cabal update && cabal build lune 2>&1 | tail -5'`
2. If still failing, add a `cabal.project` stanza pinning the dev-shell GHC and disabling the solver's hackage preference, then rebuild.
3. If still failing, add the missing package to `flake.nix` dev-shell Haskell deps and `nix develop` again.

- [ ] **Step 4: Verify the full golden suite runs (baseline, all green)**

Run: `nix develop --command cabal test golden 2>&1 | tail -30`
Expected: the suite executes (Parse / Eval / Core / Neg / Fmt / Fmt Idempotent / Fmt Check / Process groups) and reports **all PASS** on the unmodified branch. Record the pass count as the baseline.

- [ ] **Step 5: Commit**

```bash
git add flake.nix cabal.project 2>/dev/null; git commit -m "build: make cabal test golden runnable in the nix dev shell"
```
(If no files changed — i.e. only `cabal update` was needed — record the required command in `README.md` Tests section instead and commit that.)

---

## Task 1: Add `modComments` field + flat comment capture (scanner still active)

This task adds the data plumbing **without** touching the formatter, so every existing golden stays green (the scanner is still doing the rendering). It is a safe checkpoint.

**Files:**
- Modify: `src/Lune/Syntax.hs` (Module data type)
- Modify: `src/Lune/Parser.hs` (capture + return comments; populate `modComments` with `[]` for now)
- Test: existing `cabal test golden` must stay green

- [ ] **Step 1: Add the field to `Module`**

In `src/Lune/Syntax.hs`, change:

```haskell
data Module = Module
  { modName :: Text
  , modExports :: [Expose]
  , modImports :: [Import]
  , modDecls :: [Decl]
  }
  deriving (Show)
```

to:

```haskell
data Module = Module
  { modName :: Text
  , modExports :: [Expose]
  , modImports :: [Import]
  , modDecls :: [Decl]
  , modComments :: [Comment]   -- ^ Top-level standalone comments, attached by Lune.Syntax.Comments.Attach
  }
  deriving (Show)
```

`Comment` is already re-exported from `Lune.Syntax` (via `module Lune.Syntax.Comment`).

- [ ] **Step 2: Update the single `Module {…}` constructor**

In `src/Lune/Parser.hs` `parseModule`, change the record literal to include `modComments = []`:

```haskell
parseModule :: Parser Module
parseModule = do
  (name, exports) <- moduleHeader
  imports <- many (try importDecl)
  decls <- many decl
  scnOptional
  pure Module {modName = name, modExports = exports, modImports = imports, modDecls = decls, modComments = []}
```

All other `Module` references are record **updates** (`m { modDecls = … }`) and need no change.

- [ ] **Step 3: Expose the captured comment list from the parser**

Add a new exported parse entry that returns both the module and the flat comment list. In `src/Lune/Parser.hs`, add to the export list `parseTextWithComments` and define:

```haskell
-- | Parse, returning the module together with every comment collected during
-- the parse (flat, in source order, with spans). Comments are NOT yet attached.
parseTextWithComments
  :: FilePath -> Text -> Either (ParseErrorBundle Text Void) (Module, [Comment])
parseTextWithComments path contents =
  runParser
    (runStateT (scnOptional *> parseModule <* eof) initialParserState)
    path contents
    & fmap (\(m, st) -> (m, psPendingComments st))
  where (&) = flip ($)
```

(Use `runStateT` instead of `evalStateT` so the final `ParserState` — which accumulates every comment in `psPendingComments` — is recoverable. If `&` is undesirable, write it with a `case`.)

- [ ] **Step 4: Stop per-node leading attachment so comments survive in the flat list**

In `src/Lune/Parser.hs` `located` (around line 67), remove the `consumePendingComments` drain so comments are NOT consumed per node (they must remain in `psPendingComments` for Step 3 to return them). Change `located` to attach `emptyComments`:

```haskell
located :: Parser a -> Parser (Located a)
located p = do
  start <- getSourcePos
  result <- p
  end <- getSourcePos
  let span = Span (unPos' (sourceLine start)) (unPos' (sourceColumn start))
                  (unPos' (sourceLine end)) (unPos' (sourceColumn end))
  pure (Located span emptyComments result)
```

(Match the existing span-construction helpers in the file — reuse whatever `unPos`/offset helper `located` currently uses; only the comment handling changes from `Comments leading [] []` to `emptyComments`.) Delete the now-unused `consumePendingComments`. Delete the `attachTrailing` and `collectTrailingComments` stubs (lines ~90–102) — attachment now lives in the new pass.

- [ ] **Step 5: Build and run the full golden suite**

Run: `cabal build lune && cabal test golden 2>&1 | tail -20`
Expected: **all green**, same baseline as Task 0 Step 4. The scanner in `Fmt.hs` still renders comments, so fmt goldens are unaffected; `modComments` is `[]` and unused so far.

- [ ] **Step 6: Commit**

```bash
git add src/Lune/Syntax.hs src/Lune/Parser.hs
git commit -m "feat(syntax): add Module.modComments + flat comment capture (parseTextWithComments)"
```

---

## Task 2: Attachment pass with unit tests (pure, not yet wired to formatter)

Build `attachComments` heuristic-by-heuristic with direct AST assertions. The formatter is untouched; goldens stay green throughout.

**Files:**
- Create: `src/Lune/Syntax/Comments/Attach.hs`
- Modify: `lune.cabal` (add `Lune.Syntax.Comments.Attach` to `exposed-modules`)
- Modify: `tests/Main.hs` (add an `Attach` HUnit test group)
- Test: `tests/Main.hs`

The attachment contract (from the spec):
- **Trailing**: comment starts on the same line as a `Located` node's end, after it → that node's `Trailing` slot. Descends to the *innermost* `Located` node ending on that line.
- **Leading**: comment on its own line(s) immediately above a `Located` node → that node's `Leading` slot.
- **Inner**: comment between children of a compound (`DoBlock`, list, record, `Case` alts) → container node's `Inner` slot, tagged with the preceding-child index.
- **Top-level standalone**: leading/between-decl comments with no enclosing `Located` owner → `Module.modComments`.
- **No silent drops**: a comment with no structural home attaches as `Trailing` on the last declaration's outermost `Located` node (or `modComments` if none).

- [ ] **Step 1: Create the module skeleton with the signature only**

Create `src/Lune/Syntax/Comments/Attach.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Lune.Syntax.Comments.Attach
  ( attachComments
  ) where

import Lune.Syntax

-- | Attach each comment to exactly one AST node slot (Leading/Trailing/Inner)
-- or to Module.modComments. Pure; total; never drops a comment.
attachComments :: Module -> [Comment] -> Module
attachComments m _comments = m   -- placeholder; replaced test-by-test below
```

Add `Lune.Syntax.Comments.Attach` to `exposed-modules` in `lune.cabal`. Add `lune` to the test-suite `build-depends` in `lune.cabal` (so tests can import the library) if not already present, and confirm `tasty-hunit` is listed (it is).

- [ ] **Step 2: RED — trailing same-line heuristic test**

In `tests/Main.hs`, add an import `import qualified Lune.Syntax.Comments.Attach as Attach` and `import qualified Lune.Parser as Parser`, and a new test group wired into the top-level `testGroup` list:

```haskell
attachTests :: TestTree
attachTests = testGroup "Attach"
  [ testCase "trailing comment attaches to RHS expr on same line" $ do
      let src = "module M exposing (x)\n\nx = 1  -- note\n"
      case Parser.parseTextWithComments "M" src of
        Left e -> assertFailure (show e)
        Right (m, cs) -> do
          let m' = Attach.attachComments m cs
          -- The DeclValue RHS (Located Expr) must carry the trailing comment.
          assertBool "trailing comment present on a node" (moduleHasTrailing "note" m')
          assertEqual "no leftover top-level comments" [] (map commentText (modComments m'))
  ]

-- Helper: does any Located node in the module carry a trailing comment whose
-- text contains the needle?
moduleHasTrailing :: Text -> Module -> Bool
moduleHasTrailing needle m = any declHasTrailing (modDecls m)
  where
    declHasTrailing (DeclValue _ _ rhs) = locHasTrailing rhs
    declHasTrailing _ = False
    locHasTrailing l =
      any (needleIn . commentText) (commentsTrailing (locComments l))
    needleIn t = needle `T.isInfixOf` t
```

(Add `import qualified Data.Text as T` and `import Data.Text (Text)` to `tests/Main.hs` if absent.)

Run: `cabal test golden --test-options="-p '/Attach/'"`
Expected: **FAIL** — `attachComments` is a placeholder, so no trailing comment is attached.

- [ ] **Step 3: GREEN — implement trailing attachment**

In `Attach.hs`, implement the trailing rule: for each comment, find the innermost `Located` node whose `spanEndLine == commentLine` and `spanEndCol <= commentCol`, and add the comment (with `commentPosition = Trailing`) to that node's `commentsTrailing`. Comments not matched fall through to later rules. Walk `modDecls`, descending into the `Located` positions (`DeclValue` patterns + RHS; `instanceMethodExpr`; etc.). Implement a generic "annotate every Located node" traversal helper so the same machinery serves later rules.

Run: `cabal test golden --test-options="-p '/Attach/'"`
Expected: **PASS**.

- [ ] **Step 4: Commit**

```bash
git add src/Lune/Syntax/Comments/Attach.hs lune.cabal tests/Main.hs
git commit -m "feat(fmt): attach trailing same-line comments to AST nodes"
```

- [ ] **Step 5: RED — leading own-line heuristic test**

Add to `attachTests`:

```haskell
  , testCase "leading own-line comment attaches to the following node" $ do
      let src = "module M exposing (x)\n\nx =\n  -- leading\n  1\n"
      case Parser.parseTextWithComments "M" src of
        Left e -> assertFailure (show e)
        Right (m, cs) -> do
          let m' = Attach.attachComments m cs
          assertBool "leading comment on the RHS literal" (moduleHasLeading "leading" m')
```

with helper `moduleHasLeading` mirroring `moduleHasTrailing` but reading `commentsLeading` (descend into the RHS expression's inner `Located` nodes).

Run: `cabal test golden --test-options="-p '/Attach/'"`
Expected: **FAIL** (only trailing implemented).

- [ ] **Step 6: GREEN — implement leading attachment**

In `Attach.hs`, after trailing matching, for each remaining comment find the innermost `Located` node whose `spanStartLine > commentEndLine` and is the nearest following node (smallest positive line gap), add as `Leading`. 

Run: `cabal test golden --test-options="-p '/Attach/'"` → **PASS**. Re-run the trailing test too (`-p '/Attach/'` runs both).

- [ ] **Step 7: Commit** — `git commit -am "feat(fmt): attach leading own-line comments to following node"`

- [ ] **Step 8: RED — inner-in-do test**

Add a test: a comment on its own line between two do-statements must land in the `DoBlock`'s container `Inner` slot tagged with the preceding statement index. Assert via a helper `doInnerComments :: Module -> [(Int, Text)]` that finds the `DoBlock` and returns `(precedingChildIndex, commentText)` pairs from the container node's `commentsInner`.

```haskell
  , testCase "standalone comment between do-stmts is inner on the block" $ do
      let src = "module M exposing (main)\n\nmain =\n  do\n    a\n    -- between\n    b\n"
      case Parser.parseTextWithComments "M" src of
        Left e -> assertFailure (show e)
        Right (m, cs) -> do
          let m' = Attach.attachComments m cs
          assertEqual "inner comment after stmt index 0"
            [(0, "-- between")] (doInnerComments m')
```

Run `-p '/Attach/'` → **FAIL**.

- [ ] **Step 9: GREEN — implement inner attachment for `DoBlock`, list, record, `Case`**

For each remaining comment inside a compound's span, find the container `Located` node and the index of the child it follows (largest child whose end precedes the comment), store as `Inner` with that index encoded in `commentCol`-independent metadata. **Index encoding:** reuse `Comment`'s existing fields — store the child index in a wrapper list ordered so the renderer reads `commentsInner` in source order and re-derives slotting from each comment's line vs. child spans. (Simplest robust approach: keep `commentsInner` sorted by `commentLine`; the renderer compares each inner comment's line against child spans to place it. This avoids adding an index field.)

Run `-p '/Attach/'` → **PASS** (adjust `doInnerComments` helper if you choose line-based slotting: assert the inner comment text is present and its line falls between stmt `a` and `b`).

- [ ] **Step 10: Commit** — `git commit -am "feat(fmt): attach inner comments to compound containers"`

- [ ] **Step 11: RED — top-level standalone + EOF-leftover tests**

Add two tests: (a) a comment between two top-level declarations lands in `modComments`; (b) a trailing comment after the last declaration with nothing after it is not dropped (lands in `modComments` or the last decl's trailing). Assert `modComments` / no-drop via `assertBool "comment preserved somewhere" (commentPreserved "..." m')` where `commentPreserved` checks all slots + `modComments`.

Run `-p '/Attach/'` → **FAIL**.

- [ ] **Step 12: GREEN — implement top-level placement + leftover rule**

Remaining comments not inside any decl span → `modComments` (sorted by `commentLine`). Any still-unmatched comment → `modComments` as the final catch-all so nothing is dropped. Add an internal `assert`/property: `length (allAttachedComments m') == length cs`.

Run `-p '/Attach/'` → **PASS**.

- [ ] **Step 13: Refactor + commit**

Clean up the traversal (DRY the "map over every Located node" helper). Keep `-p '/Attach/'` green.
`git commit -am "feat(fmt): top-level comment placement + no-drop leftover rule"`

---

## Task 3: AST-driven rendering + scanner deletion (the cutover)

This is the big-bang step. The formatter switches from the scanner to `locComments`/`modComments`, and the fmt goldens are rewritten. Do it fixture-class by fixture-class, keeping non-comment goldens green.

**Files:**
- Modify: `src/Lune/Fmt.hs` (rewire `formatText`; delete scanner)
- Modify: `src/Lune/Fmt/Format.hs` (render comments; blank-from-spans)
- Modify/Create: `tests/fmt/*.lune`
- Test: `cabal test golden --test-options="-p '/Fmt/'"`

- [ ] **Step 1: Rewire `formatText` to parse → attach → render (scanner still present but bypassed)**

In `src/Lune/Fmt.hs`, change `formatText` to:

```haskell
formatText :: FilePath -> Text -> Either FmtError Text
formatText path src =
  case Parser.parseTextWithComments path src of
    Left err -> Left (FmtParseError (T.pack (errorBundlePretty err)))
    Right (m, comments) ->
      let m' = Attach.attachComments m comments
       in Right (ensureSingleTrailingNewline (D.render 80 (Fmt.formatModuleDocWithAttached m')))
```

Add `import qualified Lune.Syntax.Comments.Attach as Attach`. Create `formatModuleDocWithAttached :: S.Module -> D.Doc` in `Format.hs` as the new entry that renders from attached comments (initially it can delegate to the existing `formatModuleDoc` ignoring comments — that makes comments DISAPPEAR, which the next steps fix fixture-by-fixture).

- [ ] **Step 2: RED — run fmt goldens, expect comment fixtures to fail**

Run: `cabal test golden --test-options="-p '/Fmt/'"`
Expected: non-comment fixtures (`lists`, `records`, `layout`, `type_app_parens`, `module_exposing`, `pipe_do_bind`) **PASS**; comment fixtures (`comments`, `do_comment`, `do_blank_line`, `do_blank_before_bind`) **FAIL** because the new path drops comments/blanks. This confirms the cutover is active.

- [ ] **Step 3: GREEN — render leading + trailing comments on nodes**

In `Format.hs`, wrap the per-node Doc emission so that for any `Located` node being formatted it emits: each `commentsLeading` on its own line (preserving `--`/`{- -}`/`{-| -}` text verbatim) → the node Doc → a single `commentsTrailing` appended on the same line with two leading spaces. Implement via the existing `formatLExprM`/`formatLPattern` wrappers (which currently discard comments — `Format.hs:444-455`).

Update the `comments.expected.lune` and `do_comment.expected.lune` fixtures to the new **inline-trailing** behavior. For `comments`, the trailing comment now stays inline:

```
{-| Leading doc comment.
Multiline.
-}
module FmtComments exposing (main)

import A
-- between imports
import B

-- between imports and decls
x = 1  -- trailing comment
{- block between decls -}
y = 2

main = x
```

Run `-p '/Fmt/'`; iterate on the renderer until `comments` and `do_comment` PASS. Use `--test-options="--accept"` ONLY after eyeballing each diff.

- [ ] **Step 4: GREEN — render inner comments in compounds**

Extend the do-block / list / record / case renderers to consult the container node's `commentsInner` and emit each at its source-line slot among the children. Verify `do_comment` (has `-- between blocks`) stays correct and passes.

Run `-p '/Fmt/'` → comment fixtures PASS.

- [ ] **Step 5: GREEN — blank lines from spans**

Replace any remaining dependence on the scanner's blank-line map. In `Format.hs`, when laying out consecutive siblings (top-level decls, do-stmts, let-binds), emit one blank line when `next.startLine - prev.endLine > 1`, counting an interleaved comment's span in the gap so blanks around comments aren't doubled. Confirm `do_blank_line` and `do_blank_before_bind` PASS unchanged.

Run `-p '/Fmt/'` → **all Fmt fixtures PASS**.

- [ ] **Step 6: Render top-level `modComments`**

In `formatModuleDocWithAttached`, interleave `modComments` with imports/decls by `commentLine` so between-import and between-decl standalone comments (e.g. `-- between imports`, `-- between imports and decls`, `{- block between decls -}`) render at the right spot. Confirm `comments` fixture still passes.

Run `-p '/Fmt/'` → **PASS**.

- [ ] **Step 7: Delete the scanner**

In `src/Lune/Fmt.hs`, delete `formatModuleTextWithComments`, `computeDoLayouts`, `scanDoLayouts`, `extractTrivia`, `StandaloneCommentInfo`, `Trivia`, `BlankLineMap`, the local `Comment`/`Pos`/`TopItem` types, and any now-unused helpers. In `Format.hs`, delete `formatModuleDocWithComments` and `DoLayout` plumbing if no longer referenced. Keep `formatModuleDoc`/`formatModuleText` only if still used elsewhere (check with grep); otherwise remove.

Run: `cabal build lune 2>&1 | tail -20`
Expected: compiles with **no unused-binding warnings** for the deleted area (fix any dangling references).

- [ ] **Step 8: Full suite green**

Run: `cabal test golden 2>&1 | tail -25`
Expected: **all groups PASS** — Parse, Eval, Core, Neg, Fmt, Fmt Idempotent, Fmt Check, Attach, Process. Pay special attention to **Fmt Idempotent** (format∘format == format) and **Fmt Check** (`--check` exits 0 on already-formatted input) for every comment fixture.

- [ ] **Step 9: Commit**

```bash
git add src/Lune/Fmt.hs src/Lune/Fmt/Format.hs tests/fmt
git commit -m "feat(fmt): render comments from AST attachment; delete text-scanner"
```

---

## Task 4: New fixtures, docs, and cleanup

**Files:**
- Create: `tests/fmt/trailing_decl.{input,expected}.lune`, `tests/fmt/trailing_in_do.{input,expected}.lune`, `tests/fmt/inner_list.{input,expected}.lune`, `tests/fmt/inner_record.{input,expected}.lune`, `tests/fmt/comment_in_case.{input,expected}.lune`, `tests/fmt/doc_comment.{input,expected}.lune`
- Modify: `docs/Formatting.md`

- [ ] **Step 1: Add coverage fixtures (RED then GREEN)**

For each new scenario, create the `.input.lune` and the desired `.expected.lune`. Example — `tests/fmt/trailing_in_do.input.lune`:

```
module FmtTrailingDo exposing (main)

main =
  do
    let x = 1  -- keep me inline
    IO.println "hi"
```

`tests/fmt/trailing_in_do.expected.lune` (identical — the comment must stay put, fixing the original EOF-relocation bug):

```
module FmtTrailingDo exposing (main)

main =
  do
    let x = 1  -- keep me inline
    IO.println "hi"
```

Write the remaining five fixtures analogously (each `expected` is the canonical formatting with the comment in its correct in-place slot). The fmt golden discovery (`discoverFmtCases`) picks them up automatically — no `tests/Main.hs` change needed.

Run: `cabal test golden --test-options="-p '/Fmt/'"`
Expected: new fixtures **PASS** (and their Idempotent/Check variants). If any fail, fix the renderer, not the fixture, unless the canonical output is genuinely what the formatter should produce — then `--accept` after review.

- [ ] **Step 2: Commit fixtures** — `git add tests/fmt && git commit -m "test(fmt): coverage fixtures for trailing/inner/case/doc comments"`

- [ ] **Step 3: Update `docs/Formatting.md`**

Replace the "Comments (best-effort)" section (lines ~97–105) with a description of AST-driven attachment:

```markdown
## Comments

Comments are attached to AST nodes after parsing and rendered in place:

- **Leading** comments (on their own line above a node) render on their own line(s) above it.
- **Trailing** comments (on the same line as code) stay inline: `x = 1  -- note`.
- **Inner** comments (between elements of a do-block, list, record, or case) render at their slot.
- **Doc comments** (`{-| … -}`) render verbatim in leading position.
- Line and nested block comments are preserved exactly. No comment is ever dropped:
  a comment with no precise structural home attaches to the nearest enclosing construct.
```

Also update the line in "Style Summary" that says the formatter "does not consult existing whitespace (beyond comment capture)" to reflect that blank-line preservation and comment placement now derive from AST spans.

- [ ] **Step 4: Update README feature note (optional)**

In `README.md`, the formatting section can note comments are now fully preserved in place. Small edit; commit with docs.

- [ ] **Step 5: Final full-suite verification**

Run: `cabal test golden 2>&1 | tail -25`
Expected: **all groups PASS**. Record final counts; compare to Task 0 baseline (should be baseline + new Attach tests + new fmt fixtures).

- [ ] **Step 6: Commit docs**

```bash
git add docs/Formatting.md README.md
git commit -m "docs(fmt): document AST-driven comment preservation"
```

---

## Done criteria

- `cabal test golden` runs in the Nix dev shell and is **all green**, including `Attach`, `Fmt`, `Fmt Idempotent`, `Fmt Check`.
- The text-scanner (`formatModuleTextWithComments` & friends) is **deleted**; `git grep -n "scanDoLayouts\|formatModuleTextWithComments\|computeDoLayouts"` returns nothing.
- Trailing comments stay inline; the do-block trailing-comment-to-EOF bug is fixed (covered by `trailing_in_do`).
- No comment is dropped for any fixture (verified by the attachment `length` invariant and round-trip goldens).
- `docs/Formatting.md` no longer claims comments are "best-effort" / unattached.
```
