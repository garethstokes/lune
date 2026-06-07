# Comment Preservation: Migrate Formatter to AST-Attached Comments

**Date:** 2026-06-07
**Status:** Approved (design)
**Author:** Gareth Stokes (with Claude)

## Problem

The Lune formatter (`lune --fmt`) currently preserves comments using a
text-based re-scanner in `src/Lune/Fmt.hs` (`formatModuleTextWithComments`,
`computeDoLayouts`, `scanDoLayouts`, ~600 lines). The scanner ignores the AST
and re-reads source text to inject comments at top-level boundaries and
do-block boundaries. It is fragile and places comments imprecisely:

- A trailing comment (`x = 1  -- note`) is pushed onto its own line below the
  declaration instead of kept inline.
- A trailing comment inside a `do` block can be dumped at end-of-file
  (observed: a `let x = 1  -- trailing` inside `do` relocates the comment to
  the end of the module).

A separate, half-built mechanism already exists: the parser produces a
`Located` AST (`src/Lune/Syntax/Located.hs`) and a `Comments` structure with
`Leading`/`Trailing`/`Inner` slots (`src/Lune/Syntax/Comment.hs`). The parser
currently attaches **only leading** comments; `attachTrailing`
(`src/Lune/Parser.hs:90`) is an explicit stub, and the formatter renders
**none** of the attached comments (`src/Lune/Fmt/Format.hs:445` TODO). The
existing fmt goldens bake in the old scanner's behavior.

## Goal

Complete the original intent of the in-flight work: render **all** comments
(leading, trailing, inner) from AST-attached data and **retire the
text-scanner**. This is a **big-bang cutover** — the scanner is removed and the
AST-driven path replaces it in one migration, accepting a temporary broken
window and a large diff in exchange for a clean end state.

## Non-goals

- No comment-reflow, no doc-comment semantic processing beyond verbatim
  rendering, no configuration knobs (the formatter remains canonical-style).
- No change to formatting style other than comment placement and the blank-line
  rules already in effect.

## Architecture

### Decision: post-parse attachment pass (A1)

Comment attachment lives in a **new, single, unit-testable module** rather than
threaded through the megaparsec grammar.

1. **Parser** (`src/Lune/Parser.hs`): capture *every* comment into one flat
   list with its line/col span (the `Comment` record already carries
   `commentLine`/`commentCol`/`commentEndLine`/`commentEndCol`). The in-flight
   inline leading-attachment (`StateT` `consumePendingComments`, located leading
   capture) and the stubbed `attachTrailing` are **reverted** in favor of
   "collect everything flat; attach nothing during parse." The parser exposes
   the flat comment list alongside the parsed `Located` module.

2. **Attachment pass** (new module, e.g. `Lune.Syntax.Comments.Attach`):
   a pure function `attachComments :: Module -> [Comment] -> Module`
   that walks the AST once and assigns each comment to exactly one node, by
   position heuristics (below).

   **Top-level note:** `Module`, `Decl`, and `Import` are **not** `Located`
   (only the recursive positions inside them are). Per design decision X, rather
   than Located-wrapping every `Decl`/`Import` (which would ripple through every
   compiler pass), `Module` gains a `modComments :: [Comment]` field holding
   top-level *standalone/leading* comments with their spans; the module renderer
   interleaves them with imports/decls by span position. Top-level **trailing**
   comments (`x = 1  -- note`) still attach structurally — they land in the
   trailing slot of the innermost `Located` node on that line (e.g. the RHS
   `Located Expr`). Everything below declaration level is fully structural via
   `locComments`.

3. **Formatter** (`src/Lune/Fmt/Format.hs`): render comments from `locComments`
   at each `Located` node. `Fmt.hs`'s scanner machinery
   (`formatModuleTextWithComments`, `DoLayout`, `scanDoLayouts`,
   `computeDoLayouts`, `BlankLineMap`) is **deleted**; `formatText` is rewired
   to: parse → attach → render.

### Data flow

```
source text
  │  Parser.parse* (captures flat [Comment] + Located Module)
  ▼
(Located Module, [Comment])
  │  attachComments  (heuristics; every comment lands in exactly one slot)
  ▼
Located Module  (locComments populated: Leading / Trailing / Inner)
  │  Format.format  (renders comments in place; blanks from spans)
  ▼
formatted text
```

## Comment attachment semantics

Each comment is assigned to exactly one node, in one slot. Slots already exist
in `Comment.hs` (`Leading`, `Trailing`, `Inner`).

1. **Trailing** — comment starts on the *same line* as a node's end span, after
   it → `Trailing` on that node. Rendered inline, gofmt-style:
   `x = 1  -- note` (two spaces before `--`).
2. **Leading** — comment on its own line(s), immediately above a node → `Leading`
   on that node. Rendered on its own line(s) above the node.
3. **Inner** — comment inside a compound (between do-statements, list/record
   elements, case branches) that is not trailing a specific child → `Inner` on
   the container, keyed by the child-gap index it precedes so the renderer can
   slot it at the correct boundary. Top-level standalone comments (between
   imports, between decls, leading the first decl) are the module-level
   equivalent and live in `Module.modComments`, placed by span position.
4. **Doc comments** (`{-| -}`) — treated as `Leading`; rendered verbatim in
   place. No special semantics in v0.1.
5. **Determinism / no silent drops** — each comment attaches to exactly one
   node. Ties: nearest following node on a new line wins for `Leading`; nearest
   preceding node on the same line wins for `Trailing`. A comment with no home
   (e.g. trailing whitespace at EOF) attaches as `Trailing` on the last
   declaration / module so nothing is dropped.

### Rendering

At each `Located` node the formatter emits: `Leading` comments (own lines) →
the node → `Trailing` comment (same line). Compound renderers (do, let, list,
record, case, import group) consult `Inner` comments at each child boundary and
emit them on their own lines at that point.

## Blank-line preservation

Retiring the scanner removes its `BlankLineMap` blank-line logic; recover it
from AST spans. Between two sibling nodes, if
`next.startLine - prev.endLine > 1` emit exactly one blank line (multiple blanks
collapse to one — the current canonical rule). Applies at top-level
declarations and within do/let blocks. A comment's own span participates in the
gap calculation so blanks around comments are not double-counted. This keeps the
`do_blank_line` and `do_blank_before_bind` goldens green.

## Testing strategy (TDD)

1. **Step 0 — fix the dev-shell cabal** so `cabal test golden` runs natively in
   the Nix dev shell (currently fails offline: `unknown package:
   parser-combinators`). Likely `cabal update` and/or a `cabal.project`
   adjustment. This is a hard prerequisite for the RED/GREEN loop; the migration
   does not start until `cabal test golden` runs.
2. **Attachment unit tests (new group)** — assert `attachComments` output
   directly on the AST for each heuristic: trailing same-line, leading own-line,
   inner-in-do, inner-in-list, inner-in-record, comment-in-case, EOF leftover.
   This is where correctness is won; goldens are too coarse to pin attachment.
3. **Golden fixtures written first (RED)** — rewrite `comments.expected.lune`
   and `do_comment.expected.lune` for the new inline behavior; add fixtures for
   trailing-on-decl, trailing-in-do, inner-in-list, inner-in-record,
   comment-in-case, doc-comment. Watch them fail against the current formatter,
   then implement to green.
4. **Idempotence + round-trip** — the existing `Fmt Idempotent` and `Fmt Check`
   groups must stay green for every fixture: `format (format x) == format x`,
   and formatted output parses to the same AST (modulo comments).

## Affected code

- `src/Lune/Syntax.hs` — add `modComments :: [Comment]` field to `Module`.
- `src/Lune/Parser.hs` — flat comment capture; revert inline attachment + the
  `attachTrailing` stub; thread the captured comment list out of the parse.
- `src/Lune/Syntax/Comments/Attach.hs` — **new** attachment pass
  (`attachComments :: Module -> [Comment] -> Module`).
- `src/Lune/Fmt.hs` — delete scanner; rewire `formatText` to parse → attach →
  render.
- `src/Lune/Fmt/Format.hs` — render `locComments`; blank lines from spans.
- `tests/Main.hs` — new attachment test group; the `cabal run lune` golden
  runner stays as-is once cabal is fixed.
- `tests/fmt/*` — rewritten + new fixtures.
- `docs/Formatting.md` — update the now-stale "Comments (best-effort)" section
  to describe AST-driven attachment.

## Risks

- **Attachment heuristics are bug-prone.** Mitigated by isolating them in one
  pass with direct AST unit tests.
- **Big-bang broken window.** Accepted per the chosen strategy; mitigated by
  landing the test harness first and keeping the migration on a branch until
  goldens + idempotence are all green.
- **Span accuracy.** The whole design relies on accurate `Located` spans; if any
  node has a dummy/`noLoc` span, attachment and blank-line logic degrade. Verify
  spans are real for all parsed (non-generated) nodes during implementation.
