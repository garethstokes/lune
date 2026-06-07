# Nova Minimal Message Model — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add a stateless `List Message` API to Nova (System/User/Assistant roles), move ollama to `/api/chat`, and keep the existing string `ask`/`json`/`tool` working as sugar — unblocking the `n1` tool loop.

**Architecture:** A `Message`/`Role` type lands in `Nova/Core.lune`; `Request` and the provider dispatch seam (`ProviderImpl.call`, `callProvider`) switch their prompt argument from `String` to `List Message`; the string API in `Nova.lune` is rewired to wrap `[user prompt]` over a new messages core. Golden-test driven (Lune has no prelude unit tests) — existing eval goldens must stay byte-identical; a new multi-turn example proves history threading.

**Tech Stack:** Lune (compiler in Haskell), `cabal test golden` (tasty-golden) inside the Nix dev shell.

**Spec:** `docs/superpowers/specs/2026-06-07-nova-message-model-design.md`. **Branch:** `feat/nova-message-model`.

---

## Conventions

- All commands run inside the Nix dev shell: prefix with `nix develop --command bash -c '...'` (`cabal update` already done).
- Full suite: `cabal test golden`. Current baseline on this branch: **All 222 tests passed** (0 failures). Never regress it.
- Run a single example to iterate: `cabal run -v0 lune -- --eval examples/45_NovaV2.lune` (parse-only: drop `--eval`).
- Golden groups: `Parse`/`Core` (compile artifacts, auto-discovered from `examples/*.lune`), `Eval` (runs `lune --eval`). Accept new goldens with `cabal test golden --test-options="--accept -p '/<name>/'"` ONLY after eyeballing the diff.
- A shell hook (`rtk`) may garble `git`/`grep` output; if so, re-run prefixed with `rtk proxy `.
- CRITICAL back-compat invariant: `tests/golden/eval/41_Nova.golden` and `45_NovaV2.golden` must remain **byte-identical**. If either changes, behavior drifted — fix the code, do NOT `--accept`.

## File structure

| File | Responsibility | Change |
|------|----------------|--------|
| `prelude/Nova/Core.lune` | provider seam, types, ollama/mock impls | `Role`/`Message` + `messagesToPrompt`; `Request`→messages; `ProviderImpl.call`/`callProvider`→`List Message`; ollama→`/api/chat`; mock via `messagesToPrompt`; openrouter stub sig |
| `prelude/Nova.lune` | public API | new `*Messages` fns; `askRaw`/`ask`/`json`/`tool`/`askStream` rewired as `[user prompt]` sugar; `Request` construction |
| `examples/45_NovaV2.lune` | mock-provider demo | update `mockCall` to new `List Message` signature (behavior preserved) |
| `examples/48_Nova_MultiTurn.lune` (new) | multi-turn coverage | 2-turn conversation vs mock provider |
| `tests/golden/{parse,core,eval}/48_Nova_MultiTurn.golden` (new) | golden output | accepted after review |

---

## Task 1: Message/Role types + `messagesToPrompt` helper (additive)

Pure addition — compiles, changes no behavior, no golden impact. Safe checkpoint.

**Files:** Modify `prelude/Nova/Core.lune` (add types near `Request`, ~line 176; export them).

- [ ] **Step 1: Add the types, constructors, and helpers**

In `prelude/Nova/Core.lune`, add (place near the `Request` alias; match the file's existing 2-space-indent record/union style):

```
type Role
  = System
  | User
  | Assistant

type alias Message =
  { role : Role
  , content : String
  }

system : String -> Message
system content = { role = System, content = content }

user : String -> Message
user content = { role = User, content = content }

assistant : String -> Message
assistant content = { role = Assistant, content = content }

roleId : Role -> String
roleId role =
  case role of
    System ->
      "system"
    User ->
      "user"
    Assistant ->
      "assistant"

-- | Flatten messages into a single prompt string (newline-joined contents).
-- For a single [user p] message this returns exactly p, preserving the
-- existing single-shot mock/prompt behavior.
messagesToPrompt : List Message -> String
messagesToPrompt messages =
  Str.join "\n" (List.map (\m -> m.content) messages)
```

Add `Role(..)`, `Message`, `system`, `user`, `assistant`, `roleId`, `messagesToPrompt` to `Nova/Core.lune`'s `exposing (...)` list. Confirm `Str.join`/`List.map` are imported in this module (they are used elsewhere; if `Str.join` is named differently, grep `prelude/Lune/String.lune` for the join function and use that name — adjust `messagesToPrompt` accordingly).

- [ ] **Step 2: Build to verify it compiles**

Run: `nix develop --command bash -c 'cabal build lune -v0 2>&1 | tail -5'`
Expected: clean build. (If `Str.join` doesn't exist, fix the helper to use the actual String API — e.g. a fold with `Str.append` and `"\n"` — and rebuild.)

- [ ] **Step 3: Run the suite (must stay green)**

Run: `nix develop --command bash -c 'cabal test golden 2>&1 | tail -6'`
Expected: `All 222 tests passed`. The additions are unused so far.

- [ ] **Step 4: Commit**

```bash
git add prelude/Nova/Core.lune
git commit -m "feat(nova): add Role/Message types + messagesToPrompt helper"
```

---

## Task 2: Switch the provider seam to `List Message` (coordinated change)

This is the big atomic change: `Request`, `ProviderImpl.call`, `callProvider`, the ollama + openrouter + mock impls, the `45_NovaV2` mock, and `Nova.lune`'s `askRaw`/`ask`/`json`/`tool` all move to messages at once (they must, to compile). Behavior is preserved → existing goldens stay byte-identical.

**Files:** Modify `prelude/Nova/Core.lune`, `prelude/Nova.lune`, `examples/45_NovaV2.lune`.

- [ ] **Step 1: `Request` → messages, and the dispatch seam → `List Message`**

In `prelude/Nova/Core.lune`:

Change `Request`:
```
type alias Request =
  { provider : Provider
  , model : String
  , messages : List Message
  }
```

Change `ProviderImpl.call` and `callProvider` so the 5th/prompt argument is `List Message`:
```
type alias ProviderImpl =
  { id : Provider
  , call : ProviderConfig -> String -> List Message -> IO (Result Error Response)
  , capabilities : Set Capability
  }

callProvider :
    ProviderRegistry -> ProviderConfig -> Provider -> String -> List Message
      -> IO (Result Error Response)
callProvider registry cfg provider model messages =
  case dictGet eqProvider provider registry of
    Nothing ->
      IO.pure
        (Err (ProviderError (Str.append "Unknown provider: " (providerId provider))))
    Just impl ->
      impl.call cfg model messages
```

- [ ] **Step 2: Ollama impl → `/api/chat` with messages**

In `callOllamaHttp` (Core.lune ~320), change the signature's prompt arg to `List Message`, build the chat body, and POST to `/api/chat`:
```
callOllamaHttp :
    ProviderSettings -> String -> List Message -> IO (Result Error Response)
callOllamaHttp settings modelId messages =
  let
    hostPort = parseHostPort settings.baseUrl
    messageValues =
      List.map
        (\m ->
          Encode.object
            [ { key = "role", value = Encode.string (roleId m.role) }
            , { key = "content", value = Encode.string m.content }
            ])
        messages
    requestBody =
      Encode.object
        [ { key = "model", value = Encode.string modelId }
        , { key = "messages", value = Encode.list messageValues }
        , { key = "stream", value = Encode.bool False }
        ]
    bodyStr = Json.stringify requestBody
    httpRequest = buildHttpPost hostPort.host "/api/chat" bodyStr
  in
    ... (rest unchanged: tcpConnect/send/recvHttpResponse/parseOllamaResponse)
```
(Use the actual `Encode.list`/array constructor name from `prelude/Lune/Json/Encode.lune` — grep it; if it's `Encode.array`, use that.) Keep the `mock://` base-url branch (Core.lune ~314): it now must call `mockOllama modelId messages` (see Step 4).

- [ ] **Step 3: `parseOllamaResponse` → read `message.content`**

`/api/chat` returns `{"message":{"role":"assistant","content":"..."}, ...}` not `{"response":"..."}`. Change the decode (Core.lune ~451):
```
        case Decode.decodeValue (Decode.field "message" (Decode.field "content" Decode.string)) json of
          Err decodeErr ->
            Err (DecodeError (Str.append "Missing 'message.content' field: " decodeErr.message))
          Ok responseText ->
            Ok { text = responseText, usage = Nothing }
```
(Confirm `Decode.field` nests this way in `prelude/Lune/Json/Decode.lune`; if there's an `Decode.at ["message","content"]`, prefer that.)

- [ ] **Step 4: Mock impl takes messages (behavior-preserving)**

Change `mockOllama` to accept `List Message` and derive the prompt via `messagesToPrompt`, leaving the rest of its logic identical:
```
mockOllama : String -> List Message -> Result Error Response
mockOllama modelId messages =
  let prompt = messagesToPrompt messages
  in case Str.eq modelId "mock-tool-wrapper" of
       ... (body unchanged — it already reads `prompt`)
```
Because `messagesToPrompt [user p] == p`, the single-shot `mock-tool-wrapper` `countOccurrences` and all `mock-*` model-id branches behave exactly as before.

- [ ] **Step 5: OpenRouter stub signature**

Update `openrouterImpl`/`callOpenRouter` signature's prompt arg to `List Message` (the body still returns `Err Unauthorized` / `Err (ProviderError "...")` — unchanged). Update `ollamaImpl`'s `call` wiring to pass `messages` through to `callOllamaHttp`.

- [ ] **Step 6: Rewire `Nova.lune` `askRaw` (and the string API rides on it for now)**

In `prelude/Nova.lune`, change `askRaw` to build a single-user-message request and pass messages to `callProvider`:
```
askRaw : Client -> String -> Task Error Response
askRaw client prompt =
  let
    messages = [ Core.user prompt ]
    request = { provider = client.provider, model = client.model, messages = messages }
  in
    Task.andThen
      (liftResult
        (Core.callProvider client.providers client.providerConfig
          client.provider client.model messages))
      (\response ->
        ... hook handling unchanged (uses `request`) ...)
```
`ask`/`json`/`tool`/`askStream` are unchanged in this task — they already call `askRaw`/build on the string prompt; `tool`'s preamble wrapping still produces one `String` that becomes `[user wrappedPrompt]`. (`Core.user` must be imported/qualified — it's exported from Core in Task 1.)

- [ ] **Step 7: Update the `45_NovaV2` mock**

In `examples/45_NovaV2.lune`, `mockCall`'s type and body change from `... -> String -> IO ...` to `... -> List Message -> IO ...`. It almost certainly delegates to `Core.mockOllama` — update it to pass the message list through:
```
mockCall : ProviderConfig -> String -> List Message -> IO (Result Core.Error Core.Response)
mockCall _cfg model messages = IO.pure (Core.mockOllama model messages)
```
(Read the current `mockCall` first; preserve whatever it does, just thread `messages` instead of `prompt`. If it called `mockOllama model prompt`, the new form is `mockOllama model messages`.)

- [ ] **Step 8: Build + full suite, assert goldens byte-identical**

Run: `nix develop --command bash -c 'cabal build all -v0 2>&1 | tail -5 && cabal test golden 2>&1 | tail -8'`
Expected: clean build; `All 222 tests passed`. In particular `41_Nova`, `45_NovaV2` Eval goldens unchanged and HealthAgent still compiles. If `41`/`45` Eval FAILS, the behavior drifted (likely `messagesToPrompt` or the mock wiring) — fix the code so output matches the committed golden; do NOT accept.

- [ ] **Step 9: Confirm no golden files changed**

Run: `rtk proxy git status -s tests/golden`
Expected: no modified golden files (the seam change is behavior-preserving). If any show modified, investigate and fix before committing.

- [ ] **Step 10: Commit**

```bash
git add prelude/Nova/Core.lune prelude/Nova.lune examples/45_NovaV2.lune
git commit -m "feat(nova): provider seam takes List Message; ollama uses /api/chat"
```

---

## Task 3: Public messages API (`askRawMessages`/`askMessages`/`jsonMessages`/`toolMessages`)

Expose the messages core and make the string API thin sugar over it. Mostly generalizing Task 2's internal wrapping. Additive to behavior; goldens stay green.

**Files:** Modify `prelude/Nova.lune` (export the new functions).

- [ ] **Step 1: Add `askRawMessages` and make `askRaw` sugar**

In `prelude/Nova.lune`, introduce the messages core by renaming Task 2's `askRaw` body to `askRawMessages : Client -> List Message -> Task Error Response` (taking `messages` directly instead of `[user prompt]`), then:
```
askRawMessages : Client -> List Message -> Task Error Response
askRawMessages client messages =
  let request = { provider = client.provider, model = client.model, messages = messages }
  in Task.andThen
       (liftResult (Core.callProvider client.providers client.providerConfig
          client.provider client.model messages))
       (\response -> ... hook handling unchanged ...)

askRaw : Client -> String -> Task Error Response
askRaw client prompt = askRawMessages client [ Core.user prompt ]
```

- [ ] **Step 2: Add `askMessages`/`jsonMessages`/`toolMessages`**

Mirror the existing `ask`/`json`/`tool` bodies but over messages. For `ask`/`json` the message list is used as-is; for `tool`, the preamble/postamble wrapping must apply to the LAST user message (so a multi-turn tool call still instructs JSON-only). Concretely:
```
askMessages : Client -> List Message -> Task Error String
askMessages client messages =
  Task.map (\r -> r.text) (askRawMessages client messages)

jsonMessages : Json.Decode a => Client -> List Message -> Task Error a
jsonMessages client messages =
  Task.andThen (askRawMessages client messages)
    (\response -> decodeJsonResponse response.text)   -- same decode helper json uses

toolMessages : Json.Decode a => Client -> List Message -> Task Error a
toolMessages client messages =
  Task.andThen (askRawMessages client (wrapLastUserForTool messages))
    (\response -> decodeExtractedJson response.text)   -- same extract+decode tool uses
```
where `wrapLastUserForTool` replaces the content of the last `User` message with `Str.unlines [toolPreamble, "", originalContent, "", toolPostamble]` (the exact wrapping `tool` does today). Factor the existing decode/extract logic out of `json`/`tool` into shared helpers (`decodeJsonResponse`, `decodeExtractedJson`) so `json`/`jsonMessages` and `tool`/`toolMessages` share one implementation (DRY) — read the current `json`/`tool` bodies (Nova.lune ~332-359) and extract verbatim.

- [ ] **Step 3: Rewire string `json`/`tool` as sugar**

```
json client prompt = jsonMessages client [ Core.user prompt ]
tool client prompt = toolMessages client [ Core.user prompt ]
```
This must be behavior-identical to today: `toolMessages [user prompt]` wraps that single user message exactly as the old `tool` did. Export `askRawMessages`, `askMessages`, `jsonMessages`, `toolMessages` from `Nova.lune`.

- [ ] **Step 4: Build + full suite (goldens still green)**

Run: `nix develop --command bash -c 'cabal build all -v0 2>&1 | tail -5 && cabal test golden 2>&1 | tail -8'`
Expected: clean build; `All 222 tests passed`; `41`/`45` Eval goldens still byte-identical (`rtk proxy git status -s tests/golden` shows nothing modified).

- [ ] **Step 5: Commit**

```bash
git add prelude/Nova.lune
git commit -m "feat(nova): public List Message API (ask/json/tool Messages); string API is sugar"
```

---

## Task 4: Multi-turn example + golden (proves history threading)

**Files:** Create `examples/48_Nova_MultiTurn.lune`; accept its `parse`/`core`/`eval` goldens.

- [ ] **Step 1: Write the multi-turn example against the mock provider**

Create `examples/48_Nova_MultiTurn.lune`. Model it on `45_NovaV2.lune`'s mock-provider registration (copy the `mockProvider`/`mockProviderImpl`/`mockCall` setup). Add a mock model id whose reply ECHOES how many messages it received, so the golden visibly proves the second turn saw history. In `mockOllama` (Core.lune) add a branch:
```
                        case Str.eq modelId "mock-echo-count" of
                          True ->
                            Ok { text = Str.append "turns=" (Str.fromInt (List.length messages)), usage = Nothing }
                          False ->
                            ... existing Unknown mock fallthrough ...
```
Then the example:
```
module Main exposing (main)

import Lune.IO as IO
import Lune.Task as Task
import Nova exposing (newClient, askMessages)
import Nova.Core as Core exposing (Message, user, assistant)
... (mock provider wiring copied from 45_NovaV2, client model = "mock-echo-count") ...

main : Task Unit Unit
main =
  do
    let msgs1 = [ Core.user "first question" ]
    r1 <- askMessages client msgs1
    IO.println (Str.append "turn1: " r1)               -- expect turns=1
    let msgs2 = msgs1 ++ [ Core.assistant r1, Core.user "follow up" ]
    r2 <- askMessages client msgs2
    IO.println (Str.append "turn2: " r2)               -- expect turns=3
```
(Adjust imports/qualification to match how `45_NovaV2` references `Core`/`Nova`. The point: turn 2 sends 3 messages, so `turns=3` proves history threaded.)

- [ ] **Step 2: Run it and verify output is correct**

Run: `nix develop --command bash -c 'cabal run -v0 lune -- --eval examples/48_Nova_MultiTurn.lune'`
Expected output:
```
turn1: turns=1
turn2: turns=3
```
If it doesn't show `turns=1`/`turns=3`, the messages aren't threading — fix before accepting any golden.

- [ ] **Step 3: Accept the new goldens (after eyeballing)**

Run: `nix develop --command bash -c "cabal test golden --test-options=\"--accept -p '/48_Nova_MultiTurn/'\""`
Then inspect: `cat tests/golden/eval/48_Nova_MultiTurn.golden` shows the two `turns=` lines.

- [ ] **Step 4: Full suite green**

Run: `nix develop --command bash -c 'cabal test golden 2>&1 | tail -6'`
Expected: `All 225 tests passed` (222 + 3 new for the new example: parse/core/eval). `41`/`45` still byte-identical.

- [ ] **Step 5: Commit**

```bash
git add examples/48_Nova_MultiTurn.lune tests/golden/parse/48_Nova_MultiTurn.golden tests/golden/core/48_Nova_MultiTurn.golden tests/golden/eval/48_Nova_MultiTurn.golden prelude/Nova/Core.lune
git commit -m "test(nova): multi-turn example proving List Message history threads"
```

---

## Done criteria

- `cabal test golden` green (225/225); `41_Nova`/`45_NovaV2` eval goldens byte-identical (no behavior drift).
- `askRawMessages`/`askMessages`/`jsonMessages`/`toolMessages` exported; `ask`/`json`/`tool`/`askStream` keep their exact string signatures as sugar.
- Ollama posts to `/api/chat` with a `messages` array; reply parsed from `message.content`.
- `examples/48_Nova_MultiTurn.lune` demonstrates a 2-turn conversation (turns=1 → turns=3) against the mock provider.
- `n1` can now build a `List Message`, call `toolMessages`, append the assistant reply + a user-formatted tool result, and loop.
```
