# Nova Minimal Message Model

**Date:** 2026-06-07 · **Status:** Approved (design) · **Beads:** `lune-n4` (unblocks `lune-n1`)
**Author:** Gareth Stokes (with Claude)

## Problem

Nova is single-shot: `Request = { provider, model, prompt : String }`, and `ask`/`json`/`tool`
each take one `String` and return one response. There is no conversation history and no way to
feed a turn's result back. The Nova audit (`docs/nova-audit.md`) found this is a **prerequisite
for `n1` (the real tool loop)**: closing the loop requires appending the assistant's reply and the
tool's result as further turns, which needs a message list. This spec adds the minimal message
model that unblocks `n1` and enables multi-turn, without changing existing behavior.

## Goal & non-goals

**Goal:** a minimal, stateless message API — `Message`/`Role` types, a `messages` list on the
request, ollama `/api/chat` support, and `*Messages` API functions — with the existing string-based
`ask`/`json`/`tool` preserved as sugar so no caller changes.

**Non-goals (deferred):** a first-class `Tool` role (tool results are appended as `User` messages),
streaming-with-messages (`n2`), system-prompt helpers, token/temperature/other request params,
multimodal content, OpenRouter/cloud transport (`n8`/`n5`).

## Design decisions (settled in brainstorming)

1. **Stateless messages list**, not a carried `Conversation` value. The caller (e.g. `n1`'s agent
   loop) owns the `List Message` and appends turns explicitly. No hidden state — idiomatic for a
   pure language.
2. **Three roles** — `System | User | Assistant`, `content : String`. A tool result is appended as a
   `User` message (the caller formats it). Defer a `Tool` role until a provider needs it.

## Architecture

### Data model (`prelude/Nova/Core.lune`)

```
type Role = System | User | Assistant
type alias Message = { role : Role, content : String }

system    : String -> Message      -- { role = System,    content = ... }
user      : String -> Message
assistant : String -> Message
roleId    : Role -> String         -- System->"system", User->"user", Assistant->"assistant"
```

### Request & provider seam

`Request` changes from `{ provider, model, prompt : String }` to
`{ provider, model, messages : List Message }`.

The provider dispatch seam changes its prompt argument from `String` to `List Message`:

```
-- ProviderImpl.call and callProvider: the 5th argument becomes the message list
callProvider : ProviderRegistry -> ProviderConfig -> Provider -> String -> List Message
               -> IO (Result Error Response)
```

Each `ProviderImpl` builds its own chat payload from `messages`, so the registry stays the single
dispatch point and the abstraction (which the audit found clean) is preserved.

### API surface (`prelude/Nova.lune`)

New messages-based core (what `n1` drives):

```
askRawMessages : Client -> List Message -> Task Error Response
askMessages    : Client -> List Message -> Task Error String
jsonMessages   : Json.Decode a => Client -> List Message -> Task Error a
toolMessages   : Json.Decode a => Client -> List Message -> Task Error a
```

Existing string API preserved as sugar (signatures and behavior unchanged):

```
askRaw client prompt = askRawMessages client [ user prompt ]
ask     client prompt = askMessages  client [ user prompt ]
json    client prompt = jsonMessages client [ user prompt ]
tool    client prompt = toolMessages client [ user prompt ]   -- still wraps with toolPreamble/postamble
askStream client prompt -> unchanged (wraps [ user prompt ])
```

The `onTurn` hook keeps firing in the messages core. `tool`'s preamble/postamble wrapping behavior
is preserved (the tool-prompt text is injected into the single user message, exactly as today).

## Provider payloads

- **Ollama:** move from `POST /api/generate` `{model, prompt, stream:false}` to
  **`POST /api/chat`** `{model, messages: [{role: roleId, content}], stream:false}`, and parse the
  reply's `message.content` field instead of `response`. `mock://` base-url routing is preserved.
- **OpenRouter:** signature updated to take `messages`; remains the existing "not implemented" stub.
- **Mock provider** (registered by `45_NovaV2.lune`): updated to derive its fixture-lookup key from
  the messages. **Back-compat invariant:** for a single-shot call `[user prompt]`, the derived key
  MUST equal today's `prompt` string, so existing mock fixtures and the `41_Nova`/`45_NovaV2` eval
  goldens remain byte-identical.

## Data flow

```
caller builds [Message]
  -> toolMessages / jsonMessages / askMessages
  -> askRawMessages (fires onTurn hook)
  -> callProvider registry cfg provider model messages
  -> ProviderImpl.call (ollama: POST /api/chat with messages)
  -> Response { text, usage }
  -> (tool/json: extractJson + decode)
```

## Error handling

Unchanged `Error` sum (`NetworkError | RateLimited | Unauthorized | BadRequest | ProviderError |
DecodeError`). An empty `messages` list is the caller's responsibility; providers may return
`BadRequest` — no new error variants.

## Backward compatibility

The string `ask`/`json`/`tool`/`askStream` keep their exact signatures, riding on the messages
core. **No example changes** — `50_HealthAgent.lune`, `41_Nova.lune`, `45_NovaV2.lune` are untouched
and their goldens stay green (guaranteed by the mock-key invariant above). `Request` is internal to
the provider path; no user code constructs it directly.

## Testing

1. **Back-compat (must stay green, unchanged):** `41_Nova` / `45_NovaV2` eval goldens; HealthAgent
   compiles (`parse`/`core` goldens).
2. **New multi-turn coverage:** `examples/48_Nova_MultiTurn.lune` — build a 2-turn conversation
   against the **mock** provider (turn 1 → append assistant reply + a follow-up user message →
   turn 2), with an eval golden asserting the second turn observed the prior history. Deterministic,
   ollama-free.
3. **Payload check:** the mock provider asserts (via its returned text) on the messages it received,
   confirming `messages` thread to the provider with correct roles/order.

## Affected code

- `prelude/Nova/Core.lune` — `Role`/`Message` types + constructors; `Request` shape; `callProvider`
  & `ProviderImpl.call` signature; `callOllamaHttp` → `/api/chat`; mock provider keying.
- `prelude/Nova.lune` — `*Messages` functions; string API rewired as sugar; `Request` construction.
- `prelude/Nova/Tool.lune` / `Prompt.lune` — only if `tool` wrapping touches `Request` construction.
- `examples/48_Nova_MultiTurn.lune` (new) + its eval golden.

## Risks

- **Mock-key drift** silently changing `41`/`45` goldens — mitigated by the explicit single-shot key
  invariant and verifying those goldens are byte-identical (do NOT `--accept` them).
- **Ollama `/api/chat` response shape** differs from `/api/generate` (nested `message.content`) — the
  parser change must match; covered by the mock test and a manual ollama check if available.
- **`tool` preamble injection** must still produce identical prompt text in the single-message case —
  verified by the unchanged `41`/`45` tool goldens.
