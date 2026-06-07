# Nova Capability Audit & Production-Agent Gap Analysis

**Date:** 2026-06-07 · **Beads:** `lune-n0` (drives `lune-n1`..`n7`)

Evidence-based audit of the Nova AI-agent framework (`prelude/Nova*.lune`, ~1480 lines),
the flagship `examples/50_HealthAgent.lune`, and the mock-backed `41_Nova`/`45_NovaV2`
examples. Goal: separate what actually works from what's missing for a *production* agent
framework, and validate/sharpen the Nova task breakdown.

## TL;DR

Nova is a **clean, working decision-extraction engine with a stubbed actuation loop.**
The parts usually faked (real `/proc` data collection, a real ollama HTTP call, structured
JSON decode with prose-tolerant extraction, guard logic) are real. The part that makes it an
*agent* — acting on its own decisions and feeding results back — does not exist. The provider
abstraction is genuinely extensible; the limiting factor for cloud models is a missing TLS
transport, not the registry.

## Capability map

| Area | Works | Missing / fragile | Severity |
|---|---|---|---|
| **Providers** (`Core.lune:183-505`) | Clean `ProviderImpl`+registry; ollama HTTP is **real** (raw socket, `/api/generate`); `mock://` fixtures | OpenRouter is a **stub** (`ProviderError "HTTP client not implemented"`); transport is **plaintext only** (no TLS); only ollama/openrouter | Important |
| **ask/json/tool** (`Nova.lune:300-359`, `Prompt.lune`) | `extractJson` is total & prose-tolerant (prefix/suffix); clean `Error` sum; path-aware decode errors | **No retry/reprompt** on decode failure (terminal fail); **`tool` ignores the schema** — `composeToolPrompt` is wired but never called; no schema validation | Important |
| **Streaming** (`Nova.lune:388-498`) | `Stream`/`StreamStep`/`NovaEvent` types; `askStream` yields chunks | **Fake** — chunks a *complete* non-streaming response (ollama req hardcodes `"stream": false`); no token-level HTTP streaming; `ToolCallDelta` never emitted | Important (low leverage) |
| **Tool execution / runtime** (`Runtime.lune`, `Nova.lune:520-568`) | Real guards: `allowedTools`, `requireApproval`, `maxTurns`, fiber-based `withTimeout` | **Nova never executes tools** — executor is caller-supplied, defaults to `ToolNotImplemented`; **no link from LLM action → execution**; `Lune.Process` exists but is **imported by no Nova module** | **Blocking** |
| **Multi-turn / memory** | — | **Everything single-shot**: `Request = {provider,model,prompt}` (one string); no `Message`/`Role`, no history on `Client`; ollama uses `prompt` not `messages` | Important |
| **Observability** (`Nova.lune:159-171`) | `NovaHooks` (onTurn/onToolStart/onToolFinish) fire with timing | **No retry/backoff/rate-limit** (`RateLimited` defined, never produced); hook errors swallowed; no max-tokens/temperature params | Important (retry) |
| **CI coverage** | `41_Nova`/`45_NovaV2` have mock-backed eval goldens | **HealthAgent has no eval golden** — the flagship is never executed in tests (needs live ollama) | Important |

## HealthAgent: real vs mocked (the capability probe)

`check → analyze → propose → record` — **not** `→ execute`:
1. **CHECK (real):** double-samples `System.sample` 500ms apart, diffs (`50:138-167`); real `/proc`.
2. **ANALYZE (real):** renders a sysadmin prompt + calls `tool` against ollama `qwen3` (`50:331-348`).
3. **PROPOSE (real):** decodes `HealthAnalysis = {issues, actions}` via `@derive(Json)`.
4. **CONFIRM (real, hand-rolled):** y/n/q `IO.readLine` loop — **not** Nova's `requireApproval` guard.
5. **EXECUTE (mocked/absent):** approved commands are **appended to `approved-commands.sh` and never run** (`50:405-451`). No `executeTool`, no `RuntimeConfig`, no `Lune.Process`.

The doc claim that health checks are "mocked" is **out of date** — they're real; it's *execution* that's stubbed.

## Synthesis — highest-leverage gaps (priority order)

1. **Close the tool loop (`n1`) — BLOCKING.** Route a decoded `ProposedAction` through `executeTool` + `RuntimeConfig` guards into `Lune.Process`, and feed the result back as a turn. Today decision-extraction and guarded-execution are two disjoint halves, manually bridged. All primitives exist; only the wiring is missing.
2. **`n1` depends on `n4` (minimal messages).** Feeding a tool result back *requires* a message list. `Request` is single-`prompt` today. Treat a minimal `Message`/`messages` model as a prerequisite slice of `n1`, not an independent task → **`n1` is now gated on `n4`.**
3. **TLS transport is the real blocker for cloud providers, not the registry (`n5`).** The provider abstraction is clean (adding an impl is trivial); the missing piece is an **HTTPS/TLS HTTP client**. Split that out as its own task (`n8`) that `n5` (and real cloud streaming) consume.
4. **Re-scope `n3`: retry-reprompt + schema-conformance.** Extraction is already decent; the gaps are (a) retry-on-decode-failure with reprompt, (b) actually using the schema (`composeToolPrompt`) and validating the decoded result, (c) `RateLimited`/backoff handling.
5. **Streaming (`n2`) is lowest leverage** — fully simulated today; real value mostly arrives with a streaming-capable cloud provider, so it trails the transport work. Ollama NDJSON streaming can land independently.

## Decomposition changes (applied to beads)

- **`n1` ← `n4`**: add dependency (real tool loop needs the message model).
- **New `n8` — HTTPS/TLS transport**: `n5` depends on it; real cloud streaming (`n2`) benefits.
- **New `n9` — HealthAgent eval golden / CI harness**: the flagship is currently untested; add a mock-ollama eval golden like `41_Nova`'s.
- **Re-scope** `n1` (close-the-loop framing), `n3` (retry + schema), `n4` (minimal Message model), `n5` (cloud provider impl atop `n8`).

## Open uncertainty

Not run against a live ollama (none in the audit sandbox); "real vs mocked" judgments are from
code paths, prims (`prim_tcpConnect`, `prim_process_*`, `/proc` readers), and existing eval
goldens, all of which corroborate the above. Claims about LLM *response quality* are out of scope.
