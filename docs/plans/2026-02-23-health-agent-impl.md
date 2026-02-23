# Nova Health Agent Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Create a system health monitoring agent that uses Nova to analyze metrics and propose remediation actions with user approval.

**Architecture:** The agent follows a check → analyze → propose → execute loop. Health check tools are pure functions that return structured data (mocked for now since Lune lacks shell primitives). Nova's `tool` function extracts structured JSON actions from the LLM. Each proposed action goes through an approval flow before execution.

**Tech Stack:** Lune, Nova V2 API (`tool`, `executeTool`, RuntimeConfig guards), JSON encoding/decoding, mocked shell command outputs.

---

## Task 1: Create Empty File with Module Header

**Files:**
- Create: `examples/50_HealthAgent.lune`

**Step 1: Create the file with module declaration and imports**

```lune
module HealthAgent exposing (main)

{-| System health monitoring agent using Nova LLM analysis.

    Demonstrates:
    - Structured tool outputs with Nova.tool
    - JSON decoding of LLM responses
    - User approval workflows
    - RuntimeConfig guards for tool execution
-}

import Lune.IO as IO
import Lune.Task as Task
import Lune.String as Str
import Lune.Int as Int
import Lune.Float as Float
import Lune.List as List
import Lune.Json as RawJson exposing (Json)
import Lune.Json.Decode as D
import Lune.Json.Encode as E
import Nova exposing (newClient, provider, withModel, tool)
import Nova.Core as Core exposing (Error(..), Provider, Response)
import Nova.Tool as Tool
import Nova.Runtime as Runtime exposing (RuntimeConfig, ToolExecError(..))

main : Task Unit Unit
main =
  IO.println "Health Agent placeholder"
```

**Step 2: Run to verify it compiles**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 3: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(examples): add health agent skeleton"
```

---

## Task 2: Define Health Check Data Types

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add type definitions after imports**

```lune
-- Health check result types

type alias DiskUsage =
  { mountPoint : String
  , totalGb : Float
  , usedGb : Float
  , percentUsed : Int
  }

type alias MemoryUsage =
  { totalMb : Int
  , usedMb : Int
  , availableMb : Int
  , swapUsedMb : Int
  }

type alias ProcessInfo =
  { pid : Int
  , name : String
  , cpuPercent : Float
  , memoryPercent : Float
  }

type alias ServiceStatus =
  { name : String
  , isRunning : Bool
  , uptime : Maybe String
  }

type alias HealthReport =
  { disks : List DiskUsage
  , memory : MemoryUsage
  , topProcesses : List ProcessInfo
  , services : List ServiceStatus
  }
```

**Step 2: Run to verify types compile**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 3: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add health check data types"
```

---

## Task 3: Define LLM Analysis Response Types

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add LLM response types after health check types**

```lune
-- LLM analysis response types

type Severity
  = Critical
  | Warning
  | Info

type alias Issue =
  { severity : Severity
  , summary : String
  , details : String
  }

type alias ProposedAction =
  { id : Int
  , issue : String
  , action : String
  , command : String
  , risk : String
  , expectedOutcome : String
  }

type alias HealthAnalysis =
  { issues : List Issue
  , actions : List ProposedAction
  }
```

**Step 2: Run to verify types compile**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 3: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add LLM analysis response types"
```

---

## Task 4: Implement JSON Encoders for Health Report

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add encoders for sending health data to LLM**

```lune
-- JSON encoders for health report

encodeDiskUsage : DiskUsage -> Json
encodeDiskUsage d =
  E.object
    [ { key = "mountPoint", value = E.string d.mountPoint }
    , { key = "totalGb", value = E.float d.totalGb }
    , { key = "usedGb", value = E.float d.usedGb }
    , { key = "percentUsed", value = E.int d.percentUsed }
    ]

encodeMemoryUsage : MemoryUsage -> Json
encodeMemoryUsage m =
  E.object
    [ { key = "totalMb", value = E.int m.totalMb }
    , { key = "usedMb", value = E.int m.usedMb }
    , { key = "availableMb", value = E.int m.availableMb }
    , { key = "swapUsedMb", value = E.int m.swapUsedMb }
    ]

encodeProcessInfo : ProcessInfo -> Json
encodeProcessInfo p =
  E.object
    [ { key = "pid", value = E.int p.pid }
    , { key = "name", value = E.string p.name }
    , { key = "cpuPercent", value = E.float p.cpuPercent }
    , { key = "memoryPercent", value = E.float p.memoryPercent }
    ]

encodeServiceStatus : ServiceStatus -> Json
encodeServiceStatus s =
  E.object
    [ { key = "name", value = E.string s.name }
    , { key = "isRunning", value = E.bool s.isRunning }
    , { key = "uptime", value = case s.uptime of
        Nothing -> E.null
        Just u -> E.string u
      }
    ]

encodeHealthReport : HealthReport -> Json
encodeHealthReport r =
  E.object
    [ { key = "disks", value = E.list encodeDiskUsage r.disks }
    , { key = "memory", value = encodeMemoryUsage r.memory }
    , { key = "topProcesses", value = E.list encodeProcessInfo r.topProcesses }
    , { key = "services", value = E.list encodeServiceStatus r.services }
    ]
```

**Step 2: Run to verify encoders compile**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 3: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add JSON encoders for health report"
```

---

## Task 5: Implement JSON Decoders for LLM Response

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add severity decoder**

```lune
-- JSON decoders for LLM response

decodeSeverity : D.Decoder Severity
decodeSeverity =
  D.andThen
    (\s ->
      case s of
        "critical" -> D.succeed Critical
        "warning" -> D.succeed Warning
        "info" -> D.succeed Info
        _ -> D.fail (Str.append "Unknown severity: " s)
    )
    D.string
```

**Step 2: Add issue decoder**

```lune
decodeIssue : D.Decoder Issue
decodeIssue =
  D.map3
    (\sev sum det -> { severity = sev, summary = sum, details = det })
    (D.field "severity" decodeSeverity)
    (D.field "summary" D.string)
    (D.field "details" D.string)
```

**Step 3: Add proposed action decoder**

```lune
decodeProposedAction : D.Decoder ProposedAction
decodeProposedAction =
  D.map6
    (\id iss act cmd risk exp ->
      { id = id
      , issue = iss
      , action = act
      , command = cmd
      , risk = risk
      , expectedOutcome = exp
      })
    (D.field "id" D.int)
    (D.field "issue" D.string)
    (D.field "action" D.string)
    (D.field "command" D.string)
    (D.field "risk" D.string)
    (D.field "expectedOutcome" D.string)
```

**Step 4: Add health analysis decoder**

```lune
decodeHealthAnalysis : D.Decoder HealthAnalysis
decodeHealthAnalysis =
  D.map2
    (\iss acts -> { issues = iss, actions = acts })
    (D.field "issues" (D.list decodeIssue))
    (D.field "actions" (D.list decodeProposedAction))
```

**Step 5: Run to verify decoders compile**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 6: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add JSON decoders for LLM analysis"
```

---

## Task 6: Implement Mock Health Checks

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add mock health check functions**

Note: These return hardcoded data since Lune doesn't have shell execution primitives yet. In production, these would call `df`, `free`, `ps`, and `systemctl`.

```lune
-- Mock health check functions (would use shell commands in production)

mockDiskCheck : Task e (List DiskUsage)
mockDiskCheck =
  Task.succeed
    [ { mountPoint = "/"
      , totalGb = 50.0
      , usedGb = 35.0
      , percentUsed = 70
      }
    , { mountPoint = "/var"
      , totalGb = 20.0
      , usedGb = 18.8
      , percentUsed = 94
      }
    ]

mockMemoryCheck : Task e MemoryUsage
mockMemoryCheck =
  Task.succeed
    { totalMb = 16384
    , usedMb = 12800
    , availableMb = 3584
    , swapUsedMb = 512
    }

mockProcessCheck : Task e (List ProcessInfo)
mockProcessCheck =
  Task.succeed
    [ { pid = 1234
      , name = "node"
      , cpuPercent = 45.2
      , memoryPercent = 78.0
      }
    , { pid = 5678
      , name = "postgres"
      , cpuPercent = 12.5
      , memoryPercent = 15.3
      }
    ]

mockServiceCheck : Task e (List ServiceStatus)
mockServiceCheck =
  Task.succeed
    [ { name = "nginx", isRunning = True, uptime = Just "3d 12h" }
    , { name = "redis", isRunning = False, uptime = Nothing }
    ]
```

**Step 2: Add function to collect all health data**

```lune
collectHealthReport : Task e HealthReport
collectHealthReport =
  do
    disks <- mockDiskCheck
    memory <- mockMemoryCheck
    processes <- mockProcessCheck
    services <- mockServiceCheck
    Task.succeed
      { disks = disks
      , memory = memory
      , topProcesses = processes
      , services = services
      }
```

**Step 3: Run to verify mock checks compile**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 4: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add mock health check functions"
```

---

## Task 7: Implement Display Functions

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add severity display helper**

```lune
-- Display functions

showSeverity : Severity -> String
showSeverity sev =
  case sev of
    Critical -> "CRITICAL"
    Warning -> "WARNING"
    Info -> "INFO"
```

**Step 2: Add issue display function**

```lune
showIssue : Issue -> String
showIssue iss =
  Str.concat
    [ "  * ["
    , showSeverity iss.severity
    , "] "
    , iss.summary
    ]

showIssues : List Issue -> String
showIssues issues =
  case issues of
    [] -> "No issues found."
    _ ->
      Str.concat
        [ "Issues found:\n"
        , Str.join "\n" (List.map showIssue issues)
        ]
```

**Step 3: Add action display function**

```lune
showAction : Int -> Int -> ProposedAction -> String
showAction current total act =
  Str.concat
    [ "+-------------------------------------------------+\n"
    , "| Action "
    , Str.fromInt current
    , " of "
    , Str.fromInt total
    , Str.padLeft 30 ' ' (Str.concat ["[", act.risk, " risk]"])
    , "\n"
    , "|\n"
    , "| Issue: "
    , act.issue
    , "\n"
    , "|\n"
    , "| Proposed: "
    , act.action
    , "\n"
    , "| Command:  "
    , act.command
    , "\n"
    , "| Expected: "
    , act.expectedOutcome
    , "\n"
    , "+-------------------------------------------------+"
    ]
```

**Step 4: Run to verify display functions compile**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 5: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add display functions"
```

---

## Task 8: Implement Mock LLM Provider

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add mock provider that returns structured health analysis**

This mock provider simulates what a real LLM would return. The response is a valid JSON health analysis.

```lune
-- Mock LLM provider for deterministic testing

mockHealthProvider : Provider
mockHealthProvider = provider "mock-health"

mockHealthCall : Core.ProviderConfig -> String -> String -> IO (Result Core.Error Response)
mockHealthCall _ _ _ =
  IO.pure
    <| Ok
      { text = mockHealthAnalysisJson
      , usage = Just { inputTokens = 100, outputTokens = 50, totalTokens = 150 }
      }

mockHealthAnalysisJson : String
mockHealthAnalysisJson =
  """
{
  "issues": [
    {
      "severity": "critical",
      "summary": "Disk /var is 94% full",
      "details": "Only 1.2GB remaining on /var partition"
    },
    {
      "severity": "warning",
      "summary": "High memory usage",
      "details": "Process 'node' using 78% of available memory"
    },
    {
      "severity": "warning",
      "summary": "Service redis is down",
      "details": "Redis service is not running"
    }
  ],
  "actions": [
    {
      "id": 1,
      "issue": "Disk /var is 94% full",
      "action": "Clear journal logs older than 7 days",
      "command": "journalctl --vacuum-time=7d",
      "risk": "low",
      "expectedOutcome": "Free approximately 1-2GB"
    },
    {
      "id": 2,
      "issue": "Service redis is down",
      "action": "Restart redis service",
      "command": "systemctl restart redis",
      "risk": "medium",
      "expectedOutcome": "Redis service will be available"
    }
  ]
}
"""

mockHealthProviderImpl : Core.ProviderImpl
mockHealthProviderImpl =
  { id = mockHealthProvider
  , call = mockHealthCall
  , capabilities = Core.setEmpty
  }
```

**Step 2: Run to verify mock provider compiles**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 3: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add mock LLM provider"
```

---

## Task 9: Implement Analysis Prompt Builder

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add prompt building function**

```lune
-- LLM analysis

analysisSystemPrompt : String
analysisSystemPrompt =
  """
You are a Linux sysadmin assistant. Analyze these system
health metrics and identify issues that need attention.

Severity guidelines:
- Critical: System at risk of failure (disk >90%, OOM imminent)
- Warning: Degraded performance or trending toward critical
- Info: Notable but not actionable

For each issue, propose a concrete remediation action with
the exact command to run. Assess risk level:
- low: Safe, no service impact (clear caches, rotate logs)
- medium: Brief impact (restart service, kill process)
- high: Potential data loss or extended downtime

Return actions in priority order (most urgent first).
"""

buildAnalysisPrompt : HealthReport -> String
buildAnalysisPrompt report =
  Str.concat
    [ analysisSystemPrompt
    , "\n\nHealth Report:\n"
    , RawJson.stringify (encodeHealthReport report)
    ]
```

**Step 2: Run to verify prompt builder compiles**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 3: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add analysis prompt builder"
```

---

## Task 10: Implement Approval Loop

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Add single action approval function**

```lune
-- Approval loop

type ApprovalResult
  = Approved
  | Skipped
  | Quit

promptForApproval : Task e ApprovalResult
promptForApproval =
  do
    IO.println "Execute this action? [y/n/q] "
    input <- IO.readLine
    case input of
      "y" -> Task.succeed Approved
      "n" -> Task.succeed Skipped
      "q" -> Task.succeed Quit
      _ ->
        do
          IO.println "Please enter y, n, or q"
          promptForApproval
```

**Step 2: Add mock command execution**

```lune
mockExecuteCommand : String -> Task e String
mockExecuteCommand cmd =
  Task.succeed (Str.concat ["[MOCK] Would execute: ", cmd])
```

**Step 3: Add approval loop for list of actions**

```lune
processActions : Int -> List ProposedAction -> Task e Unit
processActions total actions =
  processActionsGo 1 total actions

processActionsGo : Int -> Int -> List ProposedAction -> Task e Unit
processActionsGo current total actions =
  case actions of
    [] ->
      do
        IO.println "All actions processed."
        Task.succeed unit

    Cons act rest ->
      do
        IO.println (showAction current total act)
        approval <- promptForApproval
        case approval of
          Quit ->
            do
              IO.println "Quitting. Remaining actions skipped."
              Task.succeed unit

          Skipped ->
            do
              IO.println "Action skipped."
              processActionsGo (Int.add current 1) total rest

          Approved ->
            do
              result <- mockExecuteCommand act.command
              IO.println result
              IO.println "Action completed."
              processActionsGo (Int.add current 1) total rest
```

**Step 4: Run to verify approval loop compiles**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: `Health Agent placeholder`

**Step 5: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): add approval loop"
```

---

## Task 11: Implement Main Function

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Update main to wire everything together**

```lune
main : Task Unit Unit
main =
  do
    IO.println "=== Health Agent ==="
    IO.println ""

    -- Collect health data
    IO.println "Collecting system health data..."
    report <- collectHealthReport
    IO.println "Health data collected."
    IO.println ""

    -- Set up Nova client with mock provider
    let providers =
      Core.dictInsert
        Nova.providerEq
        mockHealthProvider
        mockHealthProviderImpl
        Core.defaultProviderRegistry

    let client =
      newClient providers mockHealthProvider "health-analyzer"

    -- Analyze with LLM
    IO.println "Analyzing with LLM..."
    let prompt = buildAnalysisPrompt report
    analysisResult <- Task.result (tool client prompt)

    case analysisResult of
      Err e ->
        IO.println (Str.concat ["Analysis failed: ", Nova.showError e])

      Ok analysisJson ->
        case D.decodeValue decodeHealthAnalysis analysisJson of
          Err decodeErr ->
            IO.println (Str.concat ["Failed to decode analysis: ", decodeErr.message])

          Ok analysis ->
            do
              IO.println ""
              IO.println (showIssues analysis.issues)
              IO.println ""

              case analysis.actions of
                [] ->
                  IO.println "No remediation actions proposed."

                _ ->
                  do
                    IO.println (Str.concat
                      [ "Proposed "
                      , Str.fromInt (List.length analysis.actions)
                      , " remediation actions:"
                      ])
                    IO.println ""
                    processActions (List.length analysis.actions) analysis.actions
```

**Step 2: Run to see full agent output**

Run: `cabal run lune -- examples/50_HealthAgent.lune`
Expected: Agent displays health issues and prompts for action approval.

**Step 3: Commit**

```bash
git add examples/50_HealthAgent.lune
git commit -m "feat(health-agent): implement main orchestration"
```

---

## Task 12: Create Golden Test

**Files:**
- Create: `tests/golden/core/50_HealthAgent.golden`

**Step 1: Run with automated inputs and capture output**

We need a non-interactive test. Update the example to detect test mode or run once with scripted input.

**Step 2: For now, create a simple golden file**

The golden test for this example needs special handling since it's interactive. Create a minimal test that just verifies the agent starts:

```
=== Health Agent ===

Collecting system health data...
Health data collected.

Analyzing with LLM...

Issues found:
  * [CRITICAL] Disk /var is 94% full
  * [WARNING] High memory usage
  * [WARNING] Service redis is down

Proposed 2 remediation actions:

+-------------------------------------------------+
| Action 1 of 2                        [low risk]
|
| Issue: Disk /var is 94% full
|
| Proposed: Clear journal logs older than 7 days
| Command:  journalctl --vacuum-time=7d
| Expected: Free approximately 1-2GB
+-------------------------------------------------+
Execute this action? [y/n/q]
```

**Step 3: Commit**

```bash
git add tests/golden/core/50_HealthAgent.golden
git commit -m "test(health-agent): add golden test"
```

---

## Task 13: Final Review and Polish

**Files:**
- Modify: `examples/50_HealthAgent.lune`

**Step 1: Review the complete implementation**

Verify:
- All imports are used
- No dead code
- Module documentation is complete
- Code follows existing patterns in the codebase

**Step 2: Run full test suite**

Run: `cabal test`
Expected: All tests pass

**Step 3: Final commit**

```bash
git add -A
git commit -m "feat(examples): complete health agent implementation"
```

---

## Notes

### Missing Language Features

This implementation mocks several things that would require language features not yet in Lune:

1. **Shell command execution** - `mockDiskCheck`, `mockMemoryCheck`, etc. would need a `Lune.Process` module with `exec : String -> List String -> Task ProcessError String`

2. **Environment variables** - Reading `OLLAMA_BASE_URL` and `HEALTH_AGENT_MODEL` would need a `Lune.Env` module

3. **Multi-line string literals** - The `"""` syntax for the JSON response and prompts - verify this is supported, otherwise use string concatenation

### Future Enhancements

When shell execution is added, update the health checks to call:
- `df -h --output=target,size,used,pcent` for disk
- `free -m` for memory
- `ps aux --sort=-%mem | head -n 10` for processes
- `systemctl is-active {service}` for service status

### Testing Strategy

Since the example is interactive, the golden test captures output up to the first user prompt. For full testing, consider:
- Adding a `--non-interactive` flag that auto-approves all actions
- Or splitting into library + CLI where the library can be unit tested
