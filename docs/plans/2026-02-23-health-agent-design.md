# Nova Health Agent Design

A system health monitoring agent that uses Nova to analyze system metrics and propose remediation actions with user approval.

## Overview

The agent follows a **check → analyze → propose → execute** loop:

```
┌─────────────────────────────────────────────────────────┐
│                     Health Agent                        │
├─────────────────────────────────────────────────────────┤
│                                                         │
│  1. COLLECT        Run all health check tools           │
│       │            (disk, memory, processes, services)  │
│       ▼                                                 │
│  2. ANALYZE        Send findings to LLM via Nova        │
│       │            "Analyze these metrics, identify     │
│       │             issues, prioritize by severity"     │
│       ▼                                                 │
│  3. PROPOSE        LLM returns structured actions       │
│       │            [{action, reason, risk, command}]    │
│       ▼                                                 │
│  4. CONFIRM        Show each action, prompt user        │
│       │            for approval (y/n)                   │
│       ▼                                                 │
│  5. EXECUTE        Run approved commands via            │
│                    runCommand tool with guards          │
│                                                         │
└─────────────────────────────────────────────────────────┘
```

**Key design decisions:**

- **Tools are pure data gatherers** - Check tools collect info, they don't decide anything
- **LLM does the reasoning** - Nova analyzes raw metrics and decides what's concerning
- **Structured output** - We use `Nova.tool` to get JSON actions, not free-form text
- **Human in the loop** - Every action requires explicit approval before execution

## Data Types

### Health Check Results

```
DiskUsage =
  { mountPoint : String
  , totalGb : Float
  , usedGb : Float
  , percentUsed : Int
  }

MemoryUsage =
  { totalMb : Int
  , usedMb : Int
  , availableMb : Int
  , swapUsedMb : Int
  }

ProcessInfo =
  { pid : Int
  , name : String
  , cpuPercent : Float
  , memoryPercent : Float
  }

ServiceStatus =
  { name : String
  , isRunning : Bool
  , uptime : Maybe String
  }

HealthReport =
  { disks : List DiskUsage
  , memory : MemoryUsage
  , topProcesses : List ProcessInfo
  , services : List ServiceStatus
  , logSnippets : List { source : String, lines : List String }
  }
```

### LLM Analysis Output

```
Severity = Critical | Warning | Info

Issue =
  { severity : Severity
  , summary : String
  , details : String
  }

ProposedAction =
  { id : Int
  , issue : String
  , action : String
  , command : String
  , risk : String          -- "low", "medium", "high"
  , expectedOutcome : String
  }

HealthAnalysis =
  { issues : List Issue
  , actions : List ProposedAction
  }
```

## Tools

| Tool | Command | Purpose |
|------|---------|---------|
| `checkDisk` | `df -h --output=target,size,used,pcent` | Get disk usage per mount point |
| `checkMemory` | `free -m` | Get memory/swap usage |
| `listTopProcesses` | `ps aux --sort=-%mem \| head -n {count}` | List processes by memory |
| `checkService` | `systemctl is-active {name}` | Check if service is running |
| `readLogTail` | `tail -n {count} {path}` | Read last N lines of log |
| `runCommand` | User-provided command | Execute with approval |

**Security constraints:**

- `checkService` validates service name (alphanumeric + hyphen only)
- `readLogTail` restricts paths to `/var/log`
- `runCommand` gated behind approval flow and Nova's RuntimeConfig guards

## LLM Integration

### Analysis Prompt

```
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
```

### Expected JSON Response

```json
{
  "issues": [
    { "severity": "critical", "summary": "...", "details": "..." }
  ],
  "actions": [
    { "id": 1,
      "issue": "Disk /var at 94%",
      "action": "Clear journal logs older than 7 days",
      "command": "journalctl --vacuum-time=7d",
      "risk": "low",
      "expectedOutcome": "Free approximately 1-2GB" }
  ]
}
```

## User Interaction

### Display Format

Issues grouped by severity:
```
CRITICAL (1):
  • Disk /var is 94% full (2.1GB remaining)

WARNING (2):
  • Process 'node' using 78% memory (1.2GB)
  • Service 'nginx' restarted 3 times in last hour
```

Actions shown one at a time:
```
┌─────────────────────────────────────────────────┐
│ Action 1 of 2                          [low risk]
│
│ Issue: Disk /var is 94% full
│
│ Proposed: Clear journal logs older than 7 days
│ Command:  journalctl --vacuum-time=7d
│ Expected: Free approximately 1-2GB
└─────────────────────────────────────────────────┘

Execute this action? [y/n/q] █
```

### Approval Options

- `y` = Execute this action, show result, continue to next
- `n` = Skip this action, continue to next
- `q` = Quit, skip all remaining actions

## File Structure

Single file: `examples/50_HealthAgent.lune` (~300 lines)

Sections:
1. **Types** (~50 lines) - Data types and JSON decoders
2. **Health Checks** (~80 lines) - System inspection tools
3. **LLM Analysis** (~40 lines) - Nova integration
4. **Display** (~50 lines) - Terminal formatting
5. **Execution** (~60 lines) - Approval loop and command runner
6. **Main** (~20 lines) - Entry point

## Configuration

Environment variables (optional):
- `OLLAMA_BASE_URL` - defaults to localhost:11434
- `HEALTH_AGENT_MODEL` - defaults to llama3

## Usage

```bash
lune examples/50_HealthAgent.lune
```
