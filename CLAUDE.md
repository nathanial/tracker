# CLAUDE.md - Tracker

Local, git-friendly issue tracking with CLI and TUI interfaces. Issues are stored as markdown files with YAML frontmatter in `.issues/` directories.

## Build Commands

```bash
lake build           # Build the project
lake test            # Run test suite
lake exe tracker     # Run CLI
./install.sh         # Install to ~/.local/bin/
```

## Directory Structure

```
Tracker/
├── Core/
│   ├── Types.lean      # Priority, Status, Issue, ProgressEntry
│   ├── Storage.lean    # File I/O, filtering, dependency management
│   ├── Parser.lean     # YAML frontmatter + markdown parser (uses Sift)
│   └── Util.lean       # String utilities
├── CLI/
│   ├── Commands.lean   # Parlance command definitions
│   ├── Handlers.lean   # Command implementations
│   └── Output.lean     # JSON/text formatting
├── TUI/
│   ├── State.lean      # AppState, FormState, ViewMode
│   ├── Draw.lean       # Terminal rendering
│   ├── Update.lean     # Pure event handling
│   └── App.lean        # Main TUI loop (60 FPS)
└── Main.lean           # Entry point (CLI/TUI dispatch)
```

## Dependencies

- `terminus` - TUI framework with tree widgets
- `parlance` - CLI argument parsing
- `chronos` - Timestamp handling (ISO 8601)
- `sift` - Parser combinators
- `crucible` - Test framework

## CLI Commands

| Command | Purpose |
|---------|---------|
| `tracker init` | Initialize `.issues/` directory |
| `tracker add "Title" [--priority=X] [--project=X] [--label=X]` | Create issue |
| `tracker list [--all] [--status=X] [--project=X] [--blocked]` | List issues |
| `tracker search <query>` | Search issues by keyword |
| `tracker show <id>` | Show issue details |
| `tracker update <id> [--status=X] [--priority=X] [--title=X]` | Update issue |
| `tracker progress <id> "message"` | Add timestamped progress note |
| `tracker close <id> [comment]` | Close issue |
| `tracker reopen <id>` | Reopen closed issue |
| `tracker block <id> --by=<id>` | Mark blocking relationship |
| `tracker unblock <id> --by=<id>` | Remove blocking |
| `tracker deps <id>` | Show dependency graph |
| `tracker delete <id>` | Delete issue |
| `tracker tui [--debug]` | Launch interactive TUI |

**Flags:** `-j`/`--json` for JSON output (default is text)

## Debug Mode

The TUI supports a `--debug` flag for capturing frames:

```bash
tracker tui --debug
# Interact with TUI, then exit with Ctrl+Q
ls .debug/           # View captured frames
cat .debug/frame-000.txt  # Plain text output (no ANSI codes)
```

Only changed frames are written. This is useful for:
- Debugging rendering issues
- Creating reproducible test cases
- Verifying TUI behavior without visual inspection

## Data Model

**File format:** `<NNNN>-<slug>.md` in `.issues/` directory

```yaml
---
id: 1
title: Fix parsing bug
status: open                    # open | in-progress | closed
priority: high                  # low | medium | high | critical
created: 2026-01-06T10:00:00
updated: 2026-01-06T11:30:00
labels: [bug, parser]
assignee: claude                # nullable
project: tracker                # nullable
blocks: [3, 4]                  # issues this blocks
blocked_by: [1, 2]              # issues blocking this
---

## Description
Issue description here.

## Progress
- [2026-01-06T10:30:00] Started investigation
- [2026-01-06T11:00:00] Found root cause
```

## Architecture Notes

- **No central index:** Issues discovered by scanning `.issues/*.md` files
- **Effective blocking:** Issue only blocked if blockedBy points to open/in-progress issues
- **Bidirectional relationships:** `block A --by=B` updates both A.blockedBy and B.blocks
- **Pure TUI updates:** State machine returns (newState, pendingIOAction); IO processed in main loop
- **Tree view modes:** By project, by status, or by project+status (toggle with Tab)

## Key Types

```lean
-- Tracker/Core/Types.lean
inductive Priority | low | medium | high | critical
inductive Status | open | inProgress | closed

structure Issue where
  id : Nat
  title : String
  status : Status
  priority : Priority
  created : String           -- ISO 8601
  updated : String
  labels : List String
  assignee : Option String
  project : Option String
  description : String
  progress : List ProgressEntry
  blocks : List Nat
  blockedBy : List Nat
```

## Testing

Tests in `TrackerTests/Main.lean` using Crucible:
- Frontmatter parsing (labels, blocks, nullable values)
- Progress entry parsing
- Round-trip serialization
- Error cases
