# Tracker

A local, git-friendly issue tracker with dual interfaces:
- **CLI mode**: Programmatic commands with JSON output (designed for Claude Code)
- **TUI mode**: Interactive terminal UI for humans

Issues are stored as markdown files in your repo - human-readable, git-diffable, and machine-parseable.

## Installation

Requires [Lean 4](https://lean-lang.org/) and [Lake](https://github.com/leanprover/lake).

```bash
git clone https://github.com/nathanial/tracker.git
cd tracker
./install.sh
```

This builds the project and installs the `tracker` binary to `~/.local/bin/`.

To install to a different location:
```bash
INSTALL_DIR=/usr/local/bin ./install.sh
```

Make sure the install directory is in your `PATH`.

## Quick Start

```bash
# Initialize issue tracking in your project
tracker init

# Create issues
tracker add "Fix parsing bug" --priority=high --label=bug
tracker add "Add new feature" --priority=medium

# List issues (JSON by default)
tracker list

# List in human-readable format
tracker -t list

# Add progress notes
tracker progress 1 "Found root cause in tokenizer"

# Search issues
tracker search "tokenizer"

# Close an issue
tracker close 1 "Fixed in commit abc123"

# Launch interactive TUI
tracker
```

## CLI Commands

All commands output JSON by default. Add `-t` or `--text` before the subcommand for human-readable output.

| Command | Description |
|---------|-------------|
| `tracker init` | Initialize `.issues/` directory |
| `tracker add <title>` | Create a new issue |
| `tracker list` | List issues (with filters) |
| `tracker search <query>` | Search issues by keyword |
| `tracker show <id>` | Show issue details |
| `tracker update <id>` | Update issue metadata |
| `tracker progress <id> <msg>` | Add progress note |
| `tracker close <id> [comment]` | Close an issue |
| `tracker reopen <id>` | Reopen a closed issue |
| `tracker block <id> --by=<id>` | Add dependency |
| `tracker unblock <id> --by=<id>` | Remove dependency |
| `tracker deps <id>` | Show dependency graph |
| `tracker delete <id>` | Delete an issue |
| `tracker tui` | Launch interactive TUI |

### Options

- `--priority`, `-p`: low, medium, high, critical
- `--label`, `-l`: Add labels
- `--assignee`, `-a`: Assign to someone
- `--status`, `-s`: open, in-progress, closed
- `--all`: Include closed issues in list
- `--blocked`: Show only blocked issues

## Issue Format

Issues are stored as markdown files with YAML frontmatter:

```markdown
---
id: 1
title: Fix parsing bug
status: open
priority: high
created: 2026-01-06T10:00:00
updated: 2026-01-06T11:30:00
labels: [bug, parser]
assignee: claude
blocks: []
blocked_by: [3]
---

# Fix parsing bug

## Description
The parser fails on edge case X.

## Progress
- [2026-01-06 10:30] Started investigation
- [2026-01-06 11:00] Found root cause in tokenizer
```

## TUI Mode

Launch with `tracker` (no arguments) or `tracker tui`.

**Navigation:**
- `j`/`k` or arrows: Move selection
- `Tab`/`Shift+Tab`: Switch filter tabs
- `Enter`: View issue details
- `Esc`: Back to list
- `c`: Close issue
- `o`: Reopen issue
- `r`: Refresh
- `q`: Quit

## Dependencies

- [terminus](https://github.com/nathanial/terminus) - TUI framework
- [parlance](https://github.com/nathanial/parlance) - CLI argument parsing
- [chronos](https://github.com/nathanial/chronos-lean) - Timestamps

## License

MIT License
