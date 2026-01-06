/-
  Tracker - Main entry point.

  Dispatches between CLI mode (with arguments) and TUI mode (no arguments).
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Tracker.CLI.Commands
import Tracker.CLI.Handlers
import Tracker.TUI
import Parlance

namespace Tracker

open Tracker.CLI
open Parlance

/-- Print help message -/
def printHelp : IO Unit := do
  IO.println "Tracker - Local issue tracker for Claude Code"
  IO.println ""
  IO.println "Usage:"
  IO.println "  tracker                    Launch interactive TUI"
  IO.println "  tracker <command> [args]   Run CLI command"
  IO.println ""
  IO.println "Commands:"
  IO.println "  init                       Initialize .issues directory"
  IO.println "  add <title>                Create a new issue"
  IO.println "  list                       List issues"
  IO.println "  show <id>                  Show issue details"
  IO.println "  update <id>                Update an issue"
  IO.println "  progress <id> <message>    Add progress note"
  IO.println "  close <id> [comment]       Close an issue"
  IO.println "  reopen <id>                Reopen a closed issue"
  IO.println "  block <id> --by=<id>       Add dependency"
  IO.println "  unblock <id> --by=<id>     Remove dependency"
  IO.println "  deps <id>                  Show dependency graph"
  IO.println "  delete <id>                Delete an issue"
  IO.println "  tui                        Launch interactive TUI"
  IO.println ""
  IO.println "Options:"
  IO.println "  -t, --text                 Output in text format (default: JSON)"
  IO.println "  -h, --help                 Show this help"
  IO.println "  -V, --version              Show version"

/-- Launch TUI mode -/
def launchTui : IO Unit := do
  let cwd ← IO.currentDir
  match ← Storage.findIssuesRoot cwd with
  | some root =>
    TUI.run { root }
  | none =>
    IO.eprintln "Error: No .issues directory found."
    IO.eprintln "Run 'tracker init' to initialize issue tracking."
    IO.Process.exit 1

/-- Main entry point -/
def main (args : List String) : IO UInt32 := do
  -- No args = TUI mode
  if args.isEmpty then
    launchTui
    return 0

  -- Check for help/version before parsing
  if args.contains "--help" || args.contains "-h" then
    printHelp
    return 0

  if args.contains "--version" || args.contains "-V" then
    IO.println "tracker 0.1.0"
    return 0

  -- Parse command
  match parse trackerCommand args with
  | .ok result =>
    let outcome ← Handlers.dispatch result
    match outcome with
    | .success output =>
      IO.println output
      return 0
    | .error message =>
      IO.eprintln message
      return 1
    | .launchTui =>
      launchTui
      return 0

  | .error .helpRequested =>
    printHelp
    return 0

  | .error .versionRequested =>
    IO.println "tracker 0.1.0"
    return 0

  | .error e =>
    IO.eprintln s!"Error: {e}"
    IO.eprintln "Run 'tracker --help' for usage."
    return 1

end Tracker
