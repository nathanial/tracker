/-
  Tracker - Main entry point.

  Dispatches between CLI mode (with arguments) and TUI mode (no arguments).
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Tracker.CLI.Commands
import Tracker.CLI.Handlers
import Tracker.TUI.App
import Parlance

namespace Tracker

open Tracker.CLI
open Parlance

/-- Print help message using Parlance's auto-generation -/
def printHelp : IO Unit :=
  trackerCommand.printHelp

/-- Launch TUI mode -/
def launchTui (debug : Bool := false) : IO Unit := do
  let cwd ← IO.currentDir
  match ← Storage.findIssuesRoot cwd with
  | some root =>
    TUI.run { root } debug
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
    | .launchTui debug =>
      launchTui debug
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
