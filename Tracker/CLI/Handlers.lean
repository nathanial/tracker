/-
  CLI command handlers.
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Tracker.Core.Util
import Tracker.CLI.Output
import Parlance

namespace Tracker.CLI.Handlers

open Tracker
open Tracker.CLI.Output
open Parlance

/-- Result of command execution -/
inductive Result where
  | success (output : String)
  | error (message : String)
  | launchTui (debug : Bool := false)  -- Signal to launch TUI instead

/-- Get output mode from parse result -/
def getMode (result : ParseResult) : Mode :=
  if result.getBool "json" then .json else .text

/-- Find config by walking up directory tree -/
def findConfig : IO (Option Storage.Config) := do
  let cwd ← IO.currentDir
  match ← Storage.findIssuesRoot cwd with
  | some root => return some { root }
  | none => return none

/-- Require a valid config or return error -/
def requireConfig (mode : Mode) : IO (Except String Storage.Config) := do
  match ← findConfig with
  | some config => return .ok config
  | none => return .error (formatError "No .issues directory found. Run 'tracker init' first." mode)

/-- Handle 'init' command -/
def handleInit (mode : Mode) : IO Result := do
  let cwd ← IO.currentDir
  if ← Storage.hasIssuesDir cwd then
    return .error (formatError "Issues directory already exists" mode (some "Use 'tracker list' to see existing issues"))
  try
    Storage.initIssuesDir cwd
    return .success (formatSuccess s!"Initialized issue tracker in {cwd / Storage.issuesDirName}" mode)
  catch e =>
    return .error (formatError s!"Failed to initialize: {e}" mode)

/-- Handle 'add' command -/
def handleAdd (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    let title := result.getD "title" ""
    if title.isEmpty then
      return .error (formatError "Title is required" mode (some "Usage: tracker add \"Issue Title\""))
    let priority := result.getD "priority" "medium"
      |> Priority.fromString? |>.getD .medium
    let description := result.getD "description" ""
    let label := result.get (α := String) "label"
    let labels := match label with
      | some l => #[l]
      | none => #[]
    let assignee := result.get (α := String) "assignee"
    let project := result.get (α := String) "project"
    try
      let issue ← Storage.createIssue config title description priority labels assignee project
      return .success (formatIssue issue mode)
    catch e =>
      return .error (formatError s!"Failed to create issue: {e}" mode)

/-- Handle 'list' command -/
def handleList (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    let filter : Storage.ListFilter := {
      status := result.get (α := String) "status" >>= Status.fromString?
      label := result.get (α := String) "label"
      assignee := result.get (α := String) "assignee"
      project := result.get (α := String) "project"
      blockedOnly := result.getBool "blocked"
      includeAll := result.getBool "all"
    }
    try
      let allIssues ← Storage.loadAllIssues config
      let issues ← Storage.listIssues config filter
      return .success (formatIssueList issues allIssues mode)
    catch e =>
      return .error (formatError s!"Failed to list issues: {e}" mode)

/-- Handle 'search' command -/
def handleSearch (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    let query := Util.trim (result.getD "query" "")
    if query.isEmpty then
      return .error (formatError "Search query is required" mode (some "Usage: tracker search \"query\""))
    try
      let allIssues ← Storage.loadAllIssues config
      let issues := Storage.searchIssuesIn allIssues query
      return .success (formatIssueList issues allIssues mode)
    catch e =>
      return .error (formatError s!"Failed to search issues: {e}" mode)

/-- Handle 'show' command -/
def handleShow (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id" with
    | none => return .error (formatError "Issue ID is required" mode (some "Usage: tracker show <id>"))
    | some id =>
      try
        match ← Storage.findIssue config id with
        | some issue => return .success (formatIssue issue mode)
        | none => return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list' to see available issues"))
      catch e =>
        return .error (formatError s!"Failed to show issue: {e}" mode)

/-- Handle 'update' command -/
def handleUpdate (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id" with
    | none => return .error (formatError "Issue ID is required" mode (some "Usage: tracker update <id> [options]"))
    | some id =>
      try
        let statusOpt := result.get (α := String) "status" >>= Status.fromString?
        let priorityOpt := result.get (α := String) "priority" >>= Priority.fromString?
        let titleOpt := result.get (α := String) "title"
        let assigneeOpt := result.get (α := String) "assignee"
        let addLabel := result.get (α := String) "add-label"
        let removeLabel := result.get (α := String) "remove-label"

        let modifier := fun (issue : Issue) =>
          let issue := match statusOpt with
            | some s => { issue with status := s }
            | none => issue
          let issue := match priorityOpt with
            | some p => { issue with priority := p }
            | none => issue
          let issue := match titleOpt with
            | some t => { issue with title := t }
            | none => issue
          let issue := match assigneeOpt with
            | some a => { issue with assignee := some a }
            | none => issue
          let issue := match addLabel with
            | some l =>
              if issue.labels.contains l then issue
              else { issue with labels := issue.labels.push l }
            | none => issue
          let issue := match removeLabel with
            | some l => { issue with labels := issue.labels.filter (· != l) }
            | none => issue
          issue

        match ← Storage.updateIssue config id modifier with
        | some issue => return .success (formatIssue issue mode)
        | none => return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list' to see available issues"))
      catch e =>
        return .error (formatError s!"Failed to update issue: {e}" mode)

/-- Handle 'progress' command -/
def handleProgress (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id" with
    | none => return .error (formatError "Issue ID is required" mode (some "Usage: tracker progress <id> \"message\""))
    | some id =>
      let message := result.getD "message" ""
      if message.isEmpty then
        return .error (formatError "Progress message is required" mode (some "Usage: tracker progress <id> \"message\""))
      try
        match ← Storage.addProgress config id message with
        | some issue => return .success (formatIssue issue mode)
        | none => return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list' to see available issues"))
      catch e =>
        return .error (formatError s!"Failed to add progress: {e}" mode)

/-- Handle 'close' command -/
def handleClose (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id" with
    | none => return .error (formatError "Issue ID is required" mode (some "Usage: tracker close <id> [comment]"))
    | some id =>
      let comment := result.get (α := String) "comment"
      try
        match ← Storage.closeIssue config id comment with
        | some issue => return .success (formatIssue issue mode)
        | none => return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list' to see available issues"))
      catch e =>
        return .error (formatError s!"Failed to close issue: {e}" mode)

/-- Handle 'reopen' command -/
def handleReopen (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id" with
    | none => return .error (formatError "Issue ID is required" mode (some "Usage: tracker reopen <id>"))
    | some id =>
      try
        match ← Storage.reopenIssue config id with
        | some issue => return .success (formatIssue issue mode)
        | none => return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list --all' to see all issues including closed"))
      catch e =>
        return .error (formatError s!"Failed to reopen issue: {e}" mode)

/-- Handle 'block' command -/
def handleBlock (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id", result.getNat "by" with
    | some id, some blockedById =>
      if id == blockedById then
        return .error (formatError "An issue cannot block itself" mode (some "Use a different issue ID for --by"))
      try
        match ← Storage.addBlockedBy config id blockedById with
        | some issue => return .success (formatIssue issue mode)
        | none => return .error (formatError s!"Issue #{id} or #{blockedById} not found" mode (some "Use 'tracker list' to see available issues"))
      catch e =>
        return .error (formatError s!"Failed to add dependency: {e}" mode)
    | none, _ => return .error (formatError "Issue ID is required" mode (some "Usage: tracker block <id> --by=<blocking-id>"))
    | _, none => return .error (formatError "--by flag is required" mode (some "Usage: tracker block <id> --by=<blocking-id>"))

/-- Handle 'unblock' command -/
def handleUnblock (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id", result.getNat "by" with
    | some id, some blockedById =>
      try
        match ← Storage.removeBlockedBy config id blockedById with
        | some issue => return .success (formatIssue issue mode)
        | none => return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list' to see available issues"))
      catch e =>
        return .error (formatError s!"Failed to remove dependency: {e}" mode)
    | none, _ => return .error (formatError "Issue ID is required" mode (some "Usage: tracker unblock <id> --by=<blocking-id>"))
    | _, none => return .error (formatError "--by flag is required" mode (some "Usage: tracker unblock <id> --by=<blocking-id>"))

/-- Handle 'deps' command -/
def handleDeps (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id" with
    | none => return .error (formatError "Issue ID is required" mode (some "Usage: tracker deps <id>"))
    | some id =>
      try
        let allIssues ← Storage.loadAllIssues config
        match allIssues.find? (·.id == id) with
        | some issue => return .success (formatDeps issue allIssues mode)
        | none => return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list' to see available issues"))
      catch e =>
        return .error (formatError s!"Failed to show dependencies: {e}" mode)

/-- Handle 'delete' command -/
def handleDelete (result : ParseResult) (mode : Mode) : IO Result := do
  match ← requireConfig mode with
  | .error msg => return .error msg
  | .ok config =>
    match result.getNat "id" with
    | none => return .error (formatError "Issue ID is required" mode (some "Usage: tracker delete <id>"))
    | some id =>
      try
        if ← Storage.deleteIssue config id then
          return .success (formatSuccess s!"Deleted issue #{id}" mode)
        else
          return .error (formatError s!"Issue #{id} not found" mode (some "Use 'tracker list --all' to see all issues"))
      catch e =>
        return .error (formatError s!"Failed to delete issue: {e}" mode)

/-- Dispatch to the appropriate handler based on command path -/
def dispatch (parseResult : ParseResult) : IO Result := do
  let mode := getMode parseResult
  match parseResult.commandPath with
  | ["init"] => handleInit mode
  | ["add"] => handleAdd parseResult mode
  | ["list"] => handleList parseResult mode
  | ["search"] => handleSearch parseResult mode
  | ["show"] => handleShow parseResult mode
  | ["update"] => handleUpdate parseResult mode
  | ["progress"] => handleProgress parseResult mode
  | ["close"] => handleClose parseResult mode
  | ["reopen"] => handleReopen parseResult mode
  | ["block"] => handleBlock parseResult mode
  | ["unblock"] => handleUnblock parseResult mode
  | ["deps"] => handleDeps parseResult mode
  | ["delete"] => handleDelete parseResult mode
  | ["tui"] => return .launchTui (parseResult.getBool "debug")
  | [] => return .launchTui  -- No subcommand = launch TUI
  | path => return .error (formatError s!"Unknown command: {String.intercalate " " path}" mode (some "Run 'tracker --help' for available commands"))

end Tracker.CLI.Handlers
