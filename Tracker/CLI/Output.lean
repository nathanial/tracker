/-
  Output formatting for CLI commands (JSON and text).
-/
import Tracker.Core.Types
import Tracker.Core.Storage

namespace Tracker.CLI.Output

open Tracker

/-- Left-pad a string to a minimum length -/
private def padLeft (s : String) (len : Nat) (c : Char) : String :=
  let padding := len - s.length
  if padding > 0 then
    String.mk (List.replicate padding c) ++ s
  else s

/-- Output mode -/
inductive Mode where
  | json
  | text
  deriving BEq, Inhabited

/-- Format a single issue for output -/
def formatIssue (issue : Issue) (mode : Mode) : String :=
  match mode with
  | .json => issue.toJson
  | .text =>
    let blockedStr := if issue.isBlocked then " [BLOCKED]" else ""
    let labelsStr := if issue.labels.isEmpty then "" else s!" ({String.intercalate ", " issue.labels.toList})"
    let assigneeStr := match issue.assignee with
      | some a => s!" @{a}"
      | none => ""
    let progressStr := if issue.progress.isEmpty then ""
      else "\n\n## Progress\n" ++ String.intercalate "\n" (issue.progress.map fun e =>
        s!"- [{e.timestamp}] {e.message}").toList
    s!"#{issue.id} [{issue.priority.toString.toUpper}] {issue.title}{blockedStr}{labelsStr}{assigneeStr}
Status: {issue.status.toString}
Created: {issue.created}
Updated: {issue.updated}

{issue.description}{progressStr}"

/-- Format an issue list for output -/
def formatIssueList (issues : Array Issue) (allIssues : Array Issue) (mode : Mode) : String :=
  match mode with
  | .json =>
    let items := issues.map (·.toCompactJson) |>.toList |> String.intercalate ",\n  "
    s!"[\n  {items}\n]"
  | .text =>
    if issues.isEmpty then "No issues found."
    else
      let header := s!"Found {issues.size} issue(s):\n"
      let rows := issues.map fun issue =>
        let blockedStr := if Storage.isEffectivelyBlocked issue allIssues then "[B]" else "   "
        let priorityStr := match issue.priority with
          | .critical => "CRIT"
          | .high => "HIGH"
          | .medium => "MED "
          | .low => "LOW "
        let statusStr := match issue.status with
          | .open_ => "open"
          | .inProgress => "prog"
          | .closed => "done"
        let labelsStr := if issue.labels.isEmpty then ""
          else s!" ({String.intercalate ", " issue.labels.toList})"
        let projectStr := match issue.project with
          | some p => s!" [{p}]"
          | none => ""
        s!"{blockedStr} #{padLeft (toString issue.id) 4 ' '} [{priorityStr}] [{statusStr}]{projectStr} {issue.title.take 40}{labelsStr}"
      header ++ String.intercalate "\n" rows.toList

/-- Format a success message -/
def formatSuccess (message : String) (mode : Mode) : String :=
  match mode with
  | .json => s!"\{\"status\": \"success\", \"message\": \"{message}\"}"
  | .text => message

/-- Escape a string for JSON output -/
private def escapeJson (s : String) : String :=
  s.replace "\\" "\\\\"
    |>.replace "\"" "\\\""
    |>.replace "\n" "\\n"

/-- Format an error message with optional actionable suggestion -/
def formatError (message : String) (mode : Mode) (suggestion : Option String := none) : String :=
  match mode with
  | .json =>
    let suggestionJson := match suggestion with
      | some s => s!", \"suggestion\": \"{escapeJson s}\""
      | none => ""
    s!"\{\"status\": \"error\", \"message\": \"{escapeJson message}\"{suggestionJson}}"
  | .text =>
    match suggestion with
    | some s => s!"Error: {message}\n  Hint: {s}"
    | none => s!"Error: {message}"

/-- Format dependency graph -/
def formatDeps (issue : Issue) (allIssues : Array Issue) (mode : Mode) : String :=
  match mode with
  | .json =>
    let blockedByJson := issue.blockedBy.map toString |>.toList |> String.intercalate ", "
    let blocksJson := issue.blocks.map toString |>.toList |> String.intercalate ", "
    s!"\{\"id\": {issue.id}, \"blocked_by\": [{blockedByJson}], \"blocks\": [{blocksJson}]}"
  | .text =>
    let findTitle (id : Nat) : String :=
      match allIssues.find? (·.id == id) with
      | some i => i.title.take 30
      | none => "(unknown)"
    let blockedByStr := if issue.blockedBy.isEmpty then "  (none)"
      else String.intercalate "\n" (issue.blockedBy.map fun id =>
        s!"  #{id}: {findTitle id}").toList
    let blocksStr := if issue.blocks.isEmpty then "  (none)"
      else String.intercalate "\n" (issue.blocks.map fun id =>
        s!"  #{id}: {findTitle id}").toList
    s!"Issue #{issue.id}: {issue.title}

Blocked by:
{blockedByStr}

Blocks:
{blocksStr}"

end Tracker.CLI.Output
