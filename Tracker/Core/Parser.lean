/-
  YAML frontmatter and markdown body parser for issues.
-/
import Tracker.Core.Types

namespace Tracker.Parser

/-- Find the index of the first character matching a predicate -/
private def stringFindIdx? (s : String) (p : Char → Bool) : Option Nat :=
  let rec go (chars : List Char) (idx : Nat) : Option Nat :=
    match chars with
    | [] => none
    | c :: rest => if p c then some idx else go rest (idx + 1)
  go s.toList 0

/-- Parse result for frontmatter -/
structure FrontmatterData where
  id : Option Nat := none
  title : Option String := none
  status : Option String := none
  priority : Option String := none
  created : Option String := none
  updated : Option String := none
  labels : Array String := #[]
  assignee : Option String := none
  project : Option String := none
  blocks : Array Nat := #[]
  blockedBy : Array Nat := #[]
  deriving Repr, Inhabited

/-- Parse result for the full issue file -/
structure ParsedIssue where
  frontmatter : FrontmatterData
  description : String
  progress : Array ProgressEntry
  deriving Repr, Inhabited

/-- Split content into frontmatter and body -/
def splitFrontmatter (content : String) : Option (String × String) := do
  let lines := content.splitOn "\n"
  -- Must start with ---
  guard (lines.head? == some "---")
  let rest := lines.drop 1
  -- Find closing ---
  let idx? := rest.findIdx? (· == "---")
  let idx ← idx?
  let frontmatterLines := rest.take idx
  let bodyLines := rest.drop (idx + 1)
  let frontmatter := String.intercalate "\n" frontmatterLines
  let body := String.intercalate "\n" bodyLines
  pure (frontmatter, body)

/-- Trim whitespace from both ends of a string -/
def trim (s : String) : String :=
  s.dropWhile Char.isWhitespace |>.dropRightWhile Char.isWhitespace

/-- Parse a YAML-style array like [a, b, c] or ["a", "b"] -/
def parseStringArray (s : String) : Array String :=
  let s := trim s
  if !s.startsWith "[" || !s.endsWith "]" then #[]
  else
    let inner := s.drop 1 |>.dropRight 1 |> trim
    if inner.isEmpty then #[]
    else
      let parts := inner.splitOn ","
      parts.toArray.map fun p =>
        let p := trim p
        -- Remove quotes if present
        if p.startsWith "\"" && p.endsWith "\"" then
          p.drop 1 |>.dropRight 1
        else if p.startsWith "'" && p.endsWith "'" then
          p.drop 1 |>.dropRight 1
        else p

/-- Parse a YAML-style array of numbers -/
def parseNatArray (s : String) : Array Nat :=
  let strings := parseStringArray s
  strings.filterMap String.toNat?

/-- Parse a key-value line (key: value) -/
def parseKeyValue (line : String) : Option (String × String) := do
  let idx ← stringFindIdx? line (· == ':')
  let key := trim (line.take idx)
  let value := trim (line.drop (idx + 1))
  guard (!key.isEmpty)
  pure (key, value)

/-- Parse frontmatter section into structured data -/
def parseFrontmatter (content : String) : FrontmatterData := Id.run do
  let mut data : FrontmatterData := {}
  for line in content.splitOn "\n" do
    if let some (key, value) := parseKeyValue line then
      match key with
      | "id" => data := { data with id := value.toNat? }
      | "title" =>
        -- Remove surrounding quotes if present
        let v := if value.startsWith "\"" && value.endsWith "\"" then
          value.drop 1 |>.dropRight 1
        else value
        data := { data with title := some v }
      | "status" => data := { data with status := some value }
      | "priority" => data := { data with priority := some value }
      | "created" => data := { data with created := some value }
      | "updated" => data := { data with updated := some value }
      | "labels" => data := { data with labels := parseStringArray value }
      | "assignee" =>
        if value == "null" || value.isEmpty then
          data := { data with assignee := none }
        else
          let v := if value.startsWith "\"" && value.endsWith "\"" then
            value.drop 1 |>.dropRight 1
          else value
          data := { data with assignee := some v }
      | "project" =>
        if value == "null" || value.isEmpty then
          data := { data with project := none }
        else
          let v := if value.startsWith "\"" && value.endsWith "\"" then
            value.drop 1 |>.dropRight 1
          else value
          data := { data with project := some v }
      | "blocks" => data := { data with blocks := parseNatArray value }
      | "blocked_by" | "blockedBy" => data := { data with blockedBy := parseNatArray value }
      | _ => pure ()  -- Ignore unknown keys
  data

/-- Parse progress entries from markdown body -/
def parseProgress (body : String) : Array ProgressEntry := Id.run do
  let mut entries : Array ProgressEntry := #[]
  let mut inProgress := false
  for line in body.splitOn "\n" do
    let trimmed := trim line
    -- Check for Progress section header
    if trimmed.startsWith "## Progress" || trimmed.startsWith "## progress" then
      inProgress := true
    else if trimmed.startsWith "## " then
      inProgress := false
    else if inProgress && trimmed.startsWith "- [" then
      -- Parse progress entry: - [2026-01-06 10:30] Message
      let rest := trimmed.drop 3  -- Remove "- ["
      if let some endIdx := stringFindIdx? rest (· == ']') then
        let timestamp := rest.take endIdx
        let message := trim (rest.drop (endIdx + 1))
        entries := entries.push { timestamp, message }
  entries

/-- Extract description from markdown body (everything before ## Progress or first ## section) -/
def parseDescription (body : String) : String := Id.run do
  let mut lines : Array String := #[]
  let mut inDescription := false
  let mut foundFirstHeading := false
  for line in body.splitOn "\n" do
    let trimmed := trim line
    -- Skip the title heading (# Title)
    if trimmed.startsWith "# " && !foundFirstHeading then
      foundFirstHeading := true
    else if trimmed.startsWith "## Description" then
      inDescription := true
    else if trimmed.startsWith "## " && inDescription then
      -- End of description section
      break
    else if inDescription then
      lines := lines.push line
  if lines.isEmpty then
    -- No explicit description section, take everything before ## Progress
    let mut result : Array String := #[]
    let mut pastTitle := false
    for line in body.splitOn "\n" do
      let trimmed := trim line
      if trimmed.startsWith "# " && !pastTitle then
        pastTitle := true
      else if trimmed.startsWith "## Progress" then
        break
      else if pastTitle then
        result := result.push line
    trim (String.intercalate "\n" result.toList)
  else
    trim (String.intercalate "\n" lines.toList)

/-- Parse a full issue file -/
def parseIssueFile (content : String) : Option ParsedIssue := do
  let (frontmatterStr, body) ← splitFrontmatter content
  let frontmatter := parseFrontmatter frontmatterStr
  let description := parseDescription body
  let progress := parseProgress body
  pure { frontmatter, description, progress }

/-- Convert parsed data to an Issue (with defaults for missing fields) -/
def toIssue (parsed : ParsedIssue) (defaultId : Nat) (defaultTimestamp : String) : Issue :=
  { id := parsed.frontmatter.id.getD defaultId
  , title := parsed.frontmatter.title.getD "Untitled"
  , status := parsed.frontmatter.status.bind Status.fromString? |>.getD .open_
  , priority := parsed.frontmatter.priority.bind Priority.fromString? |>.getD .medium
  , created := parsed.frontmatter.created.getD defaultTimestamp
  , updated := parsed.frontmatter.updated.getD defaultTimestamp
  , labels := parsed.frontmatter.labels
  , assignee := parsed.frontmatter.assignee
  , project := parsed.frontmatter.project
  , blocks := parsed.frontmatter.blocks
  , blockedBy := parsed.frontmatter.blockedBy
  , description := parsed.description
  , progress := parsed.progress
  }

/-- Generate markdown content from an issue -/
def issueToMarkdown (issue : Issue) : String :=
  let labelsStr := "[" ++ String.intercalate ", " issue.labels.toList ++ "]"
  let blocksStr := "[" ++ String.intercalate ", " (issue.blocks.map toString).toList ++ "]"
  let blockedByStr := "[" ++ String.intercalate ", " (issue.blockedBy.map toString).toList ++ "]"
  let assigneeStr := match issue.assignee with
    | some a => a
    | none => ""
  let projectStr := match issue.project with
    | some p => p
    | none => ""
  let progressLines := issue.progress.map fun e =>
    s!"- [{e.timestamp}] {e.message}"
  let progressSection := if progressLines.isEmpty then ""
    else "\n## Progress\n" ++ String.intercalate "\n" progressLines.toList
  s!"---
id: {issue.id}
title: {issue.title}
status: {issue.status.toString}
priority: {issue.priority.toString}
created: {issue.created}
updated: {issue.updated}
labels: {labelsStr}
assignee: {assigneeStr}
project: {projectStr}
blocks: {blocksStr}
blocked_by: {blockedByStr}
---

# {issue.title}

## Description
{issue.description}
{progressSection}
"

end Tracker.Parser
