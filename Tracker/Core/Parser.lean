/-
  YAML frontmatter and markdown body parser for issues.
  Built on the Sift parser combinator library.
-/
import Tracker.Core.Types
import Sift

namespace Tracker.Parser

open Sift

/-! ### Data Structures -/

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

/-! ### Primitive Parsers -/

/-- Skip horizontal whitespace only (space/tab) -/
private def skipHWS : Parser Unit Unit := hspaces

/-- Parse rest of line up to newline (not including it) -/
private def restOfLine : Parser Unit String :=
  takeWhile (fun c => c != '\n' && c != '\r')

/-- Skip to end of line including the newline -/
private def skipLine : Parser Unit Unit := do
  let _ ← restOfLine
  let _ ← Sift.optional eol

/-! ### YAML Primitive Parsers -/

/-- Parse a YAML key (alphanumeric, underscore, hyphen) -/
private def yamlKey : Parser Unit String :=
  takeWhile1 (fun c => c.isAlphanum || c == '_' || c == '-') <?> "YAML key"

/-- Parse a quoted string (single or double quotes) -/
private def quotedString : Parser Unit String := do
  let quote ← char '"' <|> char '\''
  let content ← takeWhile (· != quote)
  let _ ← char quote
  pure content

/-- Strip surrounding quotes from a string if present -/
private def stripQuotes (s : String) : String :=
  if (s.startsWith "\"" && s.endsWith "\"") ||
     (s.startsWith "'" && s.endsWith "'") then
    s.drop 1 |>.dropRight 1
  else s

/-- Parse "null" or empty as None, otherwise Some value -/
private def parseNullable (s : String) : Option String :=
  let s := stripQuotes s.trim
  if s.isEmpty || s == "null" then none else some s

/-- Parse a YAML string array: [item1, item2, ...] -/
private partial def yamlStringArray : Parser Unit (Array String) := do
  let _ ← char '['
  skipHWS
  if (← peek) == some ']' then
    let _ ← char ']'
    return #[]
  let mut items : Array String := #[]
  repeat do
    skipHWS
    let item ← match ← peek with
      | some '"' | some '\'' => quotedString
      | _ => takeWhile1 (fun c => c != ',' && c != ']' && c != '\n')
    items := items.push item.trim
    skipHWS
    match ← peek with
    | some ',' => let _ ← char ','
    | some ']' => break
    | _ => Parser.fail "expected ',' or ']' in array"
  let _ ← char ']'
  return items

/-- Parse a YAML numeric array: [1, 2, 3] -/
private partial def yamlNatArray : Parser Unit (Array Nat) := do
  let _ ← char '['
  skipHWS
  if (← peek) == some ']' then
    let _ ← char ']'
    return #[]
  let mut items : Array Nat := #[]
  repeat do
    skipHWS
    let n ← natural
    items := items.push n
    skipHWS
    match ← peek with
    | some ',' => let _ ← char ','
    | some ']' => break
    | _ => Parser.fail "expected ',' or ']' in array"
  let _ ← char ']'
  return items

/-! ### Frontmatter Parser -/

/-- Parse the --- delimiter line -/
private def frontmatterDelimiter : Parser Unit Unit := do
  let _ ← string "---"
  skipHWS
  let _ ← eol <|> eof

/-- Parse a single key-value line in frontmatter -/
private def keyValueLine : Parser Unit (String × String) := do
  let key ← yamlKey
  skipHWS
  let _ ← char ':'
  skipHWS
  let value ← restOfLine
  let _ ← Sift.optional eol
  pure (key, value.trim)

/-- Parse all frontmatter key-value pairs -/
private partial def frontmatterLines : Parser Unit (Array (String × String)) := do
  let mut pairs : Array (String × String) := #[]
  while true do
    -- Check for closing delimiter
    match ← peekString 3 with
    | some "---" => break
    | _ => pure ()
    -- Check for end of input
    if ← atEnd then
      Parser.fail "unclosed frontmatter (expected closing ---)"
    -- Parse key-value or skip empty/comment lines
    skipHWS
    match ← peek with
    | some '\n' | some '\r' =>
      let _ ← eol
    | some '#' =>
      skipLine
    | some _ =>
      let pair ← keyValueLine
      pairs := pairs.push pair
    | none => break
  pure pairs

/-- Build FrontmatterData from parsed key-value pairs -/
private def buildFrontmatterData (pairs : Array (String × String)) : FrontmatterData := Id.run do
  let mut data : FrontmatterData := {}
  for (key, value) in pairs do
    match key with
    | "id" => data := { data with id := value.toNat? }
    | "title" =>
      let v := stripQuotes value
      data := { data with title := some v }
    | "status" => data := { data with status := some value }
    | "priority" => data := { data with priority := some value }
    | "created" => data := { data with created := some value }
    | "updated" => data := { data with updated := some value }
    | "labels" =>
      match Parser.run yamlStringArray value with
      | .ok arr => data := { data with labels := arr }
      | .error _ => data := { data with labels := #[] }
    | "assignee" => data := { data with assignee := parseNullable value }
    | "project" => data := { data with project := parseNullable value }
    | "blocks" =>
      match Parser.run yamlNatArray value with
      | .ok arr => data := { data with blocks := arr }
      | .error _ => data := { data with blocks := #[] }
    | "blocked_by" | "blockedBy" =>
      match Parser.run yamlNatArray value with
      | .ok arr => data := { data with blockedBy := arr }
      | .error _ => data := { data with blockedBy := #[] }
    | _ => pure ()  -- Ignore unknown keys
  data

/-- Parse complete frontmatter section -/
private def parseFrontmatterSection : Parser Unit FrontmatterData := do
  frontmatterDelimiter
  let pairs ← frontmatterLines
  frontmatterDelimiter
  pure (buildFrontmatterData pairs)

/-! ### Markdown Body Parser -/

/-- Parse a progress entry: - [TIMESTAMP] Message -/
private def progressEntry : Parser Unit ProgressEntry := do
  skipHWS
  let _ ← string "- ["
  let timestamp ← takeWhile (· != ']')
  let _ ← char ']'
  skipHWS
  let message ← restOfLine
  pure { timestamp := timestamp.trim, message := message.trim }

/-- Parse the Progress section -/
private partial def parseProgressSection : Parser Unit (Array ProgressEntry) := do
  let mut entries : Array ProgressEntry := #[]
  while true do
    if ← atEnd then break
    skipHWS
    match ← peek with
    | some '-' =>
      -- Check if this is a progress entry
      match ← peekString 3 with
      | some "- [" =>
        let entry ← progressEntry
        entries := entries.push entry
        let _ ← Sift.optional eol
      | _ => break
    | some '#' => break
    | some '\n' | some '\r' =>
      let _ ← eol
    | _ => break
  pure entries

/-- Parse the markdown body (description and progress) -/
private partial def parseBody : Parser Unit (String × Array ProgressEntry) := do
  let mut descLines : Array String := #[]
  let mut progress : Array ProgressEntry := #[]
  let mut inDescription := false
  let mut pastTitle := false

  while true do
    if ← atEnd then break

    -- Peek at line start
    skipHWS
    match ← peekString 2 with
    | some "# " =>
      if !pastTitle then
        -- Skip title line
        pastTitle := true
        skipLine
      else
        -- Some other heading in description
        let line ← restOfLine
        descLines := descLines.push line
        let _ ← Sift.optional eol
    | some "##" =>
      let line ← restOfLine
      let trimmed := line.trim
      if trimmed.startsWith "## Description" then
        inDescription := true
        let _ ← Sift.optional eol
      else if trimmed.startsWith "## Progress" || trimmed.startsWith "## progress" then
        inDescription := false
        let _ ← Sift.optional eol
        progress ← parseProgressSection
      else if trimmed.startsWith "## " then
        -- Unknown section, stop description
        inDescription := false
        let _ ← Sift.optional eol
      else
        if inDescription then
          descLines := descLines.push line
        let _ ← Sift.optional eol
    | _ =>
      if inDescription then
        let line ← restOfLine
        descLines := descLines.push line
        let _ ← Sift.optional eol
      else if !pastTitle then
        skipLine
      else
        -- Content before first ## section goes to description
        let line ← restOfLine
        descLines := descLines.push line
        let _ ← Sift.optional eol

  let description := String.intercalate "\n" descLines.toList |>.trim
  pure (description, progress)

/-! ### Main Parser -/

/-- Parse a complete issue file -/
private def parseIssueP : Parser Unit ParsedIssue := do
  let frontmatter ← parseFrontmatterSection
  let (description, progress) ← parseBody
  pure { frontmatter, description, progress }

/-- Parse a full issue file (public API) -/
def parseIssueFile (content : String) : Except String ParsedIssue :=
  match Parser.run parseIssueP content with
  | .ok result => .ok result
  | .error e => .error s!"Parse error at line {e.pos.line}, column {e.pos.column}: {e.message}"

/-! ### Conversion Functions -/

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
