/-
  Core types for the issue tracker.
-/

namespace Tracker

/-- Issue priority levels -/
inductive Priority where
  | low
  | medium
  | high
  | critical
  deriving Repr, BEq, Inhabited

namespace Priority

def toString : Priority → String
  | .low => "low"
  | .medium => "medium"
  | .high => "high"
  | .critical => "critical"

def fromString? : String → Option Priority
  | "low" => some .low
  | "medium" | "med" => some .medium
  | "high" => some .high
  | "critical" | "crit" => some .critical
  | _ => none

def toJson : Priority → String
  | p => s!"\"{p.toString}\""

def ord : Priority → Nat
  | .low => 0
  | .medium => 1
  | .high => 2
  | .critical => 3

instance : Ord Priority where
  compare a b := compare a.ord b.ord

end Priority

/-- Issue status -/
inductive Status where
  | open_
  | inProgress
  | closed
  deriving Repr, BEq, Inhabited

namespace Status

def toString : Status → String
  | .open_ => "open"
  | .inProgress => "in-progress"
  | .closed => "closed"

def fromString? : String → Option Status
  | "open" => some .open_
  | "in-progress" | "in_progress" | "inprogress" => some .inProgress
  | "closed" => some .closed
  | _ => none

def toJson : Status → String
  | s => s!"\"{s.toString}\""

def isOpen : Status → Bool
  | .open_ => true
  | .inProgress => true
  | .closed => false

end Status

/-- A progress entry (timestamped note) -/
structure ProgressEntry where
  timestamp : String  -- ISO 8601 format
  message : String
  deriving Repr, BEq, Inhabited

namespace ProgressEntry

def toJson (e : ProgressEntry) : String :=
  s!"\{\"timestamp\": \"{e.timestamp}\", \"message\": \"{escapeJson e.message}\"}"
where
  escapeJson (s : String) : String :=
    s.replace "\\" "\\\\"
      |>.replace "\"" "\\\""
      |>.replace "\n" "\\n"
      |>.replace "\r" "\\r"
      |>.replace "\t" "\\t"

end ProgressEntry

/-- An issue in the tracker -/
structure Issue where
  id : Nat
  title : String
  status : Status
  priority : Priority
  created : String      -- ISO 8601 timestamp
  updated : String      -- ISO 8601 timestamp
  labels : Array String
  assignee : Option String
  project : Option String -- Project this issue belongs to
  blocks : Array Nat    -- Issue IDs this issue blocks
  blockedBy : Array Nat -- Issue IDs blocking this one
  description : String
  progress : Array ProgressEntry
  deriving Repr, BEq, Inhabited

namespace Issue

/-- Left-pad a string to a minimum length -/
private def padLeft (s : String) (len : Nat) (c : Char) : String :=
  let padding := len - s.length
  if padding > 0 then
    String.mk (List.replicate padding c) ++ s
  else s

/-- Filter characters from a string -/
private def filterChars (s : String) (p : Char → Bool) : String :=
  String.mk (s.toList.filter p)

def isBlocked (issue : Issue) : Bool :=
  !issue.blockedBy.isEmpty

def isOpen (issue : Issue) : Bool :=
  issue.status.isOpen

def filename (issue : Issue) : String :=
  let paddedId := padLeft s!"{issue.id}" 4 '0'
  let slug := issue.title.toLower
    |>.replace " " "-"
    |> (filterChars · (fun c => c.isAlphanum || c == '-'))
    |>.take 40
  s!"{paddedId}-{slug}.md"

private def escapeJson (s : String) : String :=
  s.replace "\\" "\\\\"
    |>.replace "\"" "\\\""
    |>.replace "\n" "\\n"
    |>.replace "\r" "\\r"
    |>.replace "\t" "\\t"

def toJson (issue : Issue) : String :=
  let labelsJson := issue.labels.map (fun l => s!"\"{l}\"") |>.toList |> String.intercalate ", "
  let blocksJson := issue.blocks.map toString |>.toList |> String.intercalate ", "
  let blockedByJson := issue.blockedBy.map toString |>.toList |> String.intercalate ", "
  let assigneeJson := match issue.assignee with
    | some a => s!"\"{a}\""
    | none => "null"
  let projectJson := match issue.project with
    | some p => s!"\"{p}\""
    | none => "null"
  let progressJson := issue.progress.map ProgressEntry.toJson |>.toList |> String.intercalate ", "
  s!"\{
  \"id\": {issue.id},
  \"title\": \"{escapeJson issue.title}\",
  \"status\": {issue.status.toJson},
  \"priority\": {issue.priority.toJson},
  \"created\": \"{issue.created}\",
  \"updated\": \"{issue.updated}\",
  \"labels\": [{labelsJson}],
  \"assignee\": {assigneeJson},
  \"project\": {projectJson},
  \"blocks\": [{blocksJson}],
  \"blocked_by\": [{blockedByJson}],
  \"description\": \"{escapeJson issue.description}\",
  \"progress\": [{progressJson}]
}"

def toCompactJson (issue : Issue) : String :=
  let labelsJson := issue.labels.map (fun l => s!"\"{l}\"") |>.toList |> String.intercalate ","
  let blocksJson := issue.blocks.map toString |>.toList |> String.intercalate ","
  let blockedByJson := issue.blockedBy.map toString |>.toList |> String.intercalate ","
  let assigneeJson := match issue.assignee with
    | some a => s!"\"{a}\""
    | none => "null"
  let projectJson := match issue.project with
    | some p => s!"\"{p}\""
    | none => "null"
  s!"\{\"id\":{issue.id},\"title\":\"{escapeJson issue.title}\",\"status\":{issue.status.toJson},\"priority\":{issue.priority.toJson},\"created\":\"{issue.created}\",\"updated\":\"{issue.updated}\",\"labels\":[{labelsJson}],\"assignee\":{assigneeJson},\"project\":{projectJson},\"blocks\":[{blocksJson}],\"blocked_by\":[{blockedByJson}]}"

end Issue

/-- Summary of an issue for list views -/
structure IssueSummary where
  id : Nat
  title : String
  status : Status
  priority : Priority
  labels : Array String
  isBlocked : Bool
  deriving Repr, BEq

namespace IssueSummary

def fromIssue (issue : Issue) : IssueSummary :=
  { id := issue.id
  , title := issue.title
  , status := issue.status
  , priority := issue.priority
  , labels := issue.labels
  , isBlocked := issue.isBlocked
  }

private def escapeJson (s : String) : String :=
  s.replace "\\" "\\\\"
    |>.replace "\"" "\\\""
    |>.replace "\n" "\\n"

def toJson (summary : IssueSummary) : String :=
  let labelsJson := summary.labels.map (fun l => s!"\"{l}\"") |>.toList |> String.intercalate ", "
  s!"\{\"id\": {summary.id}, \"title\": \"{escapeJson summary.title}\", \"status\": {summary.status.toJson}, \"priority\": {summary.priority.toJson}, \"labels\": [{labelsJson}], \"blocked\": {summary.isBlocked}}"

end IssueSummary

end Tracker
