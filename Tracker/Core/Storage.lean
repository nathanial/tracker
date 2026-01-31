/-
  File I/O and index management for the issue tracker.
-/
import Tracker.Core.Types
import Tracker.Core.Parser
import Tracker.Core.Util
import Chronos

namespace Tracker.Storage

/-- Configuration for the tracker -/
structure Config where
  /-- Root directory for .issues folder -/
  root : System.FilePath
  deriving Repr, Inhabited

/-- The default issues directory name -/
def issuesDirName : String := ".issues"

/-- Get the full path to the issues directory -/
def issuesDir (config : Config) : System.FilePath :=
  config.root / issuesDirName

/-- Check if a directory contains a .issues folder -/
def hasIssuesDir (dir : System.FilePath) : IO Bool := do
  let path := dir / issuesDirName
  path.isDir

/-- Walk up the directory tree to find the nearest .issues folder -/
partial def findIssuesRoot (startDir : System.FilePath) : IO (Option System.FilePath) := do
  if ← hasIssuesDir startDir then
    return some startDir
  else
    let parent := startDir.parent
    match parent with
    | some p =>
      if p == startDir then return none  -- Reached filesystem root
      else findIssuesRoot p
    | none => return none

/-- Initialize a new issues directory -/
def initIssuesDir (root : System.FilePath) : IO Unit := do
  let dir := root / issuesDirName
  if ← dir.isDir then
    throw (IO.userError s!"Issues directory already exists: {dir}")
  IO.FS.createDirAll dir
  -- Create empty index file
  IO.FS.writeFile (dir / "index.jsonl") ""

/-- Get current UTC timestamp as ISO 8601 string -/
def nowIso8601 : IO String := do
  let dt ← Chronos.nowUtc
  return dt.toIso8601

/-- List all issue files in the issues directory -/
def listIssueFiles (config : Config) : IO (Array System.FilePath) := do
  let dir := issuesDir config
  if !(← dir.isDir) then
    return #[]
  let entries ← dir.readDir
  let files := entries.filter fun e =>
    e.fileName.endsWith ".md" && e.fileName != "README.md"
  return files.map (dir / ·.fileName)

/-- Read and parse a single issue file -/
def readIssue (path : System.FilePath) : IO (Option Issue) := do
  if !(← path.pathExists) then
    return none
  let content ← IO.FS.readFile path
  match Parser.parseIssueFile content with
  | .ok parsed =>
    let timestamp ← nowIso8601
    return some (Parser.toIssue parsed 0 timestamp)
  | .error e =>
    IO.eprintln s!"Warning: Failed to parse {path}: {e}"
    return none

/-- Write an issue to its file -/
def writeIssue (config : Config) (issue : Issue) : IO Unit := do
  let dir := issuesDir config
  let path := dir / issue.filename
  let content := Parser.issueToMarkdown issue
  IO.FS.writeFile path content

/-- Get the next available issue ID by scanning existing issues -/
def nextIssueId (config : Config) : IO Nat := do
  let files ← listIssueFiles config
  let mut maxId : Nat := 0
  for file in files do
    if let some issue ← readIssue file then
      maxId := max maxId issue.id
  return maxId + 1

/-- Load all issues from the issues directory -/
def loadAllIssues (config : Config) : IO (Array Issue) := do
  let files ← listIssueFiles config
  let mut issues : Array Issue := #[]
  for file in files do
    if let some issue ← readIssue file then
      issues := issues.push issue
  -- Sort by ID
  return issues.qsort (fun a b => a.id < b.id)

/-- Find an issue by ID -/
def findIssue (config : Config) (id : Nat) : IO (Option Issue) := do
  let issues ← loadAllIssues config
  return issues.find? (·.id == id)

/-- Find an issue file path by ID -/
def findIssuePath (config : Config) (id : Nat) : IO (Option System.FilePath) := do
  let files ← listIssueFiles config
  for file in files do
    if let some issue ← readIssue file then
      if issue.id == id then
        return some file
  return none

/-- Update an existing issue (reads, modifies, writes) -/
def updateIssue (config : Config) (id : Nat) (modify : Issue → Issue) : IO (Option Issue) := do
  let issues ← loadAllIssues config
  match issues.find? (·.id == id) with
  | some oldIssue =>
    -- Find the old file path
    if let some oldPath ← findIssuePath config id then
      -- Apply modification
      let timestamp ← nowIso8601
      let newIssue := modify { oldIssue with updated := timestamp }
      -- Delete old file if filename changed
      if oldIssue.filename != newIssue.filename then
        IO.FS.removeFile oldPath
      -- Write new/updated file
      writeIssue config newIssue
      return some newIssue
    else
      return none
  | none =>
    return none

/-- Create a new issue -/
def createIssue (config : Config) (title : String) (description : String := "")
    (priority : Priority := .medium) (labels : Array String := #[])
    (assignee : Option String := none) (project : Option String := none) : IO Issue := do
  let id ← nextIssueId config
  let timestamp ← nowIso8601
  let issue : Issue := {
    id
    title
    status := .open_
    priority
    created := timestamp
    updated := timestamp
    labels
    assignee
    project
    blocks := #[]
    blockedBy := #[]
    description
    progress := #[]
  }
  writeIssue config issue
  return issue

/-- Add a progress entry to an issue -/
def addProgress (config : Config) (id : Nat) (message : String) : IO (Option Issue) := do
  let timestamp ← nowIso8601
  let entry : ProgressEntry := { timestamp, message }
  updateIssue config id fun issue =>
    { issue with progress := issue.progress.push entry }

/-- Close an issue with optional closing comment -/
def closeIssue (config : Config) (id : Nat) (comment : Option String := none) : IO (Option Issue) := do
  let timestamp ← nowIso8601
  updateIssue config id fun issue =>
    let issue := { issue with status := .closed }
    match comment with
    | some msg =>
      let entry : ProgressEntry := { timestamp, message := s!"Closed: {msg}" }
      { issue with progress := issue.progress.push entry }
    | none => issue

/-- Reopen a closed issue -/
def reopenIssue (config : Config) (id : Nat) : IO (Option Issue) := do
  let timestamp ← nowIso8601
  updateIssue config id fun issue =>
    let entry : ProgressEntry := { timestamp, message := "Reopened" }
    { issue with
      status := .open_
      progress := issue.progress.push entry }

/-- Add a dependency (issue `id` is blocked by issue `blockedById`) -/
def addBlockedBy (config : Config) (id : Nat) (blockedById : Nat) : IO (Option Issue) := do
  -- Check that the blocking issue exists
  let blocker ← findIssue config blockedById
  match blocker with
  | none => return none
  | some _ =>
    -- Update the blocked issue
    let result ← updateIssue config id fun issue =>
      if issue.blockedBy.contains blockedById then issue
      else { issue with blockedBy := issue.blockedBy.push blockedById }
    -- Also update the blocking issue's "blocks" list
    let _ ← updateIssue config blockedById fun issue =>
      if issue.blocks.contains id then issue
      else { issue with blocks := issue.blocks.push id }
    return result

/-- Remove a dependency -/
def removeBlockedBy (config : Config) (id : Nat) (blockedById : Nat) : IO (Option Issue) := do
  -- Update the blocked issue
  let result ← updateIssue config id fun issue =>
    { issue with blockedBy := issue.blockedBy.filter (· != blockedById) }
  -- Also update the blocking issue's "blocks" list
  let _ ← updateIssue config blockedById fun issue =>
    { issue with blocks := issue.blocks.filter (· != id) }
  return result

/-- Filter for listing issues -/
structure ListFilter where
  status : Option Status := none
  label : Option String := none
  assignee : Option String := none
  project : Option String := none
  blockedOnly : Bool := false
  includeAll : Bool := false  -- Include closed issues
  deriving Repr, Inhabited

/-- List issues with optional filtering -/
def listIssues (config : Config) (filter : ListFilter := {}) : IO (Array Issue) := do
  let issues ← loadAllIssues config
  let filtered := issues.filter fun issue =>
    -- Status filter
    let statusOk := match filter.status with
      | some s => issue.status == s
      | none => filter.includeAll || issue.status.isOpen
    -- Label filter
    let labelOk := match filter.label with
      | some l => issue.labels.contains l
      | none => true
    -- Assignee filter
    let assigneeOk := match filter.assignee with
      | some a => issue.assignee == some a
      | none => true
    -- Project filter
    let projectOk := match filter.project with
      | some p => issue.project == some p
      | none => true
    -- Blocked filter
    let blockedOk := !filter.blockedOnly || issue.isBlocked
    statusOk && labelOk && assigneeOk && projectOk && blockedOk
  return filtered

/-- Search issues by keyword across title, description, and progress notes. -/
def searchIssuesIn (issues : Array Issue) (query : String) : Array Issue :=
  let term := Util.trim query |>.toLower
  if term.isEmpty then
    #[]
  else
    let containsTerm (text : String) : Bool :=
      Util.containsSubstr (text.toLower) term
    let matchesTerm (issue : Issue) : Bool :=
      containsTerm issue.title ||
      containsTerm issue.description ||
      issue.progress.any (fun entry => containsTerm entry.message)
    issues.filter matchesTerm

/-- Delete an issue (removes the file) -/
def deleteIssue (config : Config) (id : Nat) : IO Bool := do
  match ← findIssuePath config id with
  | some path =>
    IO.FS.removeFile path
    return true
  | none =>
    return false

/-- Check if an issue is effectively blocked (any blocker is still open) -/
def isEffectivelyBlocked (issue : Issue) (allIssues : Array Issue) : Bool :=
  issue.blockedBy.any fun blockerId =>
    match allIssues.find? (·.id == blockerId) with
    | some blocker => blocker.status != .closed
    | none => false  -- Blocker doesn't exist, not blocking

end Tracker.Storage
