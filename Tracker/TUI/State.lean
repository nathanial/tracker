/-
  TUI state management.
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Terminus

namespace Tracker.TUI

open Tracker
open Terminus

/-- Tree view organization mode -/
inductive TreeViewMode where
  | byProject       -- Projects as branches, issues as leaves
  | byStatus        -- Status as branches, issues as leaves
  | byProjectStatus -- Projects → Status → Issues (3-level)
  deriving BEq, Inhabited

namespace TreeViewMode

def next : TreeViewMode → TreeViewMode
  | .byProject => .byStatus
  | .byStatus => .byProjectStatus
  | .byProjectStatus => .byProject

def prev : TreeViewMode → TreeViewMode
  | .byProject => .byProjectStatus
  | .byStatus => .byProject
  | .byProjectStatus => .byStatus

def toString : TreeViewMode → String
  | .byProject => "By Project"
  | .byStatus => "By Status"
  | .byProjectStatus => "By Project+Status"

end TreeViewMode

/-- Current view mode -/
inductive ViewMode where
  | tree        -- Main tree view with tabs
  | detail      -- Single issue detail
  | create      -- Create new issue form
  | edit        -- Edit issue form
  deriving BEq, Inhabited

/-- Text input field state -/
structure TextInput where
  /-- Current text content -/
  text : String := ""
  /-- Cursor position (character index) -/
  cursor : Nat := 0
  deriving BEq, Inhabited

namespace TextInput

/-- Insert a character at cursor position -/
def insertChar (input : TextInput) (c : Char) : TextInput :=
  let before := input.text.take input.cursor
  let after := input.text.drop input.cursor
  { text := before ++ c.toString ++ after
    cursor := input.cursor + 1 }

/-- Delete character before cursor (backspace) -/
def backspace (input : TextInput) : TextInput :=
  if input.cursor == 0 then input
  else
    let before := input.text.take (input.cursor - 1)
    let after := input.text.drop input.cursor
    { text := before ++ after
      cursor := input.cursor - 1 }

/-- Delete character at cursor (delete) -/
def delete (input : TextInput) : TextInput :=
  let before := input.text.take input.cursor
  let after := input.text.drop (input.cursor + 1)
  { input with text := before ++ after }

/-- Move cursor left -/
def moveLeft (input : TextInput) : TextInput :=
  if input.cursor > 0 then
    { input with cursor := input.cursor - 1 }
  else input

/-- Move cursor right -/
def moveRight (input : TextInput) : TextInput :=
  if input.cursor < input.text.length then
    { input with cursor := input.cursor + 1 }
  else input

/-- Move cursor to start -/
def moveToStart (input : TextInput) : TextInput :=
  { input with cursor := 0 }

/-- Move cursor to end -/
def moveToEnd (input : TextInput) : TextInput :=
  { input with cursor := input.text.length }

/-- Set text and move cursor to end -/
def setText (input : TextInput) (text : String) : TextInput :=
  { text, cursor := text.length }

/-- Clear input -/
def clear : TextInput :=
  { text := "", cursor := 0 }

end TextInput

/-- Form fields for create/edit form -/
inductive FormField where
  | title
  | description
  | priority
  | labels
  | assignee
  | project
  deriving BEq, Inhabited

namespace FormField

def next : FormField → FormField
  | .title => .description
  | .description => .priority
  | .priority => .labels
  | .labels => .assignee
  | .assignee => .project
  | .project => .title

def prev : FormField → FormField
  | .title => .project
  | .description => .title
  | .priority => .description
  | .labels => .priority
  | .assignee => .labels
  | .project => .assignee

def toString : FormField → String
  | .title => "Title"
  | .description => "Description"
  | .priority => "Priority"
  | .labels => "Labels"
  | .assignee => "Assignee"
  | .project => "Project"

end FormField

/-- Form state for create/edit -/
structure FormState where
  /-- Title input -/
  title : TextInput := {}
  /-- Description input -/
  description : TextInput := {}
  /-- Selected priority -/
  priority : Priority := .medium
  /-- Labels input (comma-separated) -/
  labels : TextInput := {}
  /-- Assignee input -/
  assignee : TextInput := {}
  /-- Project input -/
  project : TextInput := {}
  /-- Currently focused field -/
  focusedField : FormField := .title
  /-- Issue being edited (for edit mode) -/
  editingIssueId : Option Nat := none
  deriving BEq, Inhabited

namespace FormState

/-- Create empty form for new issue -/
def empty : FormState := {}

/-- Create form pre-filled from an issue -/
def fromIssue (issue : Issue) : FormState :=
  { title := TextInput.clear.setText issue.title
    description := TextInput.clear.setText issue.description
    priority := issue.priority
    labels := TextInput.clear.setText (String.intercalate ", " issue.labels.toList)
    assignee := TextInput.clear.setText (issue.assignee.getD "")
    project := TextInput.clear.setText (issue.project.getD "")
    focusedField := .title
    editingIssueId := some issue.id }

/-- Move to next field -/
def nextField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.next }

/-- Move to previous field -/
def prevField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.prev }

/-- Cycle priority up -/
def cyclePriorityUp (form : FormState) : FormState :=
  let newPriority := match form.priority with
    | .low => .medium
    | .medium => .high
    | .high => .critical
    | .critical => .low
  { form with priority := newPriority }

/-- Cycle priority down -/
def cyclePriorityDown (form : FormState) : FormState :=
  let newPriority := match form.priority with
    | .low => .critical
    | .medium => .low
    | .high => .medium
    | .critical => .high
  { form with priority := newPriority }

/-- Get current text input based on focused field -/
def currentInput (form : FormState) : TextInput :=
  match form.focusedField with
  | .title => form.title
  | .description => form.description
  | .priority => TextInput.clear  -- Priority isn't text input
  | .labels => form.labels
  | .assignee => form.assignee
  | .project => form.project

/-- Update current text input -/
def updateCurrentInput (form : FormState) (input : TextInput) : FormState :=
  match form.focusedField with
  | .title => { form with title := input }
  | .description => { form with description := input }
  | .priority => form  -- Priority isn't text input
  | .labels => { form with labels := input }
  | .assignee => { form with assignee := input }
  | .project => { form with project := input }

/-- Parse labels from comma-separated string -/
def getLabels (form : FormState) : Array String :=
  form.labels.text.splitOn ","
    |>.map String.trim
    |>.filter (· != "")
    |>.toArray

/-- Get assignee (None if empty) -/
def getAssignee (form : FormState) : Option String :=
  let trimmed := form.assignee.text.trim
  if trimmed.isEmpty then none else some trimmed

/-- Get project (None if empty) -/
def getProject (form : FormState) : Option String :=
  let trimmed := form.project.text.trim
  if trimmed.isEmpty then none else some trimmed

/-- Check if form is valid for submission -/
def isValid (form : FormState) : Bool :=
  !form.title.text.trim.isEmpty

end FormState

/-- TUI application state -/
structure AppState where
  /-- Storage config -/
  config : Storage.Config
  /-- All loaded issues -/
  issues : Array Issue
  /-- Current view -/
  viewMode : ViewMode := .tree
  /-- Tree view organization mode -/
  treeViewMode : TreeViewMode := .byProject
  /-- The tree widget state -/
  issueTree : Tree := {}
  /-- Whether to show closed issues in the tree -/
  showClosed : Bool := false
  /-- Currently viewed issue (for detail view) -/
  currentIssue : Option Issue := none
  /-- Form state for create/edit views -/
  formState : FormState := {}
  /-- Status message -/
  statusMessage : String := ""
  /-- Error message -/
  errorMessage : String := ""
  deriving Inhabited

namespace AppState

/-- Helper: Format priority as short string -/
private def priorityShort : Priority → String
  | .low => "LOW"
  | .medium => "MED"
  | .high => "HIGH"
  | .critical => "CRIT"

/-- Helper: Format status icon -/
private def statusIcon : Status → String
  | .open_ => " "
  | .inProgress => ">"
  | .closed => "x"

/-- Helper: Create a tree leaf for an issue -/
private def issueToLeaf (issue : Issue) : TreeNode :=
  let icon := statusIcon issue.status
  let prio := priorityShort issue.priority
  TreeNode.leaf s!"[{icon}] #{issue.id} [{prio}] {issue.title}" (some (toString issue.id))

/-- Build tree organized by project -/
def buildByProjectTree (issues : Array Issue) (showClosed : Bool) : List TreeNode := Id.run do
  let filtered := if showClosed then issues
                  else issues.filter (·.status != .closed)

  -- Group issues by project
  let mut projectMap : List (String × Array Issue) := []
  for issue in filtered do
    let key := issue.project.getD "No Project"
    match projectMap.find? (·.1 == key) with
    | some (_, arr) =>
      projectMap := projectMap.map fun (k, v) =>
        if k == key then (k, v.push issue) else (k, v)
    | none =>
      projectMap := projectMap ++ [(key, #[issue])]

  -- Sort by project name
  let sorted := projectMap.toArray.qsort (·.1 < ·.1)

  -- Convert to TreeNodes
  let mut nodes : List TreeNode := []
  for (project, projectIssues) in sorted do
    let leaves := projectIssues.toList.map issueToLeaf
    nodes := nodes ++ [TreeNode.branch s!"{project} ({projectIssues.size})" leaves false]
  nodes

/-- Build tree organized by status -/
def buildByStatusTree (issues : Array Issue) (showClosed : Bool) : List TreeNode := Id.run do
  let filtered := if showClosed then issues
                  else issues.filter (·.status != .closed)

  -- Group by status
  let openIssues := filtered.filter (·.status == .open_)
  let inProgressIssues := filtered.filter (·.status == .inProgress)
  let closedIssues := if showClosed then filtered.filter (·.status == .closed) else #[]

  let mut nodes : List TreeNode := []

  if !openIssues.isEmpty then
    let leaves := openIssues.toList.map issueToLeaf
    nodes := nodes ++ [TreeNode.branch s!"Open ({openIssues.size})" leaves false]

  if !inProgressIssues.isEmpty then
    let leaves := inProgressIssues.toList.map issueToLeaf
    nodes := nodes ++ [TreeNode.branch s!"In Progress ({inProgressIssues.size})" leaves false]

  if showClosed && !closedIssues.isEmpty then
    let leaves := closedIssues.toList.map issueToLeaf
    nodes := nodes ++ [TreeNode.branch s!"Closed ({closedIssues.size})" leaves false]

  nodes

/-- Build tree organized by project then status -/
def buildByProjectStatusTree (issues : Array Issue) (showClosed : Bool) : List TreeNode := Id.run do
  let filtered := if showClosed then issues
                  else issues.filter (·.status != .closed)

  -- Group issues by project
  let mut projectMap : List (String × Array Issue) := []
  for issue in filtered do
    let key := issue.project.getD "No Project"
    match projectMap.find? (·.1 == key) with
    | some (_, arr) =>
      projectMap := projectMap.map fun (k, v) =>
        if k == key then (k, v.push issue) else (k, v)
    | none =>
      projectMap := projectMap ++ [(key, #[issue])]

  -- Sort by project name
  let sorted := projectMap.toArray.qsort (·.1 < ·.1)

  -- Convert to TreeNodes with status sub-branches
  let mut nodes : List TreeNode := []
  for (project, projectIssues) in sorted do
    let openIssues := projectIssues.filter (·.status == .open_)
    let inProgressIssues := projectIssues.filter (·.status == .inProgress)
    let closedIssues := if showClosed then projectIssues.filter (·.status == .closed) else #[]

    let mut statusBranches : List TreeNode := []
    if !openIssues.isEmpty then
      let leaves := openIssues.toList.map issueToLeaf
      statusBranches := statusBranches ++ [TreeNode.branch s!"Open ({openIssues.size})" leaves false]
    if !inProgressIssues.isEmpty then
      let leaves := inProgressIssues.toList.map issueToLeaf
      statusBranches := statusBranches ++ [TreeNode.branch s!"In Progress ({inProgressIssues.size})" leaves false]
    if showClosed && !closedIssues.isEmpty then
      let leaves := closedIssues.toList.map issueToLeaf
      statusBranches := statusBranches ++ [TreeNode.branch s!"Closed ({closedIssues.size})" leaves false]

    if !statusBranches.isEmpty then
      nodes := nodes ++ [TreeNode.branch s!"{project} ({projectIssues.size})" statusBranches false]
  nodes

/-- Build tree based on current view mode -/
def buildTree (state : AppState) : List TreeNode :=
  match state.treeViewMode with
  | .byProject => buildByProjectTree state.issues state.showClosed
  | .byStatus => buildByStatusTree state.issues state.showClosed
  | .byProjectStatus => buildByProjectStatusTree state.issues state.showClosed

/-- Rebuild the tree (call after issues change or view mode changes) -/
def rebuildTree (state : AppState) : AppState :=
  let nodes := buildTree state
  { state with issueTree := Tree.new nodes }

/-- Parse issue ID from tree leaf data -/
private def parseIssueId (data : Option String) : Option Nat :=
  data.bind (·.toNat?)

/-- Get the selected issue from the tree (if a leaf is selected) -/
def selectedIssue (state : AppState) : Option Issue :=
  match state.issueTree.getSelected with
  | none => none
  | some line =>
    if line.isLeaf then
      -- The leaf label contains the issue ID after '#'
      -- Format: "[x] #123 [PRIO] Title"
      let parts := line.label.splitOn "#"
      if parts.length >= 2 then
        let idPart := parts[1]!.takeWhile (·.isDigit)
        match idPart.toNat? with
        | some id => state.issues.find? (·.id == id)
        | none => none
      else none
    else none

/-- Move tree selection up -/
def moveUp (state : AppState) : AppState :=
  { state with issueTree := state.issueTree.selectPrev }

/-- Move tree selection down -/
def moveDown (state : AppState) : AppState :=
  { state with issueTree := state.issueTree.selectNext }

/-- Toggle expand/collapse at current tree selection -/
def toggleTreeNode (state : AppState) : AppState :=
  { state with issueTree := state.issueTree.toggleSelected }

/-- Switch to next tree view mode -/
def nextTreeViewMode (state : AppState) : AppState :=
  { state with treeViewMode := state.treeViewMode.next }.rebuildTree

/-- Switch to previous tree view mode -/
def prevTreeViewMode (state : AppState) : AppState :=
  { state with treeViewMode := state.treeViewMode.prev }.rebuildTree

/-- Toggle showing closed issues -/
def toggleShowClosed (state : AppState) : AppState :=
  { state with showClosed := !state.showClosed }.rebuildTree

/-- Enter detail view for selected issue -/
def enterDetail (state : AppState) : AppState :=
  match state.selectedIssue with
  | some issue =>
    { state with
      viewMode := .detail
      currentIssue := some issue }
  | none => state

/-- Return to tree view -/
def returnToTree (state : AppState) : AppState :=
  { state with
    viewMode := .tree
    currentIssue := none }

/-- Set status message -/
def setStatus (state : AppState) (msg : String) : AppState :=
  { state with statusMessage := msg, errorMessage := "" }

/-- Set error message -/
def setError (state : AppState) (msg : String) : AppState :=
  { state with errorMessage := msg, statusMessage := "" }

/-- Clear messages -/
def clearMessages (state : AppState) : AppState :=
  { state with statusMessage := "", errorMessage := "" }

/-- Enter create mode -/
def enterCreate (state : AppState) : AppState :=
  -- Try to pre-fill project from selected branch
  let projectText := match state.issueTree.getSelected with
    | some line =>
      if !line.isLeaf && line.depth == 0 then
        -- Top-level branch might be a project name
        let label := line.label
        -- Remove count suffix like " (3)"
        let parts := label.splitOn " ("
        if parts.length >= 1 then parts[0]! else ""
      else ""
    | none => ""
  { state with
    viewMode := .create
    formState := { FormState.empty with
      project := TextInput.clear.setText projectText } }

/-- Enter edit mode for current issue -/
def enterEdit (state : AppState) : AppState :=
  match state.currentIssue with
  | some issue =>
    { state with
      viewMode := .edit
      formState := FormState.fromIssue issue }
  | none => state

/-- Cancel form and return to previous view -/
def cancelForm (state : AppState) : AppState :=
  match state.formState.editingIssueId with
  | some _ =>
    -- Was editing, return to detail view
    { state with viewMode := .detail }
  | none =>
    -- Was creating, return to tree view
    { state with viewMode := .tree }

/-- Update form state -/
def updateForm (state : AppState) (f : FormState → FormState) : AppState :=
  { state with formState := f state.formState }

end AppState

end Tracker.TUI
