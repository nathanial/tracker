/-
  TUI state management.
-/
import Tracker.Core.Types
import Tracker.Core.Storage

namespace Tracker.TUI

open Tracker

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
  /-- Cursor position (0-indexed) -/
  cursorPos : Nat := 0
  deriving BEq, Inhabited

namespace TextInput

def clear : TextInput := {}

def setText (text : String) : TextInput :=
  { text := text, cursorPos := text.length }

def insertChar (c : Char) (input : TextInput) : TextInput :=
  let before := input.text.take input.cursorPos
  let after := input.text.drop input.cursorPos
  { text := before ++ c.toString ++ after, cursorPos := input.cursorPos + 1 }

def backspace (input : TextInput) : TextInput :=
  if input.cursorPos == 0 then input
  else
    let before := input.text.take (input.cursorPos - 1)
    let after := input.text.drop input.cursorPos
    { text := before ++ after, cursorPos := input.cursorPos - 1 }

def delete (input : TextInput) : TextInput :=
  if input.cursorPos >= input.text.length then input
  else
    let before := input.text.take input.cursorPos
    let after := input.text.drop (input.cursorPos + 1)
    { text := before ++ after, cursorPos := input.cursorPos }

def moveLeft (input : TextInput) : TextInput :=
  if input.cursorPos == 0 then input
  else { input with cursorPos := input.cursorPos - 1 }

def moveRight (input : TextInput) : TextInput :=
  if input.cursorPos >= input.text.length then input
  else { input with cursorPos := input.cursorPos + 1 }

def moveToStart (input : TextInput) : TextInput :=
  { input with cursorPos := 0 }

def moveToEnd (input : TextInput) : TextInput :=
  { input with cursorPos := input.text.length }

end TextInput

/-- Form field focus -/
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

end FormField

/-- Form state for creating/editing issues -/
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
  /-- Issue ID if editing (none for create) -/
  editingIssueId : Option Nat := none
  deriving BEq, Inhabited

namespace FormState

def empty : FormState := {}

def fromIssue (issue : Issue) : FormState :=
  { title := TextInput.setText issue.title
    description := TextInput.setText issue.description
    priority := issue.priority
    labels := TextInput.setText (String.intercalate ", " issue.labels.toList)
    assignee := TextInput.setText (issue.assignee.getD "")
    project := TextInput.setText (issue.project.getD "")
    focusedField := .title
    editingIssueId := some issue.id }

def nextField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.next }

def prevField (form : FormState) : FormState :=
  { form with focusedField := form.focusedField.prev }

def cyclePriorityUp (form : FormState) : FormState :=
  let newPriority := match form.priority with
    | .low => .medium
    | .medium => .high
    | .high => .critical
    | .critical => .low
  { form with priority := newPriority }

def cyclePriorityDown (form : FormState) : FormState :=
  let newPriority := match form.priority with
    | .low => .critical
    | .medium => .low
    | .high => .medium
    | .critical => .high
  { form with priority := newPriority }

def currentInput (form : FormState) : TextInput :=
  match form.focusedField with
  | .title => form.title
  | .description => form.description
  | .priority => {}  -- Priority uses left/right, not text input
  | .labels => form.labels
  | .assignee => form.assignee
  | .project => form.project

def updateCurrentInput (form : FormState) (input : TextInput) : FormState :=
  match form.focusedField with
  | .title => { form with title := input }
  | .description => { form with description := input }
  | .priority => form  -- Priority doesn't use text input
  | .labels => { form with labels := input }
  | .assignee => { form with assignee := input }
  | .project => { form with project := input }

def getLabels (form : FormState) : Array String :=
  form.labels.text.splitOn ","
    |>.map String.trim
    |>.filter (·.length > 0)
    |>.toArray

def getAssignee (form : FormState) : Option String :=
  let text := form.assignee.text.trim
  if text.isEmpty then none else some text

def getProject (form : FormState) : Option String :=
  let text := form.project.text.trim
  if text.isEmpty then none else some text

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
  /-- Error suggestion (actionable hint) -/
  errorSuggestion : String := ""
  deriving Inhabited

namespace AppState

/-- Helper: Format priority as short string -/
private def priorityShort : Priority → String
  | .low => "L"
  | .medium => "M"
  | .high => "H"
  | .critical => "!"

/-- Helper: Status icon -/
private def statusIcon : Status → String
  | .open_ => " "
  | .inProgress => ">"
  | .closed => "x"

/-- Format an issue as a tree leaf label -/
def issueLabel (issue : Issue) : String :=
  let icon := statusIcon issue.status
  let prio := priorityShort issue.priority
  s!"[{icon}] #{issue.id} [{prio}] {issue.title}"

/-- Parse issue ID from a tree label string.
    Format: "[x] #123 [PRIO] Title" -/
def parseIssueIdFromLabel (label : String) : Option Nat :=
  let parts := label.splitOn "#"
  if parts.length >= 2 then
    let idPart := parts[1]!.takeWhile (·.isDigit)
    idPart.toNat?
  else none

/-- Find an issue by ID -/
def findIssue (state : AppState) (id : Nat) : Option Issue :=
  state.issues.find? (·.id == id)

/-- Switch to next tree view mode -/
def nextTreeViewMode (state : AppState) : AppState :=
  { state with treeViewMode := state.treeViewMode.next }

/-- Switch to previous tree view mode -/
def prevTreeViewMode (state : AppState) : AppState :=
  { state with treeViewMode := state.treeViewMode.prev }

/-- Toggle showing closed issues -/
def toggleShowClosed (state : AppState) : AppState :=
  { state with showClosed := !state.showClosed }

/-- Enter detail view for a specific issue -/
def enterDetailFor (state : AppState) (issue : Issue) : AppState :=
  { state with
    viewMode := .detail
    currentIssue := some issue }

/-- Return to tree view -/
def returnToTree (state : AppState) : AppState :=
  { state with
    viewMode := .tree
    currentIssue := none }

/-- Set status message -/
def setStatus (state : AppState) (msg : String) : AppState :=
  { state with statusMessage := msg, errorMessage := "", errorSuggestion := "" }

/-- Set error message with optional suggestion -/
def setError (state : AppState) (msg : String) (suggestion : String := "") : AppState :=
  { state with errorMessage := msg, errorSuggestion := suggestion, statusMessage := "" }

/-- Clear messages -/
def clearMessages (state : AppState) : AppState :=
  { state with statusMessage := "", errorMessage := "", errorSuggestion := "" }

/-- Enter create mode -/
def enterCreate (state : AppState) : AppState :=
  { state with
    viewMode := .create
    formState := FormState.empty }

/-- Enter create mode with a pre-filled project -/
def enterCreateWithProject (state : AppState) (projectText : String) : AppState :=
  { state with
    viewMode := .create
    formState := { FormState.empty with
      project := TextInput.setText projectText } }

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
