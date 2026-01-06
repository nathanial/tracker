/-
  TUI state management.
-/
import Tracker.Core.Types
import Tracker.Core.Storage

namespace Tracker.TUI

open Tracker

/-- Current view mode -/
inductive ViewMode where
  | list        -- Issue list
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
  deriving BEq, Inhabited

namespace FormField

def next : FormField → FormField
  | .title => .description
  | .description => .priority
  | .priority => .labels
  | .labels => .assignee
  | .assignee => .title

def prev : FormField → FormField
  | .title => .assignee
  | .description => .title
  | .priority => .description
  | .labels => .priority
  | .assignee => .labels

def toString : FormField → String
  | .title => "Title"
  | .description => "Description"
  | .priority => "Priority"
  | .labels => "Labels"
  | .assignee => "Assignee"

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

/-- Update current text input -/
def updateCurrentInput (form : FormState) (input : TextInput) : FormState :=
  match form.focusedField with
  | .title => { form with title := input }
  | .description => { form with description := input }
  | .priority => form  -- Priority isn't text input
  | .labels => { form with labels := input }
  | .assignee => { form with assignee := input }

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

/-- Check if form is valid for submission -/
def isValid (form : FormState) : Bool :=
  !form.title.text.trim.isEmpty

end FormState

/-- Filter tab in list view -/
inductive FilterTab where
  | open_
  | inProgress
  | closed
  | all
  deriving BEq, Inhabited

namespace FilterTab

def next : FilterTab → FilterTab
  | .open_ => .inProgress
  | .inProgress => .closed
  | .closed => .all
  | .all => .open_

def prev : FilterTab → FilterTab
  | .open_ => .all
  | .inProgress => .open_
  | .closed => .inProgress
  | .all => .closed

def toString : FilterTab → String
  | .open_ => "Open"
  | .inProgress => "In Progress"
  | .closed => "Closed"
  | .all => "All"

end FilterTab

/-- TUI application state -/
structure AppState where
  /-- Storage config -/
  config : Storage.Config
  /-- All loaded issues -/
  issues : Array Issue
  /-- Current view -/
  viewMode : ViewMode := .list
  /-- Current filter tab -/
  filterTab : FilterTab := .open_
  /-- Selected index in list view -/
  selectedIndex : Nat := 0
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

/-- Get filtered issues based on current tab -/
def filteredIssues (state : AppState) : Array Issue :=
  state.issues.filter fun issue =>
    match state.filterTab with
    | .open_ => issue.status == .open_
    | .inProgress => issue.status == .inProgress
    | .closed => issue.status == .closed
    | .all => true

/-- Ensure selected index is valid -/
def clampSelection (state : AppState) : AppState :=
  let filtered := state.filteredIssues
  let maxIdx := if filtered.isEmpty then 0 else filtered.size - 1
  { state with selectedIndex := min state.selectedIndex maxIdx }

/-- Get the currently selected issue -/
def selectedIssue (state : AppState) : Option Issue :=
  let filtered := state.filteredIssues
  if h : state.selectedIndex < filtered.size then
    some filtered[state.selectedIndex]
  else none

/-- Move selection up -/
def moveUp (state : AppState) : AppState :=
  if state.selectedIndex > 0 then
    { state with selectedIndex := state.selectedIndex - 1 }
  else state

/-- Move selection down -/
def moveDown (state : AppState) : AppState :=
  let maxIdx := state.filteredIssues.size
  if state.selectedIndex + 1 < maxIdx then
    { state with selectedIndex := state.selectedIndex + 1 }
  else state

/-- Switch to next tab -/
def nextTab (state : AppState) : AppState :=
  { state with
    filterTab := state.filterTab.next
    selectedIndex := 0 }.clampSelection

/-- Switch to previous tab -/
def prevTab (state : AppState) : AppState :=
  { state with
    filterTab := state.filterTab.prev
    selectedIndex := 0 }.clampSelection

/-- Enter detail view for selected issue -/
def enterDetail (state : AppState) : AppState :=
  match state.selectedIssue with
  | some issue =>
    { state with
      viewMode := .detail
      currentIssue := some issue }
  | none => state

/-- Return to list view -/
def returnToList (state : AppState) : AppState :=
  { state with
    viewMode := .list
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

/-- Enter create mode with empty form -/
def enterCreate (state : AppState) : AppState :=
  { state with
    viewMode := .create
    formState := FormState.empty }

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
    -- Was creating, return to list view
    { state with viewMode := .list }

/-- Update form state -/
def updateForm (state : AppState) (f : FormState → FormState) : AppState :=
  { state with formState := f state.formState }

end AppState

end Tracker.TUI
