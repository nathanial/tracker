/-
  TUI input handling and state updates.
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Tracker.TUI.State
import Terminus

namespace Tracker.TUI

open Tracker
open Terminus

/-- Actions that require IO (will be processed in the main loop) -/
inductive PendingAction where
  | none
  | closeIssue (id : Nat)
  | reopenIssue (id : Nat)
  | refreshIssues
  | createIssue (title : String) (description : String) (priority : Priority) (labels : Array String) (assignee : Option String)
  | updateIssue (id : Nat) (title : String) (description : String) (priority : Priority) (labels : Array String) (assignee : Option String)
  deriving BEq, Inhabited

/-- Update result with optional pending action -/
structure UpdateResult where
  state : AppState
  shouldQuit : Bool := false
  pendingAction : PendingAction := .none
  deriving Inhabited

/-- Handle input in list view -/
def updateListView (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Navigation
  | .up | .char 'k' | .char 'K' =>
    { state := state.moveUp, shouldQuit := false, pendingAction := .none }
  | .down | .char 'j' | .char 'J' =>
    { state := state.moveDown, shouldQuit := false, pendingAction := .none }
  -- Tab switching (Shift+Tab for previous)
  | .tab =>
    if key.modifiers.shift then
      { state := state.prevTab, shouldQuit := false, pendingAction := .none }
    else
      { state := state.nextTab, shouldQuit := false, pendingAction := .none }
  -- Enter detail view
  | .enter =>
    { state := state.enterDetail, shouldQuit := false, pendingAction := .none }
  -- Create new issue
  | .char 'n' | .char 'N' =>
    { state := state.enterCreate, shouldQuit := false, pendingAction := .none }
  -- Quick actions on selected issue
  | .char 'c' | .char 'C' =>
    match state.selectedIssue with
    | some issue =>
      if issue.status != .closed then
        { state := state.setStatus "Closing..."
          shouldQuit := false
          pendingAction := .closeIssue issue.id }
      else
        { state := state.setError "Issue already closed", shouldQuit := false, pendingAction := .none }
    | none => { state := state, shouldQuit := false, pendingAction := .none }
  | .char 'o' | .char 'O' =>
    match state.selectedIssue with
    | some issue =>
      if issue.status == .closed then
        { state := state.setStatus "Reopening..."
          shouldQuit := false
          pendingAction := .reopenIssue issue.id }
      else
        { state := state.setError "Issue is not closed", shouldQuit := false, pendingAction := .none }
    | none => { state := state, shouldQuit := false, pendingAction := .none }
  -- Refresh
  | .char 'r' | .char 'R' =>
    { state := state.setStatus "Refreshing..."
      shouldQuit := false
      pendingAction := .refreshIssues }
  | _ => { state := state, shouldQuit := false, pendingAction := .none }

/-- Handle input in detail view -/
def updateDetailView (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Back to list
  | .escape =>
    { state := state.returnToList, shouldQuit := false, pendingAction := .none }
  -- Edit issue
  | .char 'e' | .char 'E' =>
    { state := state.enterEdit, shouldQuit := false, pendingAction := .none }
  -- Close issue
  | .char 'c' | .char 'C' =>
    match state.currentIssue with
    | some issue =>
      if issue.status != .closed then
        { state := state.setStatus "Closing..."
          shouldQuit := false
          pendingAction := .closeIssue issue.id }
      else
        { state := state.setError "Issue already closed", shouldQuit := false, pendingAction := .none }
    | none => { state := state, shouldQuit := false, pendingAction := .none }
  -- Reopen issue
  | .char 'r' | .char 'R' =>
    match state.currentIssue with
    | some issue =>
      if issue.status == .closed then
        { state := state.setStatus "Reopening..."
          shouldQuit := false
          pendingAction := .reopenIssue issue.id }
      else
        { state := state.setError "Issue is not closed", shouldQuit := false, pendingAction := .none }
    | none => { state := state, shouldQuit := false, pendingAction := .none }
  | _ => { state := state, shouldQuit := false, pendingAction := .none }

/-- Handle text input for form fields -/
def handleTextInput (state : AppState) (key : KeyEvent) : AppState :=
  let form := state.formState
  -- For priority field, handle left/right to cycle
  if form.focusedField == .priority then
    match key.code with
    | .left => state.updateForm FormState.cyclePriorityDown
    | .right => state.updateForm FormState.cyclePriorityUp
    | _ => state
  else
    -- For text fields, handle character input and editing
    match key.code with
    | .char c =>
      state.updateForm fun f =>
        f.updateCurrentInput (f.currentInput.insertChar c)
    | .backspace =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.backspace
    | .delete =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.delete
    | .left =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveLeft
    | .right =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveRight
    | .home =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveToStart
    | .«end» =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveToEnd
    | _ => state

/-- Handle input in form view (create/edit) -/
def updateFormView (state : AppState) (key : KeyEvent) : UpdateResult :=
  match key.code with
  -- Cancel
  | .escape =>
    { state := state.cancelForm, shouldQuit := false, pendingAction := .none }
  -- Navigate fields
  | .tab =>
    if key.modifiers.shift then
      { state := state.updateForm FormState.prevField, shouldQuit := false, pendingAction := .none }
    else
      { state := state.updateForm FormState.nextField, shouldQuit := false, pendingAction := .none }
  -- Submit (Ctrl+S)
  | .char 's' | .char 'S' =>
    if key.modifiers.ctrl then
      let form := state.formState
      if form.isValid then
        let title := form.title.text.trim
        let description := form.description.text.trim
        let priority := form.priority
        let labels := form.getLabels
        let assignee := form.getAssignee
        match form.editingIssueId with
        | some id =>
          { state := state.setStatus "Saving..."
            shouldQuit := false
            pendingAction := .updateIssue id title description priority labels assignee }
        | none =>
          { state := state.setStatus "Creating..."
            shouldQuit := false
            pendingAction := .createIssue title description priority labels assignee }
      else
        { state := state.setError "Title is required", shouldQuit := false, pendingAction := .none }
    else
      -- Regular 's' character input
      { state := handleTextInput state key, shouldQuit := false, pendingAction := .none }
  -- Text input
  | _ =>
    { state := handleTextInput state key, shouldQuit := false, pendingAction := .none }

/-- Process input and update state (pure function) -/
def update (state : AppState) (event : Option Event) : UpdateResult :=
  match event with
  | none => { state := state.clearMessages, shouldQuit := false, pendingAction := .none }
  | some (.key k) =>
    -- Global quit (but not in form mode - use Escape to cancel)
    if k.code == .char 'q' || k.code == .char 'Q' then
      match state.viewMode with
      | .create | .edit =>
        -- In form mode, 'q' is just a character
        updateFormView state k
      | _ =>
        { state := state, shouldQuit := true, pendingAction := .none }
    else
      match state.viewMode with
      | .list => updateListView state k
      | .detail => updateDetailView state k
      | .create | .edit => updateFormView state k
  | some (.resize _ _) =>
    { state := state, shouldQuit := false, pendingAction := .none }
  | _ => { state := state, shouldQuit := false, pendingAction := .none }

end Tracker.TUI
