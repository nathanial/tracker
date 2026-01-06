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

/-- Process input and update state (pure function) -/
def update (state : AppState) (event : Option Event) : UpdateResult :=
  match event with
  | none => { state := state.clearMessages, shouldQuit := false, pendingAction := .none }
  | some (.key k) =>
    -- Global quit
    if k.code == .char 'q' || k.code == .char 'Q' then
      { state := state, shouldQuit := true, pendingAction := .none }
    else
      match state.viewMode with
      | .list => updateListView state k
      | .detail => updateDetailView state k
      | .create | .edit =>
        -- TODO: form handling
        { state := state, shouldQuit := false, pendingAction := .none }
  | some (.resize _ _) =>
    { state := state, shouldQuit := false, pendingAction := .none }
  | _ => { state := state, shouldQuit := false, pendingAction := .none }

end Tracker.TUI
