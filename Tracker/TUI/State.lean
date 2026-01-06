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

end AppState

end Tracker.TUI
