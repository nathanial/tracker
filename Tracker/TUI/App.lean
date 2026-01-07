/-
  TUI main application.
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Tracker.TUI.State
import Tracker.TUI.Draw
import Tracker.TUI.Update
import Terminus

namespace Tracker.TUI

open Tracker
open Terminus

/-- Load issues and create initial state -/
def initState (config : Storage.Config) : IO AppState := do
  let issues ← Storage.loadAllIssues config
  return {
    config
    issues
    viewMode := .projectList
    filterTab := .open_
    selectedIndex := 0
    projectSelectedIndex := 0
    projectFilter := .all
  }

/-- Process pending IO actions -/
def processPendingAction (state : AppState) (action : PendingAction) : IO AppState := do
  match action with
  | .none => return state
  | .closeIssue id =>
    match ← Storage.closeIssue state.config id none with
    | some _ =>
      let issues ← Storage.loadAllIssues state.config
      let newState := { state with
        issues
        statusMessage := s!"Issue #{id} closed"
        errorMessage := ""
      }
      -- Update current issue if viewing it
      let newState := match state.currentIssue with
        | some issue =>
          if issue.id == id then
            { newState with currentIssue := issues.find? (·.id == id) }
          else newState
        | none => newState
      return newState.clampSelection
    | none =>
      return { state with errorMessage := s!"Issue #{id} not found" }

  | .reopenIssue id =>
    match ← Storage.reopenIssue state.config id with
    | some _ =>
      let issues ← Storage.loadAllIssues state.config
      let newState := { state with
        issues
        statusMessage := s!"Issue #{id} reopened"
        errorMessage := ""
      }
      let newState := match state.currentIssue with
        | some issue =>
          if issue.id == id then
            { newState with currentIssue := issues.find? (·.id == id) }
          else newState
        | none => newState
      return newState.clampSelection
    | none =>
      return { state with errorMessage := s!"Issue #{id} not found" }

  | .refreshIssues =>
    let issues ← Storage.loadAllIssues state.config
    return { state with
      issues
      statusMessage := s!"Loaded {issues.size} issues"
      errorMessage := ""
    }.clampSelection

  | .createIssue title description priority labels assignee project =>
    let issue ← Storage.createIssue state.config title description priority labels assignee project
    let issues ← Storage.loadAllIssues state.config
    return { state with
      issues
      viewMode := .detail
      currentIssue := some issue
      statusMessage := s!"Created issue #{issue.id}"
      errorMessage := ""
    }.clampSelection

  | .updateIssue id title description priority labels assignee project =>
    let result ← Storage.updateIssue state.config id fun issue =>
      { issue with
        title
        description
        priority
        labels
        assignee
        project }
    match result with
    | some updatedIssue =>
      let issues ← Storage.loadAllIssues state.config
      return { state with
        issues
        viewMode := .detail
        currentIssue := some updatedIssue
        statusMessage := s!"Updated issue #{id}"
        errorMessage := ""
      }.clampSelection
    | none =>
      return { state with errorMessage := s!"Issue #{id} not found" }

/-- Custom tick function that handles pending actions -/
def tick (term : Terminal) (state : AppState) : IO (Terminal × AppState × Bool) := do
  -- Poll for input
  let event ← Events.poll
  let optEvent := match event with
    | .none => none
    | e => some e

  -- Run pure update
  let result := update state optEvent

  -- Process any pending IO action
  let state ← processPendingAction result.state result.pendingAction

  -- Draw
  let frame := Frame.new term.area
  let frame := draw frame state

  -- Update buffer and flush
  let term := term.setBuffer frame.buffer
  let term ← term.flush frame.commands

  return (term, state, result.shouldQuit)

/-- Main run loop -/
partial def runLoop (term : Terminal) (state : AppState) : IO Unit := do
  let (term, state, shouldQuit) ← tick term state

  if shouldQuit then
    return ()

  -- Sleep for ~60 FPS
  IO.sleep 16

  runLoop term state

/-- Run the TUI application -/
def run (config : Storage.Config) : IO Unit := do
  -- Initialize state
  let state ← initState config

  -- Use withTerminal for proper setup/teardown
  Terminal.withTerminal fun term => do
    -- Initial draw
    let term ← term.draw
    runLoop term state

end Tracker.TUI
