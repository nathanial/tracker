/-
  Tracker TUI Integration Tests
-/
import Crucible
import Terminus.Reactive
import Reactive
import Tracker.Core.Types
import Tracker.TUI.State
import Tracker.TUI.App

namespace TrackerTests.TUI

open Crucible
open Tracker
open Tracker.TUI
open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

testSuite "Tracker.TUI"

/-- Create mock issues for testing -/
def mockIssues : Array Issue := #[
  { id := 1, title := "First Issue", status := .open_, priority := .high,
    created := "2026-01-01", updated := "2026-01-01", labels := #[],
    assignee := none, project := some "TestProject", blocks := #[],
    blockedBy := #[], description := "", progress := #[] },
  { id := 2, title := "Second Issue", status := .open_, priority := .medium,
    created := "2026-01-01", updated := "2026-01-01", labels := #[],
    assignee := none, project := some "TestProject", blocks := #[],
    blockedBy := #[], description := "", progress := #[] }
]

/-- Helper to check if a string contains a substring -/
def containsSubstr (s needle : String) : Bool :=
  (s.splitOn needle).length > 1

test "down arrow moves tree selection" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs

    -- Build tree data using tracker's buildTreeData function
    let treeData := buildTreeData mockIssues .byProject false

    -- Create dynamic from static data
    let treeDataDyn ← Dynamic.pureM treeData

    -- Build tree widget with globalKeys (same as tracker app)
    let (result, _render) ← (runWidget do
      forestDyn' treeDataDyn { globalKeys := true }
    ).run events

    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Initial selection should be the first node (project branch)
  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some label =>
    ensure (containsSubstr label "TestProject") s!"should start at project, got: {label}"
  | none =>
    ensure false "should have initial selection"

  -- Press down arrow
  inputs.fireKey { event := KeyEvent.down, focusedWidget := none }

  -- Selection should move to first issue
  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some label =>
    ensure (containsSubstr label "#1") s!"should move to first issue, got: {label}"
  | none =>
    ensure false "should have selection after down arrow"

  env.currentScope.dispose

test "tree selection navigates through issues" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    let treeData := buildTreeData mockIssues .byProject false
    let treeDataDyn ← Dynamic.pureM treeData
    let (result, _render) ← (runWidget do
      forestDyn' treeDataDyn { globalKeys := true }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Navigate: Project -> Issue 1 -> Issue 2
  inputs.fireKey { event := KeyEvent.down, focusedWidget := none }  -- Move to Issue 1
  inputs.fireKey { event := KeyEvent.down, focusedWidget := none }  -- Move to Issue 2

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some label =>
    ensure (containsSubstr label "#2") s!"should be at second issue, got: {label}"
  | none =>
    ensure false "should have selection"

  env.currentScope.dispose

test "up arrow moves selection back" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    let treeData := buildTreeData mockIssues .byProject false
    let treeDataDyn ← Dynamic.pureM treeData
    let (result, _render) ← (runWidget do
      forestDyn' treeDataDyn { globalKeys := true }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Navigate down twice, then up once
  inputs.fireKey { event := KeyEvent.down, focusedWidget := none }  -- Issue 1
  inputs.fireKey { event := KeyEvent.down, focusedWidget := none }  -- Issue 2
  inputs.fireKey { event := KeyEvent.up, focusedWidget := none }    -- Back to Issue 1

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some label =>
    ensure (containsSubstr label "#1") s!"should be at first issue after up, got: {label}"
  | none =>
    ensure false "should have selection"

  env.currentScope.dispose

test "buildTreeData groups by project" := do
  let treeData := buildTreeData mockIssues .byProject false
  ensure (treeData.size == 1) s!"expected 1 project branch, got {treeData.size}"
  match treeData[0]? with
  | some branch =>
    ensure (containsSubstr branch.value "TestProject")
        s!"branch value should contain TestProject, got: {branch.value}"
    ensure (branch.children.size == 2)
        s!"project should have 2 issues, got: {branch.children.size}"
  | none =>
    ensure false "expected project branch"

test "buildTreeData groups by status" := do
  let issues := #[
    { id := 1, title := "Open Issue", status := .open_, priority := .medium,
      created := "2026-01-01", updated := "2026-01-01", labels := #[],
      assignee := none, project := none, blocks := #[],
      blockedBy := #[], description := "", progress := #[] },
    { id := 2, title := "In Progress", status := .inProgress, priority := .medium,
      created := "2026-01-01", updated := "2026-01-01", labels := #[],
      assignee := none, project := none, blocks := #[],
      blockedBy := #[], description := "", progress := #[] }
  ]
  let treeData := buildTreeData issues .byStatus false
  ensure (treeData.size == 2) s!"expected 2 status branches, got {treeData.size}"

-- Test with dynWidget wrapper and state updates on every key event (like real tracker)
test "tree inside dynWidget responds to keys with state churn" := do
  let env ← SpiderEnv.new
  let (treeResultDyn, inputs) ← (do
    let (events, inputs) ← createInputs

    let (resultDyn, _render) ← (runWidget do
      -- Simulate state updates on every key event, without changing viewMode.
      let keyEvents ← useKeyEventW
      let keyCountDyn ← foldDyn (fun _ n => n + 1) 0 keyEvents

      let baseState : AppState := {
        config := default
        issues := mockIssues
        viewMode := .tree
        treeViewMode := .byProject
        showClosed := false
      }
      let stateDyn ← keyCountDyn.map' fun n =>
        { baseState with statusMessage := s!"tick {n}" }

      let viewModeDyn ← stateDyn.mapUniq' (·.viewMode)
      let issuesDyn ← stateDyn.mapUniq' (·.issues)

      column' (gap := 0) {} do
        text' "Header" {}
        -- Use dynWidget like the real tracker app
        dynWidget viewModeDyn fun _viewMode => do
          let treeDataDyn ← issuesDyn.map' (fun issues =>
            buildTreeData issues .byProject false)
          forestDyn' treeDataDyn { globalKeys := true }
    ).run events

    pure (resultDyn, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Get the tree result from the dynamic
  let treeResult ← treeResultDyn.sample

  -- Initial selection
  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some label =>
    ensure (containsSubstr label "TestProject") s!"should start at project, got: {label}"
  | none =>
    ensure false "should have initial selection"

  -- Press down arrow
  inputs.fireKey { event := KeyEvent.down, focusedWidget := none }

  -- Should move to first issue
  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some label =>
    ensure (containsSubstr label "#1") s!"should move to first issue, got: {label}"
  | none =>
    ensure false "should have selection after down arrow"

  env.currentScope.dispose

end TrackerTests.TUI
