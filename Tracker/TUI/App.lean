/-
  TUI main application using Terminus.Reactive.
  Uses idiomatic FRP patterns with foldDynM for state management.
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Tracker.TUI.State
import Tracker.TUI.Draw
import Tracker.TUI.Update
import Terminus
import Terminus.Reactive

namespace Tracker.TUI

open Tracker
open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Styles -/

def captionStyle : Style := { fg := .ansi .brightBlack }
def bodyStyle : Style := {}

/-! ## Tree Building -/

/-- Create a branch node with expansion state from the tracked array -/
private def makeBranch (label : String) (children : Array (TreeNode String))
    (key : String) (expanded : Array String) : TreeNode String :=
  if expanded.contains key then
    TreeNode.branch label children
  else
    TreeNode.collapsedBranch label children

/-- Build tree nodes from issues based on view mode, preserving expansion state -/
def buildTreeNodes (issues : Array Issue) (mode : TreeViewMode) (showClosed : Bool)
    (expanded : Array String) : Array (TreeNode String) := Id.run do
  let filtered := if showClosed then issues
                  else issues.filter (·.status != .closed)

  match mode with
  | .byProject =>
    let mut projectMap : List (String × Array Issue) := []
    for issue in filtered do
      let key := issue.project.getD "No Project"
      match projectMap.find? (·.1 == key) with
      | some _ =>
        projectMap := projectMap.map fun (k, v) =>
          if k == key then (k, v.push issue) else (k, v)
      | none =>
        projectMap := projectMap ++ [(key, #[issue])]

    let sorted := projectMap.toArray.qsort (·.1 < ·.1)
    let mut nodes : Array (TreeNode String) := #[]
    for (project, projectIssues) in sorted do
      let leaves := projectIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
      let label := s!"{project} ({projectIssues.size})"
      let key := project  -- Use project name as the expansion key
      nodes := nodes.push (makeBranch label leaves key expanded)
    nodes

  | .byStatus =>
    let openIssues := filtered.filter (·.status == .open_)
    let inProgressIssues := filtered.filter (·.status == .inProgress)
    let closedIssues := if showClosed then filtered.filter (·.status == .closed) else #[]

    let mut nodes : Array (TreeNode String) := #[]
    if !openIssues.isEmpty then
      let leaves := openIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
      let label := s!"Open ({openIssues.size})"
      nodes := nodes.push (makeBranch label leaves "Open" expanded)
    if !inProgressIssues.isEmpty then
      let leaves := inProgressIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
      let label := s!"In Progress ({inProgressIssues.size})"
      nodes := nodes.push (makeBranch label leaves "In Progress" expanded)
    if showClosed && !closedIssues.isEmpty then
      let leaves := closedIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
      let label := s!"Closed ({closedIssues.size})"
      nodes := nodes.push (makeBranch label leaves "Closed" expanded)
    nodes

  | .byProjectStatus =>
    let mut projectMap : List (String × Array Issue) := []
    for issue in filtered do
      let key := issue.project.getD "No Project"
      match projectMap.find? (·.1 == key) with
      | some _ =>
        projectMap := projectMap.map fun (k, v) =>
          if k == key then (k, v.push issue) else (k, v)
      | none =>
        projectMap := projectMap ++ [(key, #[issue])]

    let sorted := projectMap.toArray.qsort (·.1 < ·.1)
    let mut nodes : Array (TreeNode String) := #[]
    for (project, projectIssues) in sorted do
      let openIssues := projectIssues.filter (·.status == .open_)
      let inProgressIssues := projectIssues.filter (·.status == .inProgress)
      let closedIssues := if showClosed then projectIssues.filter (·.status == .closed) else #[]

      let mut statusBranches : Array (TreeNode String) := #[]
      if !openIssues.isEmpty then
        let leaves := openIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
        let label := s!"Open ({openIssues.size})"
        let key := s!"{project}/Open"
        statusBranches := statusBranches.push (makeBranch label leaves key expanded)
      if !inProgressIssues.isEmpty then
        let leaves := inProgressIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
        let label := s!"In Progress ({inProgressIssues.size})"
        let key := s!"{project}/In Progress"
        statusBranches := statusBranches.push (makeBranch label leaves key expanded)
      if showClosed && !closedIssues.isEmpty then
        let leaves := closedIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
        let label := s!"Closed ({closedIssues.size})"
        let key := s!"{project}/Closed"
        statusBranches := statusBranches.push (makeBranch label leaves key expanded)

      if !statusBranches.isEmpty then
        let label := s!"{project} ({projectIssues.size})"
        nodes := nodes.push (makeBranch label statusBranches project expanded)
    nodes

/-! ## Events -/

/-- Raw events that can trigger state changes -/
inductive AppEvent where
  | key (kd : KeyData)
  | refresh
  | selectIssue (id : Nat)
  | toggleBranch (key : String)  -- Toggle expansion state for a branch
  | updateSelection (label : Option String)  -- Track current tree selection
  deriving Inhabited

/-! ## Form Input Handler -/

/-- Handle text input for form fields -/
def handleFormInput (state : AppState) (ke : KeyEvent) : AppState :=
  let form := state.formState
  if form.focusedField == .priority then
    match ke.code with
    | KeyCode.left => state.updateForm FormState.cyclePriorityDown
    | KeyCode.right => state.updateForm FormState.cyclePriorityUp
    | _ => state
  else
    match ke.code with
    | KeyCode.char c =>
      state.updateForm fun f =>
        f.updateCurrentInput (f.currentInput.insertChar c)
    | KeyCode.backspace =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.backspace
    | KeyCode.delete =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.delete
    | KeyCode.left =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveLeft
    | KeyCode.right =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveRight
    | KeyCode.home =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveToStart
    | KeyCode.«end» =>
      state.updateForm fun f =>
        f.updateCurrentInput f.currentInput.moveToEnd
    | _ => state

/-! ## Event Processing -/

/-- Process an event, returning the new state. Handles key mapping internally. -/
def processEvent (config : Storage.Config) (event : AppEvent) (state : AppState) : IO AppState := do
  match event with
  | .refresh =>
    let issues ← Storage.loadAllIssues config
    pure { state with issues }

  | .toggleBranch key =>
    pure (state.toggleExpanded key)

  | .updateSelection label =>
    pure { state with selectedLabel := label }

  | .selectIssue id =>
    match state.issues.find? (·.id == id) with
    | some issue => pure (state.enterDetailFor issue)
    | none => pure state

  | .key kd =>
    let ke := kd.event
    match state.viewMode with
    | .tree =>
      match ke.code with
      | .char 'q' | .char 'Q' => IO.Process.exit 0
      | .tab =>
        if ke.modifiers.shift then pure state.prevTreeViewMode
        else pure state.nextTreeViewMode
      | .char 'h' | .char 'H' => pure state.toggleShowClosed
      | .char 'n' | .char 'N' => pure state.enterCreate
      | .char 'r' | .char 'R' =>
        let issues ← Storage.loadAllIssues config
        pure { state with issues, statusMessage := s!"Refreshed {issues.size} issues" }
      | _ => pure state

    | .detail =>
      match ke.code with
      | .char 'q' | .char 'Q' => IO.Process.exit 0
      | .escape => pure state.returnToTree
      | .char 'e' | .char 'E' => pure state.enterEdit
      | .char 'c' | .char 'C' =>
        match state.currentIssue with
        | some issue =>
          if issue.status != .closed then
            let _ ← Storage.closeIssue config issue.id none
            let issues ← Storage.loadAllIssues config
            let updatedIssue := issues.find? (·.id == issue.id)
            pure { state with issues, currentIssue := updatedIssue, statusMessage := s!"Issue #{issue.id} closed" }
          else
            pure (state.setError "Issue already closed")
        | none => pure state
      | .char 'r' | .char 'R' =>
        match state.currentIssue with
        | some issue =>
          if issue.status == .closed then
            let _ ← Storage.reopenIssue config issue.id
            let issues ← Storage.loadAllIssues config
            let updatedIssue := issues.find? (·.id == issue.id)
            pure { state with issues, currentIssue := updatedIssue, statusMessage := s!"Issue #{issue.id} reopened" }
          else
            pure (state.setError "Issue is not closed")
        | none => pure state
      | _ => pure state

    | .create | .edit =>
      match ke.code with
      | .escape => pure state.cancelForm
      | .tab =>
        if ke.modifiers.shift then pure (state.updateForm FormState.prevField)
        else pure (state.updateForm FormState.nextField)
      | .char 's' | .char 'S' =>
        if ke.modifiers.ctrl then
          let form := state.formState
          if form.isValid then
            let title := form.title.text.trim
            let description := form.description.text.trim
            let priority := form.priority
            let labels := form.getLabels
            let assignee := form.getAssignee
            let project := form.getProject
            match form.editingIssueId with
            | some id =>
              let _ ← Storage.updateIssue config id fun issue =>
                { issue with title, description, priority, labels, assignee, project }
              let issues ← Storage.loadAllIssues config
              let updatedIssue := issues.find? (·.id == id)
              pure { state with issues, viewMode := .detail, currentIssue := updatedIssue, statusMessage := s!"Updated issue #{id}" }
            | none =>
              let newIssue ← Storage.createIssue config title description priority labels assignee project
              let issues ← Storage.loadAllIssues config
              pure { state with issues, viewMode := .detail, currentIssue := some newIssue, statusMessage := s!"Created issue #{newIssue.id}" }
          else
            pure (state.setError "Title is required")
        else
          pure (handleFormInput state ke)
      | _ => pure (handleFormInput state ke)

/-! ## Expansion Key Computation -/

/-- Get the label of a node at the given path in a tree forest -/
private def getNodeLabelAtPath (nodes : Array (TreeNode String)) (path : Array Nat) : Option String := do
  match path.toList with
  | [] => none
  | [i] => nodes[i]?.map (·.value)
  | i :: rest =>
    let node ← nodes[i]?
    getChildLabelAtPath node.children rest
where
  getChildLabelAtPath (children : Array (TreeNode String)) : List Nat → Option String
    | [] => none
    | [i] => children[i]?.map (·.value)
    | i :: rest => do
      let node ← children[i]?
      getChildLabelAtPath node.children rest

/-- Compute the expansion key for a toggled branch at the given path.
    The key format depends on the tree view mode:
    - byProject: project name (e.g., "Reactive")
    - byStatus: status name (e.g., "Open")
    - byProjectStatus: "project/status" for nested (e.g., "Reactive/Open") -/
def computeExpansionKey (nodes : Array (TreeNode String)) (mode : TreeViewMode) (path : Array Nat)
    : Option String := do
  match mode with
  | .byProject =>
    -- Single level: path is [projectIndex], key is project name
    let label ← getNodeLabelAtPath nodes path
    some (AppState.baseLabel label)
  | .byStatus =>
    -- Single level: path is [statusIndex], key is status name
    let label ← getNodeLabelAtPath nodes path
    some (AppState.baseLabel label)
  | .byProjectStatus =>
    -- Two levels: path can be [projectIdx] or [projectIdx, statusIdx]
    match path.toList with
    | [i] =>
      -- Toggling a project branch
      let label ← nodes[i]?.map (·.value)
      some (AppState.baseLabel label)
    | [i, j] =>
      -- Toggling a status branch within a project
      -- Use bounds-safe access to handle any potential index mismatch
      if h : i < nodes.size then
        let projectNode := nodes[i]
        let projectLabel := AppState.baseLabel projectNode.value
        if h2 : j < projectNode.children.size then
          let statusNode := projectNode.children[j]
          let statusLabel := AppState.baseLabel statusNode.value
          some s!"{projectLabel}/{statusLabel}"
        else
          -- Fallback: just use project key if status lookup fails
          some projectLabel
      else
        none
    | _ => none

/-! ## Main App -/

/-- Run the TUI application -/
def run (config : Storage.Config) : IO Unit := do
  runReactiveApp do
    -- Load initial issues
    let initialIssues ← SpiderM.liftIO (Storage.loadAllIssues config)

    -- Create initial state
    let initialState : AppState := {
      config
      issues := initialIssues
      viewMode := .tree
      treeViewMode := .byProject
      showClosed := false
    }

    -- Get event sources
    let keyEvents ← useKeyEvent
    let tickEvents ← useTick

    -- Trigger event for tree selection (forest' widget fires this)
    let (selectIssueEvent, fireSelectIssue) ← newTriggerEvent (a := Nat)

    -- Trigger event for branch toggle (forest' widget fires this)
    let (toggleBranchEvent, fireToggleBranch) ← newTriggerEvent (a := String)

    -- Trigger event for tracking current selection (for restoring after rebuild)
    let (selectionUpdateEvent, fireSelectionUpdate) ← newTriggerEvent (a := Option String)

    -- Map raw events to AppEvent
    let keyAppEvents ← Event.mapM (fun kd => AppEvent.key kd) keyEvents

    -- Periodic refresh: filter tick events to every 5 seconds
    let refreshAppEvents ← Event.mapMaybeM (fun (td : TickData) =>
      if td.elapsedMs % 5000 < 50 then some AppEvent.refresh else none) tickEvents

    -- Map tree selection to AppEvent
    let selectAppEvents ← Event.mapM (fun id => AppEvent.selectIssue id) selectIssueEvent

    -- Map branch toggle to AppEvent
    let toggleAppEvents ← Event.mapM (fun key => AppEvent.toggleBranch key) toggleBranchEvent

    -- Map selection updates to AppEvent
    let selectionAppEvents ← Event.mapM (fun label => AppEvent.updateSelection label) selectionUpdateEvent

    -- Merge all event streams
    let events1 ← Event.mergeM keyAppEvents refreshAppEvents
    let events2 ← Event.mergeM events1 selectAppEvents
    let events3 ← Event.mergeM events2 toggleAppEvents
    let allEvents ← Event.mergeM events3 selectionAppEvents

    -- Fold events into state using foldDynM (idiomatic FRP)
    -- Key-to-action mapping happens inside processEvent with access to current state
    let stateDyn ← foldDynM (fun event state => SpiderM.liftIO do
      processEvent config event state
    ) initialState allEvents

    -- Build the widget tree
    let (_, render) ← runWidget do
      -- Derive dynamics from state
      let viewModeDyn ← stateDyn.map' (·.viewMode)
      let issuesDyn ← stateDyn.map' (·.issues)
      let treeModeDyn ← stateDyn.map' (·.treeViewMode)
      let showClosedDyn ← stateDyn.map' (·.showClosed)
      let expandedBranchesDyn ← stateDyn.map' (·.expandedBranches)
      let selectedLabelDyn ← stateDyn.map' (·.selectedLabel)
      let currentIssueDyn ← stateDyn.map' (·.currentIssue)
      let formStateDyn ← stateDyn.map' (·.formState)
      let statusMsgDyn ← stateDyn.map' (·.statusMessage)
      let errorMsgDyn ← stateDyn.map' (·.errorMessage)

      dockBottom' (footerHeight := 1)
        -- Main content
        (do
          column' (gap := 0) {} do
            -- Header
            row' (gap := 1) {} do
              text' " Tracker " { fg := .ansi .cyan, modifier := { bold := true } }
              let closedNode ← showClosedDyn.map' fun sc =>
                RNode.text (if sc then "[showing closed]" else "") captionStyle
              emit closedNode

            -- View content
            let _ ← dynWidget viewModeDyn fun viewMode => do
              match viewMode with
              | .tree => do
                -- Mode tabs - reactive to treeModeDyn
                let tabsNode ← treeModeDyn.map' fun m =>
                  let idx := match m with
                    | .byProject => 0
                    | .byStatus => 1
                    | .byProjectStatus => 2
                  let labels := #["By Project", "By Status", "By Project+Status"]
                  let tabsText := Id.run do
                    let mut result := ""
                    for i in [:labels.size] do
                      let label := labels[i]!
                      result := result ++ (if i == idx then s!" [{label}] " else s!"  {label}  ")
                    result
                  RNode.text tabsText { fg := .ansi .cyan }
                emit tabsNode

                -- Tree - rebuild only when issues, mode, or showClosed change
                -- Expansion and selection state attached via attachWithM so they don't trigger rebuild
                let issueModeDyn ← issuesDyn.zipWith' (fun a b => (a, b)) treeModeDyn
                let dataChangeDyn ← issueModeDyn.zipWith' (fun (a, b) c => (a, b, c)) showClosedDyn

                -- Combine expansion and selection behaviors
                let treeStateBehavior := Behavior.zip
                  expandedBranchesDyn.current
                  selectedLabelDyn.current

                -- Attach current tree state when data changes (not when tree state changes)
                let rebuildEvent ← Event.attachWithM
                  (fun (expanded, selected) (issues, mode, showClosed) =>
                    (issues, mode, showClosed, expanded, selected))
                  treeStateBehavior
                  dataChangeDyn.updated

                -- Use initial values from app startup for holdDyn
                let treeInputDyn ← holdDyn (initialIssues, TreeViewMode.byProject, false, #[], none) rebuildEvent

                let _ ← dynWidget treeInputDyn fun (issues, mode, showClosed, expanded, selected) => do
                  let nodes := buildTreeNodes issues mode showClosed expanded

                  if nodes.isEmpty then
                    text' "No issues found. Press 'n' to create one." captionStyle
                  else
                    let treeResult ← forest' nodes { globalKeys := true, initialSelection := selected }

                    -- Handle tree selection by firing trigger event
                    let selectEvent ← Event.mapMaybeM (fun label =>
                      AppState.parseIssueIdFromLabel label) treeResult.onSelect
                    let fireIO ← Event.mapM (fun id => (fireSelectIssue id : IO Unit)) selectEvent
                    performEvent_ fireIO

                    -- Handle branch toggle by extracting key from tree state and firing toggle
                    -- The path gives us indices; we need to map to the expansion key
                    let toggleKeyEvent ← Event.mapMaybeM (fun (path : Array Nat) => do
                      -- Get the node label at this path and compute its expansion key
                      computeExpansionKey nodes mode path) treeResult.onToggle
                    let fireToggleIO ← Event.mapM (fun key => (fireToggleBranch key : IO Unit)) toggleKeyEvent
                    performEvent_ fireToggleIO

                    -- Track selection changes for restoring after rebuild
                    let selectionIO ← Event.mapM (fun label => (fireSelectionUpdate label : IO Unit)) treeResult.selectedNode.updated
                    performEvent_ selectionIO
                    pure ()
                pure ()

              | .detail => do
                let issueOpt : Option Issue ← sample currentIssueDyn.current
                match issueOpt with
                | some issue =>
                  column' (gap := 1) {} do
                    -- Title
                    text' s!"#{issue.id}: {issue.title}" { modifier := { bold := true } }

                    -- Status and Priority
                    let statusStr := statusText issue.status
                    let prioStr := priorityText issue.priority
                    row' (gap := 2) {} do
                      text' "Status: " captionStyle
                      text' statusStr bodyStyle
                      text' "  Priority: " captionStyle
                      text' prioStr bodyStyle

                    -- Timestamps
                    text' s!"Created: {issue.created}  |  Updated: {issue.updated}" captionStyle

                    -- Labels
                    if !issue.labels.isEmpty then
                      text' s!"Labels: {String.intercalate ", " issue.labels.toList}" bodyStyle

                    -- Assignee
                    if let some assignee := issue.assignee then
                      text' s!"Assignee: @{assignee}" bodyStyle

                    -- Project
                    if let some project := issue.project then
                      text' s!"Project: {project}" bodyStyle

                    -- Blocking relationships
                    if !issue.blockedBy.isEmpty then
                      text' s!"Blocked by: {String.intercalate ", " (issue.blockedBy.map (s!"#{·}")).toList}" { fg := .ansi .red }
                    if !issue.blocks.isEmpty then
                      text' s!"Blocks: {String.intercalate ", " (issue.blocks.map (s!"#{·}")).toList}" bodyStyle

                    -- Description
                    if !issue.description.isEmpty then
                      spacer' 1 1
                      text' "Description:" { modifier := { bold := true } }
                      text' issue.description bodyStyle

                    -- Progress entries
                    if !issue.progress.isEmpty then
                      spacer' 1 1
                      text' "Progress:" { modifier := { bold := true } }
                      for entry in issue.progress do
                        text' s!"  [{entry.timestamp}] {entry.message}" captionStyle

                | none =>
                  text' "No issue selected." captionStyle

              | .create | .edit => do
                let form : FormState ← sample formStateDyn.current
                let formTitle := if form.editingIssueId.isSome then "Edit Issue" else "New Issue"
                column' (gap := 1) {} do
                  text' formTitle { modifier := { bold := true } }
                  spacer' 1 1

                  -- Title field
                  let titleFocused := form.focusedField == FormField.title
                  text' "Title:" (if titleFocused then { fg := .ansi .cyan, modifier := { bold := true } } else bodyStyle)
                  text' s!"  {form.title.text}" (if titleFocused then { fg := .ansi .white } else captionStyle)

                  -- Description field
                  let descFocused := form.focusedField == FormField.description
                  text' "Description:" (if descFocused then { fg := .ansi .cyan, modifier := { bold := true } } else bodyStyle)
                  text' s!"  {form.description.text}" (if descFocused then { fg := .ansi .white } else captionStyle)

                  -- Priority selector
                  let prioFocused := form.focusedField == FormField.priority
                  text' "Priority: (use ←/→ to change)" (if prioFocused then { fg := .ansi .cyan, modifier := { bold := true } } else bodyStyle)
                  text' s!"  {priorityText form.priority}" (if prioFocused then { fg := .ansi .white } else captionStyle)

                  -- Labels field
                  let labelsFocused := form.focusedField == FormField.labels
                  text' "Labels (comma-separated):" (if labelsFocused then { fg := .ansi .cyan, modifier := { bold := true } } else bodyStyle)
                  text' s!"  {form.labels.text}" (if labelsFocused then { fg := .ansi .white } else captionStyle)

                  -- Assignee field
                  let assigneeFocused := form.focusedField == FormField.assignee
                  text' "Assignee:" (if assigneeFocused then { fg := .ansi .cyan, modifier := { bold := true } } else bodyStyle)
                  text' s!"  {form.assignee.text}" (if assigneeFocused then { fg := .ansi .white } else captionStyle)

                  -- Project field
                  let projectFocused := form.focusedField == FormField.project
                  text' "Project:" (if projectFocused then { fg := .ansi .cyan, modifier := { bold := true } } else bodyStyle)
                  text' s!"  {form.project.text}" (if projectFocused then { fg := .ansi .white } else captionStyle)

                  -- Validation message
                  if !form.isValid then
                    spacer' 1 1
                    text' "Title is required" { fg := .ansi .red }
        )
        -- Footer
        (do
          row' (gap := 2) {} do
            let helpNode ← viewModeDyn.map' fun vm =>
              match vm with
              | .tree => RNode.text treeViewHelp captionStyle
              | .detail => RNode.text detailViewHelp captionStyle
              | .create | .edit => RNode.text formViewHelp captionStyle
            emit helpNode
            let msgNode ← statusMsgDyn.zipWith' (fun status error =>
              if !status.isEmpty then RNode.text status { fg := .ansi .green }
              else if !error.isEmpty then RNode.text error { fg := .ansi .red }
              else RNode.empty
            ) errorMsgDyn
            emit msgNode
        )

    pure { render }

end Tracker.TUI
