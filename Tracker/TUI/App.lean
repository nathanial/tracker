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

/-- Build tree data from issues based on view mode.
    Returns pure structural data without embedded expansion state.
    The tree widget handles expansion internally. -/
def buildTreeData (issues : Array Issue) (mode : TreeViewMode) (showClosed : Bool)
    : Array (TreeData String) := Id.run do
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
    let mut nodes : Array (TreeData String) := #[]
    for (project, projectIssues) in sorted do
      let leaves := projectIssues.map fun issue => TreeData.leaf (AppState.issueLabel issue)
      let label := s!"{project} ({projectIssues.size})"
      nodes := nodes.push (TreeData.branch label leaves)
    nodes

  | .byStatus =>
    let openIssues := filtered.filter (·.status == .open_)
    let inProgressIssues := filtered.filter (·.status == .inProgress)
    let closedIssues := if showClosed then filtered.filter (·.status == .closed) else #[]

    let mut nodes : Array (TreeData String) := #[]
    if !openIssues.isEmpty then
      let leaves := openIssues.map fun issue => TreeData.leaf (AppState.issueLabel issue)
      let label := s!"Open ({openIssues.size})"
      nodes := nodes.push (TreeData.branch label leaves)
    if !inProgressIssues.isEmpty then
      let leaves := inProgressIssues.map fun issue => TreeData.leaf (AppState.issueLabel issue)
      let label := s!"In Progress ({inProgressIssues.size})"
      nodes := nodes.push (TreeData.branch label leaves)
    if showClosed && !closedIssues.isEmpty then
      let leaves := closedIssues.map fun issue => TreeData.leaf (AppState.issueLabel issue)
      let label := s!"Closed ({closedIssues.size})"
      nodes := nodes.push (TreeData.branch label leaves)
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
    let mut nodes : Array (TreeData String) := #[]
    for (project, projectIssues) in sorted do
      let openIssues := projectIssues.filter (·.status == .open_)
      let inProgressIssues := projectIssues.filter (·.status == .inProgress)
      let closedIssues := if showClosed then projectIssues.filter (·.status == .closed) else #[]

      let mut statusBranches : Array (TreeData String) := #[]
      if !openIssues.isEmpty then
        let leaves := openIssues.map fun issue => TreeData.leaf (AppState.issueLabel issue)
        let label := s!"Open ({openIssues.size})"
        statusBranches := statusBranches.push (TreeData.branch label leaves)
      if !inProgressIssues.isEmpty then
        let leaves := inProgressIssues.map fun issue => TreeData.leaf (AppState.issueLabel issue)
        let label := s!"In Progress ({inProgressIssues.size})"
        statusBranches := statusBranches.push (TreeData.branch label leaves)
      if showClosed && !closedIssues.isEmpty then
        let leaves := closedIssues.map fun issue => TreeData.leaf (AppState.issueLabel issue)
        let label := s!"Closed ({closedIssues.size})"
        statusBranches := statusBranches.push (TreeData.branch label leaves)

      if !statusBranches.isEmpty then
        let label := s!"{project} ({projectIssues.size})"
        nodes := nodes.push (TreeData.branch label statusBranches)
    nodes

/-! ## Events -/

/-- Raw events that can trigger state changes -/
inductive AppEvent where
  | key (kd : KeyData)
  | refresh
  | selectIssue (id : Nat)
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

/-- Ensure terminal is restored before exiting (Process.exit skips runReactiveApp teardown). -/
def quitApp : IO AppState := do
  Terminal.teardown
  IO.Process.exit 0

/-- Process an event, returning the new state. Handles key mapping internally. -/
def processEvent (config : Storage.Config) (event : AppEvent) (state : AppState) : IO AppState := do
  match event with
  | .refresh =>
    let issues ← Storage.loadAllIssues config
    pure { state with issues }

  | .selectIssue id =>
    match state.issues.find? (·.id == id) with
    | some issue => pure (state.enterDetailFor issue)
    | none => pure state

  | .key kd =>
    let ke := kd.event
    match state.viewMode with
    | .tree =>
      match ke.code with
      | .char 'q' | .char 'Q' => quitApp
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
      | .char 'q' | .char 'Q' => quitApp
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

/-! ## View Widgets -/

/-- Header widget showing app title and closed issues indicator -/
def headerWidget (showClosedDyn : Dyn Bool) : WidgetM Unit := do
  row' (gap := 1) {} do
    text' " Tracker " { fg := .ansi .cyan, modifier := { bold := true } }
    let closedNode ← showClosedDyn.map' fun sc =>
      RNode.text (if sc then "[showing closed]" else "") captionStyle
    emit closedNode

/-- Footer widget showing help text and status/error messages -/
def footerWidget (viewModeDyn : Dyn ViewMode) (statusMsgDyn errorMsgDyn errorSuggDyn : Dyn String)
    : WidgetM Unit := do
  row' (gap := 2) {} do
    let helpNode ← viewModeDyn.map' fun vm =>
      match vm with
      | .tree => RNode.text treeViewHelp captionStyle
      | .detail => RNode.text detailViewHelp captionStyle
      | .create | .edit => RNode.text formViewHelp captionStyle
    emit helpNode
    let errorPairDyn ← errorMsgDyn.zipWith' (·, ·) errorSuggDyn
    let msgNode ← statusMsgDyn.zipWith' (fun status (error, sugg) =>
      if !status.isEmpty then RNode.text status { fg := .ansi .green }
      else if !error.isEmpty then
        let hint := if sugg.isEmpty then "" else s!" ({sugg})"
        RNode.text (error ++ hint) { fg := .ansi .red }
      else RNode.empty
    ) errorPairDyn
    emit msgNode

/-- Tree view widget showing issues organized by project/status -/
def treeViewWidget (issuesDyn : Dyn (Array Issue)) (treeModeDyn : Dyn TreeViewMode)
    (showClosedDyn : Dyn Bool) (fireSelectIssue : Nat → IO Unit) : WidgetM Unit := do
  -- Get terminal height dynamically for tree scrolling
  let resizeEvent ← useResizeW
  let heightDyn ← holdDyn 24 (← Event.mapM (·.height) resizeEvent)
  -- Reserve space for header/tabs/help/status (about 6 lines)
  let maxVisibleDyn ← heightDyn.map' (fun h => if h > 8 then h - 6 else 2)

  -- Mode tabs - reactive to treeModeDyn
  let tabsNode ← treeModeDyn.mapUniq' fun m =>
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

  -- Build tree data dynamic from state components
  let treeTupleDyn ← issuesDyn.zipWith' (fun a b => (a, b)) treeModeDyn
  let treeDataDyn ← treeTupleDyn.zipWith' (fun (issues, mode) showClosed =>
    buildTreeData issues mode showClosed) showClosedDyn

  -- Check if empty and render appropriately
  let isEmptyDyn ← treeDataDyn.mapUniq' (·.isEmpty)
  let _ ← dynWidget isEmptyDyn fun isEmpty => do
    if isEmpty then
      text' "No issues found. Press 'n' to create one." captionStyle
    else
      -- Use the dynamic tree widget - state persists across data updates!
      let treeResult ← forestDyn' treeDataDyn { globalKeys := true, expandByDefault := false, maxVisible := some maxVisibleDyn }

      -- Handle tree selection by firing trigger event
      let selectEvent ← Event.mapMaybeM (fun label =>
        AppState.parseIssueIdFromLabel label) treeResult.onSelect
      let fireIO ← Event.mapM (fun id => (fireSelectIssue id : IO Unit)) selectEvent
      performEvent_ fireIO
      pure ()
  pure ()

/-- Detail view widget showing a single issue's full details -/
def detailViewWidget (currentIssueDyn : Dyn (Option Issue)) : WidgetM Unit := do
  let _ ← dynWidget currentIssueDyn fun issueOpt => do
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
  pure ()

/-- Form view widget for creating/editing issues -/
def formViewWidget (formStateDyn : Dyn FormState) : WidgetM Unit := do
  let _ ← dynWidget formStateDyn fun form => do
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
  pure ()

/-! ## Main App -/

/-- Run the TUI application -/
def run (config : Storage.Config) (debugMode : Bool := false) : IO Unit := do
  let appConfig : Terminus.Reactive.AppConfig := {
    debugDir := if debugMode then some ".debug" else none
  }
  runReactiveApp (config := appConfig) do
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

    -- Trigger event for tree selection
    let (selectIssueEvent, fireSelectIssue) ← newTriggerEvent (a := Nat)

    -- Map raw events to AppEvent
    let keyAppEvents ← Event.mapM (fun kd => AppEvent.key kd) keyEvents

    -- Periodic refresh: filter tick events to every 5 seconds
    let refreshAppEvents ← Event.mapMaybeM (fun (td : TickData) =>
      if td.elapsedMs % 5000 < 50 then some AppEvent.refresh else none) tickEvents

    -- Map tree selection to AppEvent
    let selectAppEvents ← Event.mapM (fun id => AppEvent.selectIssue id) selectIssueEvent

    -- Merge all event streams
    let events1 ← Event.mergeM keyAppEvents refreshAppEvents
    let allEvents ← Event.mergeM events1 selectAppEvents

    -- Fold events into state using foldDynM (idiomatic FRP)
    let stateDyn ← foldDynM (fun event state => SpiderM.liftIO do
      processEvent config event state
    ) initialState allEvents

    -- Build the widget tree
    let (_, render) ← runWidget do
      -- Derive dynamics from state
      let viewModeDyn ← stateDyn.mapUniq' (·.viewMode)
      let issuesDyn ← stateDyn.mapUniq' (·.issues)
      let treeModeDyn ← stateDyn.mapUniq' (·.treeViewMode)
      let showClosedDyn ← stateDyn.mapUniq' (·.showClosed)
      let currentIssueDyn ← stateDyn.mapUniq' (·.currentIssue)
      let formStateDyn ← stateDyn.mapUniq' (·.formState)
      let statusMsgDyn ← stateDyn.mapUniq' (·.statusMessage)
      let errorMsgDyn ← stateDyn.mapUniq' (·.errorMessage)
      let errorSuggDyn ← stateDyn.mapUniq' (·.errorSuggestion)

      dockBottom' (footerHeight := 1)
        -- Main content
        (do
          column' (gap := 0) {} do
            headerWidget showClosedDyn
            let _ ← dynWidget viewModeDyn fun viewMode => do
              match viewMode with
              | .tree => treeViewWidget issuesDyn treeModeDyn showClosedDyn fireSelectIssue
              | .detail => detailViewWidget currentIssueDyn
              | .create | .edit => formViewWidget formStateDyn
        )
        -- Footer
        (footerWidget viewModeDyn statusMsgDyn errorMsgDyn errorSuggDyn)

    pure { render }

end Tracker.TUI
