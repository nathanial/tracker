/-
  TUI main application using Terminus.Reactive.
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

/-- Caption style -/
def captionStyle : Style := { fg := .ansi .brightBlack }

/-- Body style -/
def bodyStyle : Style := {}

/-! ## Tree Building -/

/-- Build tree nodes from issues based on view mode -/
def buildTreeNodes (issues : Array Issue) (mode : TreeViewMode) (showClosed : Bool)
    : Array (TreeNode String) := Id.run do
  let filtered := if showClosed then issues
                  else issues.filter (·.status != .closed)

  match mode with
  | .byProject =>
    -- Group by project
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
      nodes := nodes.push (TreeNode.branch s!"{project} ({projectIssues.size})" leaves)
    nodes

  | .byStatus =>
    let openIssues := filtered.filter (·.status == .open_)
    let inProgressIssues := filtered.filter (·.status == .inProgress)
    let closedIssues := if showClosed then filtered.filter (·.status == .closed) else #[]

    let mut nodes : Array (TreeNode String) := #[]
    if !openIssues.isEmpty then
      let leaves := openIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
      nodes := nodes.push (TreeNode.branch s!"Open ({openIssues.size})" leaves)
    if !inProgressIssues.isEmpty then
      let leaves := inProgressIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
      nodes := nodes.push (TreeNode.branch s!"In Progress ({inProgressIssues.size})" leaves)
    if showClosed && !closedIssues.isEmpty then
      let leaves := closedIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
      nodes := nodes.push (TreeNode.branch s!"Closed ({closedIssues.size})" leaves)
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
        statusBranches := statusBranches.push (TreeNode.branch s!"Open ({openIssues.size})" leaves)
      if !inProgressIssues.isEmpty then
        let leaves := inProgressIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
        statusBranches := statusBranches.push (TreeNode.branch s!"In Progress ({inProgressIssues.size})" leaves)
      if showClosed && !closedIssues.isEmpty then
        let leaves := closedIssues.map fun issue => TreeNode.leaf (AppState.issueLabel issue)
        statusBranches := statusBranches.push (TreeNode.branch s!"Closed ({closedIssues.size})" leaves)

      if !statusBranches.isEmpty then
        nodes := nodes.push (TreeNode.branch s!"{project} ({projectIssues.size})" statusBranches)
    nodes

/-! ## Text Input Handler -/

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

    -- State ref for imperative updates from IO
    let stateRef ← SpiderM.liftIO (IO.mkRef initialState)

    -- Trigger events for state updates
    let (stateUpdateEvent, fireStateUpdate) ← newTriggerEvent (a := AppState)
    let stateDyn ← holdDyn initialState stateUpdateEvent

    -- Get key events
    let keyEvents ← useKeyEvent

    -- Handle key events
    let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
      let state ← stateRef.get
      let ke := kd.event

      let newState ← match state.viewMode with
      | .tree =>
        match ke.code with
        | .char 'q' | .char 'Q' =>
          IO.Process.exit 0
        | .tab =>
          if ke.modifiers.shift then
            pure state.prevTreeViewMode
          else
            pure state.nextTreeViewMode
        | .char 'c' | .char 'C' =>
          pure state.toggleShowClosed
        | .char 'n' | .char 'N' =>
          pure state.enterCreate
        | .char 'r' | .char 'R' =>
          let issues ← Storage.loadAllIssues config
          pure { state with issues, statusMessage := s!"Loaded {issues.size} issues" }
        | _ => pure state

      | .detail =>
        match ke.code with
        | .char 'q' | .char 'Q' =>
          IO.Process.exit 0
        | .escape =>
          pure state.returnToTree
        | .char 'e' | .char 'E' =>
          pure state.enterEdit
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
        | .escape =>
          pure state.cancelForm
        | .tab =>
          if ke.modifiers.shift then
            pure (state.updateForm FormState.prevField)
          else
            pure (state.updateForm FormState.nextField)
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
        | _ =>
          pure (handleFormInput state ke)

      stateRef.set newState
      fireStateUpdate newState

    -- Build the widget tree
    let (_, render) ← runWidget do
      -- Derive dynamics from state
      let viewModeDyn ← stateDyn.map' (·.viewMode)
      let issuesDyn ← stateDyn.map' (·.issues)
      let treeModeDyn ← stateDyn.map' (·.treeViewMode)
      let showClosedDyn ← stateDyn.map' (·.showClosed)
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
                -- Mode tabs
                row' (gap := 0) {} do
                  let modeIdx ← treeModeDyn.map' fun m =>
                    match m with
                    | .byProject => 0
                    | .byStatus => 1
                    | .byProjectStatus => 2
                  let idx ← sample modeIdx.current
                  let _ ← tabs' #["By Project", "By Status", "By Project+Status"] idx
                    { globalKeys := false, activeStyle := { fg := .ansi .cyan, modifier := { bold := true } } }
                  pure ()

                -- Tree
                let issues ← sample issuesDyn.current
                let mode ← sample treeModeDyn.current
                let showClosed ← sample showClosedDyn.current
                let nodes := buildTreeNodes issues mode showClosed

                if nodes.isEmpty then
                  text' "No issues found. Press 'n' to create one." captionStyle
                else
                  let treeResult ← forest' nodes { globalKeys := true }

                  -- Handle selection
                  let _unsub ← SpiderM.liftIO <| treeResult.onSelect.subscribe fun label => do
                    match AppState.parseIssueIdFromLabel label with
                    | some id =>
                      let state ← stateRef.get
                      match state.issues.find? (·.id == id) with
                      | some issue =>
                        let newState := state.enterDetailFor issue
                        stateRef.set newState
                        fireStateUpdate newState
                      | none => pure ()
                    | none => pure ()
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
