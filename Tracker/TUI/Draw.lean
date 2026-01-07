/-
  TUI rendering.
-/
import Tracker.Core.Types
import Tracker.Core.Storage
import Tracker.TUI.State
import Terminus

namespace Tracker.TUI

open Tracker
open Terminus

/-- Left-pad a string -/
private def padLeft (s : String) (len : Nat) (c : Char) : String :=
  let padding := len - s.length
  if padding > 0 then
    String.mk (List.replicate padding c) ++ s
  else s

/-- Draw the header with title -/
def drawHeader (buf : Buffer) (state : AppState) (startX startY : Nat) (width : Nat) : Buffer := Id.run do
  let mut buf := buf

  -- Draw title
  let title := " Tracker "
  buf := buf.writeString startX startY title (Style.bold.withFg (.ansi .cyan))

  -- Show view info based on mode
  let mut x := startX + title.length
  match state.viewMode with
  | .tree =>
    -- Show closed filter status
    let closedStatus := if state.showClosed then "[showing closed]" else ""
    if !closedStatus.isEmpty then
      buf := buf.writeString x startY s!" {closedStatus}" (Style.default.withFg (.ansi .brightBlack))
  | .detail =>
    buf := buf.writeString x startY " > Detail" (Style.default.withFg (.ansi .yellow))
  | .create =>
    buf := buf.writeString x startY " > New Issue" (Style.default.withFg (.ansi .yellow))
  | .edit =>
    buf := buf.writeString x startY " > Edit Issue" (Style.default.withFg (.ansi .yellow))

  buf

/-- Draw the tab bar for tree view modes -/
def drawTabs (frame : Frame) (state : AppState) (area : Rect) : Frame := Id.run do
  let tabIdx := match state.treeViewMode with
    | .byProject => 0
    | .byStatus => 1
    | .byProjectStatus => 2

  let tabs := Tabs.new ["By Project", "By Status", "By Project+Status"]
    |>.withSelected tabIdx
    |>.withSelectedStyle (Style.bold.withFg (.ansi .cyan))
    |>.withNormalStyle (Style.default.withFg (.ansi .white))
    |>.withDivider " │ "

  frame.render tabs area

/-- Draw the tree view -/
def drawTreeView (frame : Frame) (state : AppState) (area : Rect) : Frame := Id.run do
  if state.issueTree.nodes.isEmpty then
    let emptyMsg := "No issues found. Press 'n' to create one."
    let buf := frame.buffer.writeString (area.x + 2) (area.y + 1) emptyMsg Style.default
    return { frame with buffer := buf }

  -- Style the tree
  let styledTree := state.issueTree
    |>.withSelectedStyle (Style.reversed.withFg (.ansi .cyan))
    |>.withBranchStyle (Style.bold.withFg (.ansi .yellow))
    |>.withLeafStyle (Style.default.withFg (.ansi .white))
    |>.withPrefixStyle (Style.default.withFg (.ansi .brightBlack))
    |>.withExpandedChar "▼"
    |>.withCollapsedChar "▶"
    |>.withLeafChar "•"

  frame.render styledTree area

/-- Draw issue detail view -/
def drawDetail (buf : Buffer) (state : AppState) (startX startY : Nat) (width height : Nat) : Buffer := Id.run do
  let mut buf := buf
  match state.currentIssue with
  | none =>
    buf := buf.writeString startX startY "No issue selected." Style.default
  | some issue =>
    let mut y := startY

    -- Title
    let title := s!"#{issue.id}: {issue.title}"
    buf := buf.writeString startX y title Style.bold
    y := y + 2

    -- Status and priority
    let statusLine := s!"Status: {issue.status.toString}  |  Priority: {issue.priority.toString}"
    buf := buf.writeString startX y statusLine Style.default
    y := y + 1

    -- Dates
    let dateLine := s!"Created: {issue.created}  |  Updated: {issue.updated}"
    buf := buf.writeString startX y dateLine (Style.default.withFg (.ansi .brightBlack))
    y := y + 1

    -- Labels
    if !issue.labels.isEmpty then
      let labelsLine := s!"Labels: {String.intercalate ", " issue.labels.toList}"
      buf := buf.writeString startX y labelsLine Style.default
      y := y + 1

    -- Assignee
    if let some assignee := issue.assignee then
      buf := buf.writeString startX y s!"Assignee: @{assignee}" Style.default
      y := y + 1

    -- Dependencies
    if !issue.blockedBy.isEmpty then
      let blockedByLine := s!"Blocked by: {String.intercalate ", " (issue.blockedBy.map (s!"#{·}")).toList}"
      buf := buf.writeString startX y blockedByLine (Style.default.withFg (.ansi .red))
      y := y + 1

    if !issue.blocks.isEmpty then
      let blocksLine := s!"Blocks: {String.intercalate ", " (issue.blocks.map (s!"#{·}")).toList}"
      buf := buf.writeString startX y blocksLine Style.default
      y := y + 1

    y := y + 1

    -- Description
    buf := buf.writeString startX y "Description:" Style.bold
    y := y + 1
    let maxDescLines := if height > y - startY + 2 then height - (y - startY) - 2 else 5
    let descLines := issue.description.splitOn "\n"
    for lineIdx in [:min descLines.length maxDescLines] do
      if h : lineIdx < descLines.length then
        let line := descLines[lineIdx]
        let maxLineLen := if width > 4 then width - 4 else 40
        buf := buf.writeString (startX + 2) y (line.take maxLineLen) Style.default
        y := y + 1

    -- Progress
    if !issue.progress.isEmpty && y < startY + height - 1 then
      y := y + 1
      buf := buf.writeString startX y "Progress:" Style.bold
      y := y + 1
      for entry in issue.progress do
        if y < startY + height - 1 then
          let maxEntryLen := if width > 2 then width - 2 else 40
          let entryLine := s!"  [{entry.timestamp}] {entry.message}"
          buf := buf.writeString startX y (entryLine.take maxEntryLen) (Style.default.withFg (.ansi .brightBlack))
          y := y + 1

  buf

/-- Draw a rounded border box -/
def drawBorder (buf : Buffer) (x y width height : Nat) (focused : Bool) : Buffer := Id.run do
  let mut buf := buf
  if width < 2 || height < 2 then return buf

  -- Border characters (rounded style)
  let topLeft := '╭'
  let topRight := '╮'
  let bottomLeft := '╰'
  let bottomRight := '╯'
  let horizontal := '─'
  let vertical := '│'

  let borderStyle := if focused then
    Style.default.withFg (.ansi .cyan)
  else
    Style.default.withFg (.ansi .brightBlack)

  -- Draw corners
  buf := buf.setStyled x y topLeft borderStyle
  buf := buf.setStyled (x + width - 1) y topRight borderStyle
  buf := buf.setStyled x (y + height - 1) bottomLeft borderStyle
  buf := buf.setStyled (x + width - 1) (y + height - 1) bottomRight borderStyle

  -- Draw horizontal borders
  for xi in [x + 1 : x + width - 1] do
    buf := buf.setStyled xi y horizontal borderStyle
    buf := buf.setStyled xi (y + height - 1) horizontal borderStyle

  -- Draw vertical borders
  for yi in [y + 1 : y + height - 1] do
    buf := buf.setStyled x yi vertical borderStyle
    buf := buf.setStyled (x + width - 1) yi vertical borderStyle

  buf

/-- Draw a text input field with border -/
def drawTextInput (buf : Buffer) (input : TextInput) (startX startY : Nat) (width : Nat)
    (focused : Bool) : Buffer := Id.run do
  let mut buf := buf

  -- Draw border around input (3 rows tall: border, text, border)
  let borderWidth := if width > 4 then width else 4
  buf := drawBorder buf startX startY borderWidth 3 focused

  -- Calculate visible portion of text (inside the border)
  let innerX := startX + 1
  let innerY := startY + 1
  let maxWidth := if borderWidth > 4 then borderWidth - 4 else 1
  let text := input.text.take maxWidth
  let cursor := min input.cursor maxWidth

  -- Draw text inside border
  let textStyle := if focused then
    Style.default.withFg (.ansi .white)
  else
    Style.default.withFg (.ansi .brightBlack)
  buf := buf.writeString innerX innerY text textStyle

  -- Draw cursor if focused
  if focused then
    let cursorX := innerX + cursor
    let cursorChar := if cursor < text.length then
      text.get ⟨cursor⟩
    else
      ' '
    let cursorStyle := Style.default.withBg (.ansi .white) |>.withFg (.ansi .black)
    buf := buf.writeString cursorX innerY cursorChar.toString cursorStyle

  buf

/-- Draw a form field with label -/
def drawFormField (buf : Buffer) (label : String) (input : TextInput) (startX startY : Nat)
    (width : Nat) (focused : Bool) : Buffer := Id.run do
  let mut buf := buf

  -- Draw label
  let labelStyle := if focused then
    Style.bold.withFg (.ansi .cyan)
  else
    Style.default.withFg (.ansi .white)
  buf := buf.writeString startX startY s!"{label}:" labelStyle

  -- Draw bordered input on next line (takes 3 rows for border)
  let inputX := startX
  let inputWidth := if width > 2 then width - 2 else 10
  buf := drawTextInput buf input inputX (startY + 1) inputWidth focused

  buf

/-- Draw priority selector -/
def drawPrioritySelector (buf : Buffer) (priority : Priority) (startX startY : Nat)
    (focused : Bool) : Buffer := Id.run do
  let mut buf := buf

  -- Draw label
  let labelStyle := if focused then
    Style.bold.withFg (.ansi .cyan)
  else
    Style.default.withFg (.ansi .white)
  buf := buf.writeString startX startY "Priority:" labelStyle

  -- Draw priority options
  let priorities : List (Priority × String) := [
    (.low, "Low"), (.medium, "Medium"), (.high, "High"), (.critical, "Critical")
  ]
  let mut x := startX + 2
  let y := startY + 1
  for (p, name) in priorities do
    let style := if p == priority then
      if focused then
        Style.default.withBg (.ansi .blue) |>.withFg (.ansi .white)
      else
        Style.default.withBg (.ansi .brightBlack) |>.withFg (.ansi .white)
    else
      Style.default.withFg (.ansi .brightBlack)
    let label := s!" {name} "
    buf := buf.writeString x y label style
    x := x + label.length + 1

  -- Draw hint if focused
  if focused then
    buf := buf.writeString x y "  ←/→ to change" (Style.default.withFg (.ansi .brightBlack))

  buf

/-- Draw the create/edit form -/
def drawForm (buf : Buffer) (state : AppState) (startX startY : Nat) (width height : Nat) : Buffer := Id.run do
  let mut buf := buf
  let form := state.formState

  -- Draw form title
  let title := if form.editingIssueId.isSome then "Edit Issue" else "New Issue"
  buf := buf.writeString startX startY title Style.bold
  let mut y := startY + 2

  -- Title field (label + 3-row bordered input + gap = 5 rows)
  buf := drawFormField buf "Title" form.title startX y width (form.focusedField == .title)
  y := y + 5

  -- Description field
  buf := drawFormField buf "Description" form.description startX y width (form.focusedField == .description)
  y := y + 5

  -- Priority selector (not bordered, keeps 3-row spacing)
  buf := drawPrioritySelector buf form.priority startX y (form.focusedField == .priority)
  y := y + 3

  -- Labels field
  buf := drawFormField buf "Labels (comma-separated)" form.labels startX y width (form.focusedField == .labels)
  y := y + 5

  -- Assignee field
  buf := drawFormField buf "Assignee" form.assignee startX y width (form.focusedField == .assignee)
  y := y + 5

  -- Project field
  buf := drawFormField buf "Project" form.project startX y width (form.focusedField == .project)
  y := y + 5

  -- Validation message
  if !form.isValid then
    buf := buf.writeString startX y "Title is required" (Style.default.withFg (.ansi .red))

  buf

/-- Draw the footer/help bar -/
def drawFooter (buf : Buffer) (state : AppState) (startX startY : Nat) (width : Nat) : Buffer := Id.run do
  let mut buf := buf

  let help : String := match state.viewMode with
    | ViewMode.tree => "[Tab] Switch View  [↑/↓] Navigate  [Enter] Toggle/View  [c] Show Closed  [n] New  [q] Quit"
    | ViewMode.detail => "[Esc] Back  [e] Edit  [c] Close  [r] Reopen  [q] Quit"
    | ViewMode.create | ViewMode.edit => "[Tab] Next Field  [Shift+Tab] Prev  [Ctrl+S] Save  [Esc] Cancel"

  buf := buf.writeString startX startY help (Style.default.withFg (.ansi .brightBlack))

  -- Show status/error message
  if !state.statusMessage.isEmpty then
    let msgX := if width > state.statusMessage.length + 1 then
      startX + width - state.statusMessage.length - 1
    else startX
    buf := buf.writeString msgX startY state.statusMessage (Style.default.withFg (.ansi .green))
  else if !state.errorMessage.isEmpty then
    let msgX := if width > state.errorMessage.length + 1 then
      startX + width - state.errorMessage.length - 1
    else startX
    buf := buf.writeString msgX startY state.errorMessage (Style.default.withFg (.ansi .red))

  buf

/-- Main draw function -/
def draw (frame : Frame) (state : AppState) : Frame := Id.run do
  let mut f := { frame with buffer := frame.buffer.fill Cell.empty }
  let area := frame.area

  match state.viewMode with
  | .tree =>
    -- Layout: Header (1 line) | Tabs (1 line) | Tree (fill) | Footer (1 line)
    let headerArea := Rect.mk area.x area.y area.width 1
    let tabsArea := Rect.mk area.x (area.y + 1) area.width 1
    let treeHeight := if area.height > 3 then area.height - 3 else 1
    let treeArea := Rect.mk area.x (area.y + 2) area.width treeHeight
    let footerArea := Rect.mk area.x (area.y + area.height - 1) area.width 1

    -- Draw header
    let buf := drawHeader f.buffer state headerArea.x headerArea.y headerArea.width
    f := { f with buffer := buf }

    -- Draw tabs
    f := drawTabs f state tabsArea

    -- Draw tree
    f := drawTreeView f state treeArea

    -- Draw footer
    let buf := drawFooter f.buffer state footerArea.x footerArea.y footerArea.width
    f := { f with buffer := buf }

  | .detail | .create | .edit =>
    -- Layout: Header (1 line) | Content (fill) | Footer (1 line)
    let headerArea := Rect.mk area.x area.y area.width 1
    let contentHeight := if area.height > 2 then area.height - 2 else 1
    let contentArea := Rect.mk (area.x + 1) (area.y + 1) (area.width - 2) contentHeight
    let footerArea := Rect.mk area.x (area.y + area.height - 1) area.width 1

    -- Draw header
    let buf := drawHeader f.buffer state headerArea.x headerArea.y headerArea.width
    f := { f with buffer := buf }

    -- Draw content
    let buf := match state.viewMode with
      | .detail => drawDetail f.buffer state contentArea.x contentArea.y contentArea.width contentArea.height
      | .create | .edit => drawForm f.buffer state contentArea.x contentArea.y contentArea.width contentArea.height
      | _ => f.buffer  -- unreachable
    f := { f with buffer := buf }

    -- Draw footer
    let buf := drawFooter f.buffer state footerArea.x footerArea.y footerArea.width
    f := { f with buffer := buf }

  f

end Tracker.TUI
