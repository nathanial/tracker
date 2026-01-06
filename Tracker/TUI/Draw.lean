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

/-- Draw the header with tabs -/
def drawHeader (buf : Buffer) (state : AppState) (startX startY : Nat) (width : Nat) : Buffer := Id.run do
  let mut buf := buf

  -- Draw title
  let title := " Tracker "
  buf := buf.writeString startX startY title (Style.bold.withFg (.ansi .cyan))

  -- Draw tabs
  let tabs : List FilterTab := [.open_, .inProgress, .closed, .all]
  let mut x := startX + title.length + 2
  for tab in tabs do
    let label := s!" {tab.toString} "
    let style := if state.filterTab == tab then
      Style.default.withBg (.ansi .blue) |>.withFg (.ansi .white)
    else
      Style.default.withFg (.ansi .white)
    buf := buf.writeString x startY label style
    x := x + label.length + 1

  buf

/-- Draw the issue list -/
def drawList (buf : Buffer) (state : AppState) (startX startY : Nat) (width height : Nat) : Buffer := Id.run do
  let mut buf := buf
  let issues := state.filteredIssues

  if issues.isEmpty then
    buf := buf.writeString (startX + 2) (startY + 1) "No issues found." Style.default
    return buf

  let mut y := startY
  let maxRows := height - 1
  for idx in [:min issues.size maxRows] do
    if h : idx < issues.size then
      let issue := issues[idx]
      let isSelected := idx == state.selectedIndex

      -- Build row content
      let blockedStr := if Storage.isEffectivelyBlocked issue state.issues then "[B]" else "   "
      let idStr := padLeft (toString issue.id) 4 ' '
      let priorityStr : String := match issue.priority with
        | Priority.critical => "CRIT"
        | Priority.high => "HIGH"
        | Priority.medium => "MED "
        | Priority.low => "LOW "
      let statusStr : String := match issue.status with
        | Status.open_ => " "
        | Status.inProgress => ">"
        | Status.closed => "x"
      let maxTitleLen := if width > 25 then width - 25 else 20
      let title := issue.title.take maxTitleLen
      let row := s!"{blockedStr} #{idStr} [{priorityStr}] [{statusStr}] {title}"

      -- Style based on selection and status
      let style := if isSelected then
        Style.default.withBg (.ansi .blue) |>.withFg (.ansi .white)
      else if issue.status == Status.closed then
        Style.default.withFg (.ansi .brightBlack)
      else if issue.priority == Priority.critical || issue.priority == Priority.high then
        Style.default.withFg (.ansi .red)
      else
        Style.default

      buf := buf.writeString startX y row style
      y := y + 1

  buf

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
    | ViewMode.list => "[↑/↓] Navigate  [Tab] Switch Tab  [Enter] View  [n] New  [q] Quit"
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
  let mut buf := frame.buffer.fill Cell.empty
  let areaWidth := frame.area.width
  let areaHeight := frame.area.height
  let areaX := frame.area.x
  let areaY := frame.area.y

  -- Header (2 lines)
  buf := drawHeader buf state areaX areaY areaWidth

  -- Main content area
  let contentX := areaX + 1
  let contentY := areaY + 2
  let contentWidth := if areaWidth > 2 then areaWidth - 2 else 1
  let contentHeight := if areaHeight > 4 then areaHeight - 4 else 1

  buf := match state.viewMode with
    | ViewMode.list => drawList buf state contentX contentY contentWidth contentHeight
    | ViewMode.detail => drawDetail buf state contentX contentY contentWidth contentHeight
    | ViewMode.create | ViewMode.edit => drawForm buf state contentX contentY contentWidth contentHeight

  -- Footer (1 line)
  let footerY := areaY + areaHeight - 1
  buf := drawFooter buf state areaX footerY areaWidth

  { frame with buffer := buf }

end Tracker.TUI
