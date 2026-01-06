/-
  TUI rendering.
-/
import Tracker.Core.Types
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
      let blockedStr := if issue.isBlocked then "[B]" else "   "
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

/-- Draw the footer/help bar -/
def drawFooter (buf : Buffer) (state : AppState) (startX startY : Nat) (width : Nat) : Buffer := Id.run do
  let mut buf := buf

  let help : String := match state.viewMode with
    | ViewMode.list => "[up/down] Navigate  [Tab] Switch Tab  [Enter] View  [q] Quit"
    | ViewMode.detail => "[Esc] Back  [c] Close  [r] Reopen  [q] Quit"
    | ViewMode.create | ViewMode.edit => "[Esc] Cancel  [Enter] Save"

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
    | ViewMode.create | ViewMode.edit => buf  -- TODO: form rendering

  -- Footer (1 line)
  let footerY := areaY + areaHeight - 1
  buf := drawFooter buf state areaX footerY areaWidth

  { frame with buffer := buf }

end Tracker.TUI
