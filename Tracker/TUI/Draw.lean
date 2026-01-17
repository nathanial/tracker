/-
  TUI rendering helpers.
  Note: Most rendering now uses Terminus.Reactive widgets in App.lean.
  This file contains helper functions for formatting.
-/
import Tracker.Core.Types

namespace Tracker.TUI

open Tracker

/-- Format priority as a short string with suggested color -/
def priorityText (p : Priority) : String :=
  match p with
  | .low => "LOW"
  | .medium => "MED"
  | .high => "HIGH"
  | .critical => "CRIT"

/-- Format status as an icon -/
def statusIcon (s : Status) : String :=
  match s with
  | .open_ => " "
  | .inProgress => ">"
  | .closed => "x"

/-- Format status as text -/
def statusText (s : Status) : String :=
  match s with
  | .open_ => "Open"
  | .inProgress => "In Progress"
  | .closed => "Closed"

/-- Get help text for tree view -/
def treeViewHelp : String :=
  "[Tab] Switch View  [↑/↓] Navigate  [Enter] Toggle/View  [h] Show Closed  [n] New  [q] Quit"

/-- Get help text for detail view -/
def detailViewHelp : String :=
  "[Esc] Back  [e] Edit  [c] Close  [r] Reopen  [q] Quit"

/-- Get help text for form view -/
def formViewHelp : String :=
  "[Tab] Next Field  [Shift+Tab] Prev  [Ctrl+S] Save  [Esc] Cancel"

end Tracker.TUI
