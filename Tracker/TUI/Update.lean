/-
  TUI input handling and state updates.
  Note: Tree navigation is now handled by the reactive forest' widget.
  This file contains PendingAction types.
-/
import Tracker.Core.Types

namespace Tracker.TUI

open Tracker

/-- Actions that require IO (will be processed in the main loop) -/
inductive PendingAction where
  | none
  | closeIssue (id : Nat)
  | reopenIssue (id : Nat)
  | refreshIssues
  | createIssue (title : String) (description : String) (priority : Priority) (labels : Array String) (assignee : Option String) (project : Option String)
  | updateIssue (id : Nat) (title : String) (description : String) (priority : Priority) (labels : Array String) (assignee : Option String) (project : Option String)
  deriving BEq, Inhabited

end Tracker.TUI
