/-
  Tracker Test Suite
-/
import Crucible
import Tracker.Core.Parser
import Tracker.Core.Storage
import Tracker.Core.Types
import TrackerTests.TUI

open Crucible
open Tracker.Parser
open Tracker
open Tracker.Storage

testSuite "Tracker.Parser"

/-! ## Frontmatter Parsing -/

test "parse minimal frontmatter" := do
  let content := "---\nid: 1\ntitle: Test Issue\nstatus: open\npriority: medium\ncreated: 2026-01-01\nupdated: 2026-01-01\nlabels: []\nassignee: \nproject: \nblocks: []\nblocked_by: []\n---\n\n# Test Issue\n\n## Description\nTest description\n"
  match parseIssueFile content with
  | .ok parsed =>
    parsed.frontmatter.id ≡ some 1
    parsed.frontmatter.title ≡ some "Test Issue"
    parsed.frontmatter.status ≡ some "open"
    parsed.frontmatter.priority ≡ some "medium"
  | .error e => throwThe IO.Error s!"Parse failed: {e}"

test "parse frontmatter with labels" := do
  let content := "---\nid: 2\ntitle: Bug Fix\nstatus: in-progress\npriority: high\ncreated: 2026-01-01\nupdated: 2026-01-01\nlabels: [bug, urgent]\nassignee: claude\nproject: tracker\nblocks: []\nblocked_by: []\n---\n\n# Bug Fix\n\n## Description\n"
  match parseIssueFile content with
  | .ok parsed =>
    parsed.frontmatter.labels ≡ #["bug", "urgent"]
    parsed.frontmatter.assignee ≡ some "claude"
    parsed.frontmatter.project ≡ some "tracker"
  | .error e => throwThe IO.Error s!"Parse failed: {e}"

test "parse frontmatter with blocks and blocked_by" := do
  let content := "---\nid: 4\ntitle: Blocked Issue\nstatus: open\npriority: medium\ncreated: 2026-01-01\nupdated: 2026-01-01\nlabels: []\nassignee: \nproject: \nblocks: [5, 6]\nblocked_by: [1, 2, 3]\n---\n\n# Blocked Issue\n\n## Description\n"
  match parseIssueFile content with
  | .ok parsed =>
    parsed.frontmatter.blocks ≡ #[5, 6]
    parsed.frontmatter.blockedBy ≡ #[1, 2, 3]
  | .error e => throwThe IO.Error s!"Parse failed: {e}"

test "parse frontmatter with null values" := do
  let content := "---\nid: 5\ntitle: No Assignee\nstatus: open\npriority: medium\ncreated: 2026-01-01\nupdated: 2026-01-01\nlabels: []\nassignee: null\nproject: null\nblocks: []\nblocked_by: []\n---\n\n# No Assignee\n\n## Description\n"
  match parseIssueFile content with
  | .ok parsed =>
    parsed.frontmatter.assignee ≡ none
    parsed.frontmatter.project ≡ none
  | .error e => throwThe IO.Error s!"Parse failed: {e}"

/-! ## Progress Parsing -/

test "parse progress entries" := do
  let content := "---\nid: 7\ntitle: With Progress\nstatus: open\npriority: medium\ncreated: 2026-01-01\nupdated: 2026-01-01\nlabels: []\nassignee: \nproject: \nblocks: []\nblocked_by: []\n---\n\n# With Progress\n\n## Description\nTest\n\n## Progress\n- [2026-01-01T10:00:00] Started work\n- [2026-01-01T11:00:00] Made progress\n"
  match parseIssueFile content with
  | .ok parsed =>
    parsed.progress.size ≡ 2
    parsed.progress[0]!.timestamp ≡ "2026-01-01T10:00:00"
    parsed.progress[0]!.message ≡ "Started work"
  | .error e => throwThe IO.Error s!"Parse failed: {e}"

/-! ## toIssue Conversion -/

test "toIssue applies defaults" := do
  let content := "---\nid: 9\ntitle: Defaults Test\nstatus: open\npriority: high\ncreated: 2026-01-01\nupdated: 2026-01-01\nlabels: [test]\nassignee: \nproject: \nblocks: []\nblocked_by: []\n---\n\n# Defaults Test\n\n## Description\nTest\n"
  match parseIssueFile content with
  | .ok parsed =>
    let issue := toIssue parsed 99 "2026-01-01T00:00:00"
    issue.id ≡ 9
    issue.title ≡ "Defaults Test"
    issue.status ≡ Status.open_
    issue.priority ≡ Priority.high
  | .error e => throwThe IO.Error s!"Parse failed: {e}"

/-! ## Round-trip -/

test "round-trip simple issue" := do
  let content := "---\nid: 10\ntitle: Round Trip Test\nstatus: open\npriority: medium\ncreated: 2026-01-01T00:00:00\nupdated: 2026-01-01T00:00:00\nlabels: []\nassignee: \nproject: \nblocks: []\nblocked_by: []\n---\n\n# Round Trip Test\n\n## Description\nOriginal description\n"
  match parseIssueFile content with
  | .ok parsed1 =>
    let issue := toIssue parsed1 0 "2026-01-01T00:00:00"
    let markdown := issueToMarkdown issue
    match parseIssueFile markdown with
    | .ok parsed2 =>
      let issue2 := toIssue parsed2 0 "2026-01-01T00:00:00"
      issue2.id ≡ issue.id
      issue2.title ≡ issue.title
    | .error e => throwThe IO.Error s!"Second parse failed: {e}"
  | .error e => throwThe IO.Error s!"First parse failed: {e}"

/-! ## Search -/

test "search matches title description and progress" := do
  let issue1 : Issue := {
    id := 1
    title := "Fix parser crash"
    status := Status.open_
    priority := Priority.high
    created := "2026-01-01T00:00:00"
    updated := "2026-01-01T00:00:00"
    labels := #[]
    assignee := none
    project := none
    blocks := #[]
    blockedBy := #[]
    description := "Tokenizer fails on emoji input"
    progress := #[]
  }
  let issue2 : Issue := {
    id := 2
    title := "UI polish"
    status := Status.open_
    priority := Priority.medium
    created := "2026-01-01T00:00:00"
    updated := "2026-01-01T00:00:00"
    labels := #[]
    assignee := none
    project := none
    blocks := #[]
    blockedBy := #[]
    description := ""
    progress := #[{ timestamp := "2026-01-02T00:00:00", message := "Investigate parser regression" }]
  }
  let issue3 : Issue := {
    id := 3
    title := "Docs cleanup"
    status := Status.open_
    priority := Priority.low
    created := "2026-01-01T00:00:00"
    updated := "2026-01-01T00:00:00"
    labels := #[]
    assignee := none
    project := none
    blocks := #[]
    blockedBy := #[]
    description := "Improve onboarding"
    progress := #[]
  }
  let results := searchIssuesIn #[issue1, issue2, issue3] "parser"
  results.size ≡ 2
  results.any (·.id == 1) ≡ true
  results.any (·.id == 2) ≡ true
  results.any (·.id == 3) ≡ false

test "search is case-insensitive" := do
  let issue : Issue := {
    id := 4
    title := "Parser regression"
    status := Status.open_
    priority := Priority.medium
    created := "2026-01-01T00:00:00"
    updated := "2026-01-01T00:00:00"
    labels := #[]
    assignee := none
    project := none
    blocks := #[]
    blockedBy := #[]
    description := ""
    progress := #[]
  }
  let results := searchIssuesIn #[issue] "PARSER"
  results.size ≡ 1

/-! ## Error Cases -/

test "error on missing frontmatter delimiter" := do
  let content := "id: 1\ntitle: Bad\n"
  match parseIssueFile content with
  | .ok _ => throwThe IO.Error "Should have failed"
  | .error _ => pure ()

test "error on unclosed frontmatter" := do
  let content := "---\nid: 1\ntitle: Unclosed\n"
  match parseIssueFile content with
  | .ok _ => throwThe IO.Error "Should have failed"
  | .error _ => pure ()

def main : IO UInt32 := runAllSuites
