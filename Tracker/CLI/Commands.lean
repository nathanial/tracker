/-
  CLI command definitions using Parlance.
-/
import Parlance

namespace Tracker.CLI

open Parlance

/-- The main tracker command with all subcommands -/
def trackerCommand : Command := command "tracker" do
  Cmd.version "0.1.0"
  Cmd.description "Local issue tracker for Claude Code"

  -- Global flags
  Cmd.boolFlag "json" (short := some 'j') (description := "Output in JSON format (default is text)")

  -- init subcommand
  Cmd.subcommand "init" do
    Cmd.description "Initialize issue tracker in current directory"

  -- add subcommand
  Cmd.subcommand "add" do
    Cmd.description "Create a new issue"
    Cmd.arg "title" (description := "Issue title")
    Cmd.flag "priority" (short := some 'p')
      (argType := .choice ["low", "medium", "high", "critical"])
      (description := "Issue priority")
      (defaultValue := some "medium")
    Cmd.flag "label" (short := some 'l')
      (description := "Add a label (can be used multiple times)")
    Cmd.flag "assignee" (short := some 'a')
      (description := "Assign to someone")
    Cmd.flag "description" (short := some 'd')
      (description := "Issue description")
    Cmd.flag "project" (short := some 'P')
      (description := "Project this issue belongs to")

  -- list subcommand
  Cmd.subcommand "list" do
    Cmd.description "List issues"
    Cmd.boolFlag "all" (description := "Include closed issues")
    Cmd.flag "status" (short := some 's')
      (argType := .choice ["open", "in-progress", "closed"])
      (description := "Filter by status")
    Cmd.flag "label" (short := some 'l')
      (description := "Filter by label")
    Cmd.flag "assignee" (short := some 'a')
      (description := "Filter by assignee")
    Cmd.flag "project" (short := some 'p')
      (description := "Filter by project")
    Cmd.boolFlag "blocked" (description := "Show only blocked issues")

  -- search subcommand
  Cmd.subcommand "search" do
    Cmd.description "Search issues by keyword"
    Cmd.arg "query" (description := "Keyword to search in title, description, and progress")

  -- show subcommand
  Cmd.subcommand "show" do
    Cmd.description "Show issue details"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID")

  -- update subcommand
  Cmd.subcommand "update" do
    Cmd.description "Update an issue"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID")
    Cmd.flag "status" (short := some 's')
      (argType := .choice ["open", "in-progress", "closed"])
      (description := "New status")
    Cmd.flag "priority" (short := some 'p')
      (argType := .choice ["low", "medium", "high", "critical"])
      (description := "New priority")
    Cmd.flag "title" (description := "New title")
    Cmd.flag "assignee" (short := some 'a')
      (description := "New assignee")
    Cmd.flag "add-label" (description := "Add a label")
    Cmd.flag "remove-label" (description := "Remove a label")

  -- progress subcommand
  Cmd.subcommand "progress" do
    Cmd.description "Add a progress note to an issue"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID")
    Cmd.arg "message" (description := "Progress message")

  -- close subcommand
  Cmd.subcommand "close" do
    Cmd.description "Close an issue"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID")
    Cmd.arg "comment" (required := false) (description := "Closing comment")

  -- reopen subcommand
  Cmd.subcommand "reopen" do
    Cmd.description "Reopen a closed issue"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID")

  -- block subcommand
  Cmd.subcommand "block" do
    Cmd.description "Mark an issue as blocked by another"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID to block")
    Cmd.flag "by" (argType := .nat) (required := true)
      (description := "ID of the blocking issue")

  -- unblock subcommand
  Cmd.subcommand "unblock" do
    Cmd.description "Remove a blocking relationship"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID to unblock")
    Cmd.flag "by" (argType := .nat) (required := true)
      (description := "ID of the blocking issue to remove")

  -- deps subcommand
  Cmd.subcommand "deps" do
    Cmd.description "Show dependency graph for an issue"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID")

  -- delete subcommand
  Cmd.subcommand "delete" do
    Cmd.description "Delete an issue"
    Cmd.arg "id" (argType := .nat) (description := "Issue ID")
    Cmd.boolFlag "force" (short := some 'f')
      (description := "Skip confirmation")

  -- tui subcommand (explicit TUI launch)
  Cmd.subcommand "tui" do
    Cmd.description "Launch the interactive TUI"
    Cmd.boolFlag "debug" (description := "Write changed frames to .debug/ directory")

end Tracker.CLI
