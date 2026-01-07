# Tracker Roadmap

Local issue tracker for Claude Code and human developers. This document outlines planned features, improvements, and technical debt items.

## Current State Overview

Tracker is a dual-interface issue tracking tool:
- **CLI Mode**: JSON output by default (designed for programmatic access by Claude Code)
- **TUI Mode**: Interactive terminal UI (using Terminus library)

**Core Features Implemented:**
- Issue CRUD (create, read, update, delete)
- Status workflow (open -> in-progress -> closed)
- Priority levels (low, medium, high, critical)
- Labels, assignees, and project assignment
- Progress logging with timestamps
- Dependency tracking (blocks/blocked_by)
- Two-level navigation in TUI (project list -> issue list)
- Markdown file storage with YAML frontmatter

**Dependencies:**
- terminus (TUI framework)
- parlance (CLI argument parsing)
- chronos (timestamp handling)

---

## Feature Proposals

### [Priority: High] Search and Filter Command

**Description:** Add a `tracker search` command to find issues by keyword across title, description, and progress notes.

**Rationale:** As issue counts grow, users need a way to quickly find relevant issues without scrolling through lists. Essential for Claude Code to locate issues efficiently.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Storage.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: High] Bulk Operations

**Description:** Support bulk status changes (e.g., `tracker close 1 2 3` or `tracker close --project=foo`).

**Rationale:** Common workflow pattern when closing multiple related issues after completing a feature.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: High] Delete Issue from TUI

**Description:** Add delete functionality in the TUI (currently only available via CLI).

**Rationale:** Feature parity between CLI and TUI. Users should be able to delete issues without leaving the TUI.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Update.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/App.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Draw.lean` (help text)

**Estimated Effort:** Small

**Dependencies:** Consider adding confirmation dialog first

---

### [Priority: Medium] Import/Export Commands

**Description:** Add `tracker export` and `tracker import` commands for migrating issues between projects or backing up data.

**Rationale:** Enables interoperability and backup workflows.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Storage.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Due Dates and Reminders

**Description:** Add optional due date field to issues with support for filtering overdue items.

**Rationale:** Time-sensitive issue tracking is a common requirement for project management.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Types.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Parser.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Output.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/State.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Draw.lean`

**Estimated Effort:** Large

**Dependencies:** Chronos library already provides timestamp support

---

### [Priority: Medium] Statistics Command

**Description:** Add `tracker stats` command to show issue counts by status, priority, project, assignee, and label.

**Rationale:** Provides quick overview of project health and workload distribution.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Output.lean`

**Estimated Effort:** Small

**Dependencies:** None

---

### [Priority: Medium] TUI Issue Creation with Templates

**Description:** Pre-populate issue description with configurable templates based on labels or project.

**Rationale:** Improves consistency in issue documentation and speeds up issue creation.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Storage.lean` (load templates)
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/State.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Update.lean`

**Estimated Effort:** Medium

**Dependencies:** Configuration file support

---

### [Priority: Medium] Configuration File Support

**Description:** Add `.tracker.yaml` or `.tracker.json` configuration file for default options (default project, custom statuses, templates).

**Rationale:** Reduces repetitive command-line arguments and enables project customization.

**Affected Files:**
- New file: `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Config.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Storage.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Medium] Multi-Label CLI Support

**Description:** Allow multiple `--label` flags in add/update commands (e.g., `tracker add "Title" -l bug -l urgent`).

**Rationale:** Currently only supports one label per command invocation.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean` (lines 60-63)

**Estimated Effort:** Small

**Dependencies:** May require parlance library update for multi-value flags

---

### [Priority: Medium] TUI Scrolling for Long Lists and Descriptions

**Description:** Add proper scrolling support for issue lists that exceed screen height and for long descriptions in detail view.

**Rationale:** Current implementation truncates at maxRows without scroll indicators.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/State.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Draw.lean` (lines 70-104, 194-201)
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Update.lean`

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Git Integration

**Description:** Add `tracker commit-hook` command to auto-reference issues in commit messages and close issues via commit message patterns (e.g., "fixes #42").

**Rationale:** Streamlines developer workflow by connecting commits to issues.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean`
- New file: `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Git/Hooks.lean`

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Low] Markdown Rendering in TUI

**Description:** Render markdown formatting (bold, code, lists) in issue description and progress notes in TUI.

**Rationale:** Issues are stored as markdown; TUI should respect this formatting.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Draw.lean`
- May require terminus widget updates

**Estimated Effort:** Large

**Dependencies:** Terminus markdown widget or custom renderer

---

### [Priority: Low] Keyboard Shortcut Customization

**Description:** Allow users to customize TUI keyboard shortcuts via config file.

**Rationale:** Different users have different preferences for navigation keys.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Update.lean`
- Requires configuration file support

**Estimated Effort:** Medium

**Dependencies:** Configuration file support

---

### [Priority: Low] Watch Mode

**Description:** Add `tracker watch` command that monitors .issues directory and live-reloads TUI when files change.

**Rationale:** Useful when editing issue files directly or when multiple tools modify issues.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/App.lean`
- May require file system watcher FFI

**Estimated Effort:** Large

**Dependencies:** File system watcher capability

---

## Code Improvements

### [Priority: High] Consolidate Duplicate escapeJson Functions

**Current State:** `escapeJson` function is defined in three places:
- `Types.lean` line 83 (in ProgressEntry.toJson)
- `Types.lean` line 138 (in Issue namespace)
- `IssueSummary.toJson` line 207

**Proposed Change:** Move to a shared utility module and reuse across all types.

**Benefits:** Reduced code duplication, easier maintenance, consistent escaping behavior.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Types.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Util.lean`

**Estimated Effort:** Small

---

### [Priority: High] Consolidate Duplicate padLeft Functions

**Current State:** `padLeft` function is defined in three places:
- `Types.lean` lines 113-118
- `Output.lean` lines 12-16
- `Draw.lean` lines 15-19

**Proposed Change:** Move to Util.lean (already has a version) and import where needed.

**Benefits:** Single source of truth for string padding.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Types.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Util.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Output.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Draw.lean`

**Estimated Effort:** Small

---

### [Priority: High] Use Proper JSON Library

**Current State:** Manual JSON string construction with hand-rolled escaping in `Types.lean`.

**Proposed Change:** Consider using a proper JSON library (e.g., Lean 4 JSON support or create a minimal JSON builder).

**Benefits:** Correct handling of all edge cases, cleaner code, extensibility.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Types.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Improve Error Handling Consistency

**Current State:** Mix of `Option`, `IO.userError`, and `Except` for error handling across different modules.

**Proposed Change:** Standardize on `Except String α` or a custom `TrackerError` type for all fallible operations.

**Benefits:** More predictable error propagation, better error messages for users.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Parser.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Storage.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Optimize Issue Loading for Large Repositories

**Current State:** `loadAllIssues` reads and parses every issue file on every operation.

**Proposed Change:** Add caching with mtime-based invalidation, or lazy loading for operations that only need a subset.

**Benefits:** Better performance with hundreds of issues.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Storage.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Separate CLI and TUI Mode Detection

**Current State:** Mode detection is done via argument parsing in `Main.lean` with manual help/version checks before Parlance parsing.

**Proposed Change:** Let Parlance handle all argument parsing including help/version, remove redundant checks.

**Benefits:** Cleaner entry point, consistent behavior.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Main.lean` (lines 58-99)

**Estimated Effort:** Small

---

### [Priority: Medium] Type-Safe Command Dispatch

**Current State:** Command dispatch uses string matching on commandPath in `Handlers.lean` line 270.

**Proposed Change:** Use a proper sum type for commands and pattern match.

**Benefits:** Compiler-checked exhaustiveness, better refactoring safety.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Extract TUI Widgets

**Current State:** All drawing code is in a single `Draw.lean` file with inline rendering logic.

**Proposed Change:** Extract reusable widgets (border, text input, selector) into separate modules.

**Benefits:** Better organization, potential reuse across other TUI apps.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/Draw.lean`
- New files: `Tracker/TUI/Widgets/*.lean`

**Estimated Effort:** Medium

---

### [Priority: Low] Add Result Type for PendingAction Processing

**Current State:** `processPendingAction` in App.lean directly modifies state with inline error handling.

**Proposed Change:** Return a structured result type that separates success/failure paths.

**Benefits:** Cleaner control flow, easier testing.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/TUI/App.lean`

**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: High] Add Unit Tests

**Issue:** No test coverage for core functionality.

**Location:** Missing `Tests/` directory

**Action Required:**
1. Create `Tests/Main.lean` with Crucible test suite
2. Add tests for Parser (frontmatter parsing, progress parsing)
3. Add tests for Types (JSON serialization, status/priority conversion)
4. Add tests for Storage (mocked file operations)

**Estimated Effort:** Medium

---

### [Priority: Medium] Remove Unused Util Functions

**Issue:** Several functions in `Util.lean` may not be used after duplicate consolidation.

**Location:** `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Util.lean`

**Action Required:** Audit usage and remove dead code after deduplication.

**Estimated Effort:** Small

---

### [Priority: Medium] Add Module-Level Documentation

**Issue:** Most modules lack module-level documentation comments explaining their purpose.

**Location:** All `.lean` files in Tracker/

**Action Required:** Add `/-! Module documentation -/` comments to each module.

**Estimated Effort:** Small

---

### [Priority: Medium] Document Public APIs

**Issue:** Public functions lack docstrings.

**Location:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Storage.lean` (public API)
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Types.lean` (public types)

**Action Required:** Add `/-- docstring -/` to all public definitions.

**Estimated Effort:** Small

---

### [Priority: Low] README Text Mode Flag Inconsistency

**Issue:** README says `-t` or `--text` for text mode, but code uses `-j` or `--json` flag with text as default. The README is outdated.

**Location:** `/Users/Shared/Projects/lean-workspace/apps/tracker/README.md` lines 56, 42-43

**Action Required:** Update README to match current behavior (text is default, use `-j` for JSON).

**Estimated Effort:** Small

---

### [Priority: Low] Lakefile Uses Local Paths

**Issue:** Lakefile uses relative paths instead of GitHub URLs as recommended in workspace conventions.

**Location:** `/Users/Shared/Projects/lean-workspace/apps/tracker/lakefile.lean` lines 11-13

**Action Required:** For release, update to use GitHub URLs with version tags.

**Estimated Effort:** Small

---

### [Priority: Low] Version String Duplication

**Issue:** Version "0.1.0" appears in multiple places without a single source of truth.

**Location:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean` line 12
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Main.lean` lines 70, 93

**Action Required:** Define version constant in one place and reference it elsewhere.

**Estimated Effort:** Small

---

## API Enhancements

### [Priority: Medium] Richer JSON Output

**Description:** Include computed fields in JSON output (e.g., `isEffectivelyBlocked`, `openDependencyCount`).

**Rationale:** Reduces client-side computation for Claude Code.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/Core/Types.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Output.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Machine-Readable Error Codes

**Description:** Add structured error codes to JSON error responses (e.g., `{"error": "NOT_FOUND", "message": "Issue #42 not found"}`).

**Rationale:** Enables programmatic error handling by Claude Code.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Output.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Handlers.lean`

**Estimated Effort:** Small

---

### [Priority: Low] Output Format Options

**Description:** Add `--format=table|compact|full` for text output to control verbosity.

**Rationale:** Different contexts need different levels of detail.

**Affected Files:**
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Commands.lean`
- `/Users/Shared/Projects/lean-workspace/apps/tracker/Tracker/CLI/Output.lean`

**Estimated Effort:** Small

---

## Notes

- Priority levels reflect impact on core use cases (Claude Code integration and human usability)
- Effort estimates: Small (< 1 hour), Medium (1-4 hours), Large (> 4 hours)
- Many improvements are independent and can be tackled in any order
- Test coverage should be addressed early to support safe refactoring
