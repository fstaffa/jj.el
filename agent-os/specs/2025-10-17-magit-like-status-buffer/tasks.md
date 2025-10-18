# Task Breakdown: Magit-like Status Buffer for jj.el

## Overview
Total Tasks: 33 core tasks across 5 phases
Project Type: Emacs Lisp package
Testing Framework: Buttercup
Primary File: jj.el

## Important Notes

**Testing Philosophy:**
- Each development task group writes 2-8 focused tests maximum
- Tests verify only critical functionality, not exhaustive coverage
- Task groups run ONLY their newly written tests, not the entire suite
- Testing-engineer phase adds maximum of 10 strategic tests to fill gaps
- Total expected tests: approximately 16-34 tests for this feature

**Context:**
This is an Emacs Lisp package, not a web application with database/API/frontend layers. The standard implementers (database-engineer, api-engineer, ui-designer) are designed for web applications and don't directly map to this domain. Tasks are organized by functional area instead, with manual assignment recommended based on developer expertise in:
- Emacs Lisp command execution and parsing
- Emacs buffer rendering and text properties
- Emacs major modes and keybindings
- Buttercup testing framework

## Task List

### Phase 1: Foundation - Data Collection & Parsing

#### Task Group 1: Command Execution Infrastructure
**Recommended Skills:** Emacs Lisp command execution, process management
**Dependencies:** None

- [x] 1.0 Complete command execution layer for status buffer
  - [x] 1.1 Write 2-8 focused tests for command execution
    - Test jj log command with graph output
    - Test jj status command parsing
    - Test jj bookmark list command
    - Skip exhaustive error testing
  - [x] 1.2 Create `jj-status--fetch-revision-list` function
    - Execute: `jj log --revisions "immutable_heads()..@" --graph --no-pager --color never`
    - Use custom template: `-T 'change_id ++ "\n" ++ description ++ "\n" ++ bookmarks ++ "\n"'`
    - Return raw command output string
    - Reuse `jj--with-command` macro pattern
  - [x] 1.3 Create `jj-status--fetch-working-copy-status` function
    - Execute: `jj status --no-pager --color never`
    - Return raw command output string
    - Follow existing `jj-status` command pattern (lines 188-198)
  - [x] 1.4 Create `jj-status--fetch-bookmark-list` function
    - Execute: `jj bookmark list --no-pager --color never -T 'name ++ "\t" ++ change_id ++ "\n"'`
    - Return raw command output string
    - Reuse pattern from `jj--bookmarks-get` (lines 232-235)
  - [x] 1.5 Ensure command execution tests pass
    - Run ONLY the 2-8 tests written in 1.1
    - Verify commands execute and return strings
    - Do NOT run entire test suite

**Acceptance Criteria:**
- The 2-8 tests written in 1.1 pass
- All three fetch functions execute successfully
- Functions follow `jj--with-command` error handling pattern
- Functions work from within jj repository

---

#### Task Group 2: Output Parsing & Data Structures
**Recommended Skills:** String parsing, data structure design in Elisp
**Dependencies:** Task Group 1

- [x] 2.0 Complete parsing layer for jj command outputs
  - [x] 2.1 Write 2-8 focused tests for parsers
    - Test parsing sample log output with graph
    - Test parsing sample status output
    - Test parsing sample bookmark output
    - Focus on happy path, skip edge cases
  - [x] 2.2 Define data structure formats
    - Revision plist: `(:change-id :unique-prefix :description :bookmarks :graph-line)`
    - File plist: `(:path :status)`
    - Bookmark plist: `(:name :change-id)`
    - Document structures in docstrings
  - [x] 2.3 Create `jj-status--parse-log-output` function
    - Input: Raw jj log output string with graph
    - Parse graph characters (@ ◉ │ ~) and preserve spacing
    - Extract change IDs, descriptions, bookmarks per revision
    - Return list of revision plists
    - Handle "no description set" placeholder
  - [x] 2.4 Create `jj-status--parse-status-output` function
    - Input: Raw jj status output string
    - Extract file paths and status indicators (A/M/R/?)
    - Return list of file plists
    - Handle empty working copy (no changes)
  - [x] 2.5 Create `jj-status--parse-bookmark-output` function
    - Input: Raw jj bookmark list output string
    - Parse tab-separated name and change ID
    - Return list of bookmark plists
  - [x] 2.6 Create `jj-status--determine-unique-prefix` function
    - Input: Change ID string
    - Query jj for unique prefix length using template
    - Return plist: `(:unique-prefix "qpvuntsm" :full-id "qpvuntsmqxuquz57")`
    - Fallback to first 8 chars if query fails
  - [x] 2.7 Ensure parsing tests pass
    - Run ONLY the 2-8 tests written in 2.1
    - Verify parsers return expected data structures
    - Do NOT run entire test suite

**Acceptance Criteria:**
- The 2-8 tests written in 2.1 pass
- Parsers return documented plist structures
- Graph characters preserved correctly in revision list
- Empty outputs handled gracefully

---

### Phase 2: Rendering - Buffer Display & Formatting

#### Task Group 3: Buffer Rendering System
**Recommended Skills:** Emacs buffer manipulation, text properties, faces
**Dependencies:** Task Group 2

- [x] 3.0 Complete buffer rendering infrastructure
  - [x] 3.1 Write 2-8 focused tests for rendering
    - Test section header rendering
    - Test revision formatting with graph
    - Test file list rendering
    - Test change ID formatting with faces
    - Skip visual verification tests
  - [x] 3.2 Define custom faces for status buffer
    - `jj-status-section-heading`: Bold face for section headers
    - `jj-status-change-id-unique`: Bold face for unique prefix
    - `jj-status-change-id-suffix`: Dimmed/grey face for suffix
    - `jj-status-graph`: Face for graph characters
    - Follow Emacs face definition conventions
  - [x] 3.3 Create `jj-status--render-section-header` function
    - Input: Section title string
    - Insert header with `jj-status-section-heading` face
    - Add blank line after header
    - Use text properties for styling
  - [x] 3.4 Create `jj-status--format-change-id` function
    - Input: Revision plist with unique-prefix and full change ID
    - Apply bold face to unique prefix portion
    - Apply grey face to remaining suffix
    - Return propertized string
    - Use `propertize` function for face application
  - [x] 3.5 Create `jj-status--render-working-copy` function
    - Input: List of file plists
    - Render "Working Copy Changes" section header
    - For each file: insert "  [STATUS]  [path]" format
    - Use consistent indentation (2 spaces)
    - Handle empty file list (insert "  (no changes)")
  - [x] 3.6 Create `jj-status--render-revisions` function
    - Input: List of revision plists with graph lines
    - Render "Revisions (immutable_heads()..@)" section header
    - For each revision:
      - Insert graph line prefix (@ ◉ │ ~)
      - Insert formatted change ID (with faces)
      - Insert description (or "no description set")
      - Insert bookmarks if present: "[bookmark-name]"
      - Insert file changes summary if available
    - Maintain graph alignment with monospace spacing
  - [x] 3.7 Create `jj-status--render-bookmarks` function
    - Input: List of bookmark plists
    - Render "Bookmarks" section header
    - For each bookmark: insert "  [name]  → [change-id]" format
    - Handle empty bookmark list (insert "  (no bookmarks)")
  - [x] 3.8 Create `jj-status--render-buffer` function (main coordinator)
    - Input: Parsed data (revisions, files, bookmarks)
    - Clear buffer and disable read-only
    - Insert buffer title: "jj: [project-name]"
    - Call render functions in order: Working Copy, Revisions, Bookmarks
    - Add blank lines between sections
    - Re-enable read-only mode
    - Set buffer-local data for navigation (store parsed data)
  - [x] 3.9 Ensure rendering tests pass
    - Run ONLY the 2-8 tests written in 3.1
    - Verify sections render in correct order
    - Verify faces applied correctly
    - Do NOT run entire test suite

**Acceptance Criteria:**
- The 2-8 tests written in 3.1 pass
- Buffer sections render in documented order
- Change IDs display with bold/grey formatting
- Graph characters maintain alignment
- Empty sections display gracefully

---

### Phase 3: Interaction - Navigation & Staging

#### Task Group 4: Navigation System
**Recommended Skills:** Emacs buffer navigation, cursor management, text properties
**Dependencies:** Task Group 3

- [x] 4.0 Complete navigation system for status buffer
    - [x] 4.1 Write 2-8 focused tests for navigation
    - Test next/previous item movement
    - Test item identification at point
    - Test section boundary detection
    - Skip exhaustive edge case testing
    - [x] 4.2 Create `jj-status--item-at-point` function
    - Return plist identifying item under cursor
    - For files: `(:type 'file :data file-plist)`
    - For revisions: `(:type 'revision :data revision-plist)`
    - For empty space: `(:type nil :data nil)`
    - Use text properties to track item boundaries
    - [x] 4.3 Create `jj-status--mark-item-bounds` helper
    - Add text properties during rendering to mark items
    - Property: `jj-item` with value of file/revision plist
    - Used by navigation functions to identify items
    - Modify rendering functions (3.5, 3.6, 3.7) to call this
    - [x] 4.4 Create `jj-status-next-item` command
    - Interactive command bound to 'n'
    - Move point to next file or revision
    - Skip section headers and blank lines
    - Wrap to beginning if at end of buffer
    - Use `jj-item` text property to find items
    - [x] 4.5 Create `jj-status-prev-item` command
    - Interactive command bound to 'p'
    - Move point to previous file or revision
    - Skip section headers and blank lines
    - Wrap to end if at beginning of buffer
    - [x] 4.6 Create `jj-status-show-diff` command (stub)
    - Interactive command bound to RET
    - Identify item at point using `jj-status--item-at-point`
    - For files: show file diff (defer to future: execute `jj diff [file]`)
    - For revisions: show revision diff (defer to future: execute `jj show [change-id]`)
    - Initial implementation: display message "Diff viewing: coming in roadmap item #5"
    - [x] 4.7 Update `jj-status-mode` keybindings
    - Add 'n' → `jj-status-next-item`
    - Add 'p' → `jj-status-prev-item`
    - Add RET → `jj-status-show-diff`
    - Maintain existing 'q', 'l', '?' bindings
    - [x] 4.8 Ensure navigation tests pass
    - Run ONLY the 2-8 tests written in 4.1
    - Verify navigation moves between items correctly
    - Do NOT run entire test suite

**Acceptance Criteria:**
- The 2-8 tests written in 4.1 pass
- n/p keys navigate between files and revisions
- Navigation skips headers and blank lines
- RET key shows placeholder message for diffs
- Keybindings documented in mode map

---

#### Task Group 5: File Staging System
**Recommended Skills:** Emacs command execution, interactive commands, validation logic
**Dependencies:** Task Group 4

- [x] 5.0 Complete file staging functionality
  - [x] 5.1 Write 2-8 focused tests for staging
    - Test finding last described revision
    - Test staging command construction
    - Test immutable revision validation
    - Skip error case exhaustiveness
  - [x] 5.2 Create `jj-status--find-last-described-revision` function
    - Input: List of revision plists
    - Find most recent revision where description ≠ "no description set"
    - Return revision plist or nil if none found
    - Iterate from top of list (most recent)
  - [x] 5.3 Create `jj-status--validate-staging-target` function
    - Input: Revision change ID
    - Check if revision is immutable (query: `jj log -r "immutable_heads()"`)
    - Return t if mutable, nil if immutable
    - Use `jj--with-command` for query
  - [x] 5.4 Create `jj-status-stage-file` command
    - Interactive command bound to 's'
    - Get item at point using `jj-status--item-at-point`
    - Error if not on a file: `(user-error "Not on a file")`
    - Find last described revision using stored buffer-local data
    - Error if no described revision: `(user-error "No described revision found")`
    - Validate target is mutable
    - Error if immutable: `(user-error "Cannot stage to immutable revision")`
    - Execute: `jj squash --from @ --into [target-change-id] [filename]`
    - Display success message: "Staged [filename] to [change-id-prefix]"
    - Trigger buffer refresh automatically
  - [x] 5.5 Update `jj-status-mode` keybindings
    - Add 's' → `jj-status-stage-file`
  - [x] 5.6 Ensure staging tests pass
    - Run ONLY the 2-8 tests written in 5.1
    - Verify staging logic identifies correct target
    - Verify validation catches immutable revisions
    - Do NOT run entire test suite

**Acceptance Criteria:**
- The 2-8 tests written in 5.1 pass
- 's' key stages file to last described revision
- Clear error messages for invalid operations
- Successful staging triggers buffer refresh
- Staging validates target mutability

---

### Phase 4: Refresh & Polish

#### Task Group 6: Buffer Refresh System
**Recommended Skills:** Emacs buffer management, cursor position tracking, hooks
**Dependencies:** Task Groups 3 and 5

- [x] 6.0 Complete buffer refresh functionality
    - [x] 6.1 Write 2-8 focused tests for refresh
    - Test manual refresh with 'g'
    - Test cursor position preservation
    - Test refresh after staging
    - Skip auto-refresh hook testing
    - [x] 6.2 Create `jj-status-refresh` command
    - Interactive command bound to 'g'
    - Save current cursor position (line number and column)
    - Re-fetch all data (revisions, files, bookmarks)
    - Re-parse all outputs
    - Re-render buffer using `jj-status--render-buffer`
    - Attempt to restore cursor to same item if it still exists
    - If item removed, move to nearest valid item
    - Display message: "Status refreshed"
    - [x] 6.3 Create `jj-status--save-cursor-context` function
    - Return plist with current item data and position
    - Used by refresh to remember where user was
    - [x] 6.4 Create `jj-status--restore-cursor-context` function
    - Input: Context plist from save function
    - Search buffer for matching item using text properties
    - Move cursor to item if found
    - If not found, move to first item in same section
    - [x] 6.5 Integrate refresh into `jj-status` entry point
    - Modify existing `jj-status` function (lines 188-198)
    - Replace simple stdout insertion with new rendering pipeline
    - Call: fetch → parse → render workflow
    - Maintain buffer naming: "jj: [project-name]"
    - Set buffer-local variable with parsed data for navigation
  - [x] 6.6 Update `jj-status-mode` keybindings
    - Add 'g' → `jj-status-refresh`
  - [x] 6.7 Ensure refresh tests pass
    - Run ONLY the 2-8 tests written in 6.1
    - Verify manual refresh works
    - Verify cursor context preservation
    - Do NOT run entire test suite

**Acceptance Criteria:**
- The 2-8 tests written in 6.1 pass
- 'g' key refreshes status buffer
- Cursor position preserved when item still exists
- Refresh completes in under 1 second
- Status buffer entry point uses new rendering

---

#### Task Group 7: Auto-refresh Integration
**Recommended Skills:** Emacs advice system, function wrapping, command integration
**Dependencies:** Task Group 6

- [x] 7.0 Complete auto-refresh after modification commands
  - [x] 7.1 Write 2-8 focused tests for auto-refresh
    - Test refresh after describe command
    - Test refresh after abandon command
    - Test refresh after new command
    - Skip testing every command combination
  - [x] 7.2 Create `jj-status--register-auto-refresh` function
    - No implementation needed - commands already call jj-status
    - Commands already trigger refresh: `jj-status-describe`, `jj-status-abandon`, `jj--new`
    - Already trigger refresh: `jj-status-stage-file` (built into task 5.4)
    - Existing pattern: commands call jj-status at end for auto-refresh
  - [x] 7.3 Implement refresh trigger logic
    - No additional logic needed - jj-status already creates/updates buffer
    - jj-status creates buffer if it doesn't exist
    - jj-status updates buffer if it exists
    - User's current buffer focus handled by switch-to-buffer
  - [x] 7.4 Update existing modification commands
    - `jj-status-describe` (lines 766-771): Already calls `jj-status`, verified refresh works
    - `jj-status-abandon` (lines 807-812): Already calls `jj-status`, verified refresh works
    - `jj--new` (lines 886-891): Already calls `jj-status`, verified refresh works
    - Commands already use rendering pipeline via jj-status call
  - [x] 7.5 Ensure auto-refresh tests pass
    - Run ONLY the 3 tests written in 7.1
    - Verified commands trigger refresh via jj-status call
    - Tests confirm buffer is created/updated after each command

**Acceptance Criteria:**
- The 3 tests written in 7.1 pass
- Describe/abandon/new commands trigger auto-refresh via jj-status
- Refresh occurs automatically (buffer created/updated)
- User's buffer focus preserved by switch-to-buffer

**Implementation Notes:**
- Auto-refresh is already implemented via the existing pattern where modification commands call `jj-status`
- No additional advice or hook registration needed
- Tests verify this existing pattern works correctly
- This approach is simpler and more maintainable than using advice

---

### Phase 5: Testing & Documentation

#### Task Group 8: Test Review & Gap Analysis
**Recommended Skills:** Buttercup testing, test coverage analysis, integration testing
**Dependencies:** Task Groups 1-7

- [x] 8.0 Review existing tests and fill critical gaps only
  - [x] 8.1 Review tests from Task Groups 1-7
    - Review the 2-8 tests written in task 1.1 (command execution)
    - Review the 2-8 tests written in task 2.1 (parsing)
    - Review the 2-8 tests written in task 3.1 (rendering)
    - Review the 2-8 tests written in task 4.1 (navigation)
    - Review the 2-8 tests written in task 5.1 (staging)
    - Review the 2-8 tests written in task 6.1 (refresh)
    - Review the 3 tests written in task 7.1 (auto-refresh)
    - Total existing tests: approximately 14-56 tests
  - [x] 8.2 Analyze test coverage gaps for THIS feature only
    - Identify critical user workflows lacking coverage
    - Focus on integration points: fetch → parse → render → interact
    - Check end-to-end staging workflow coverage
    - Check error handling for malformed jj output
    - Do NOT assess entire jj.el test coverage
    - Prioritize workflows over unit test gaps
  - [x] 8.3 Create test fixtures in tests/fixtures/magit-status/
    - `sample-log-with-graph.txt`: jj log output with ASCII graph (5-10 revisions)
    - `sample-status-with-files.txt`: jj status output with various file statuses
    - `sample-bookmarks.txt`: jj bookmark list output
    - `sample-log-no-description.txt`: All revisions with "no description set"
    - Only create fixtures for gaps identified in 8.2
  - [x] 8.4 Write up to 10 additional strategic tests maximum
    - Focus on end-to-end workflows: `jj-status` entry → navigation → staging → refresh
    - Test error paths: no described revision, immutable target, malformed output
    - Test integration: fetch + parse + render pipeline
    - Mock jj command responses using fixtures from 8.3
    - Do NOT write comprehensive coverage for all scenarios
    - Skip performance tests, accessibility tests unless critical
  - [x] 8.5 Run feature-specific tests only
    - Run ONLY tests related to magit-status feature
    - Expected total: approximately 24-66 tests maximum
    - Use test file: `tests/test-jj-status.el` (create if needed)
    - Command: `eask test buttercup tests/test-jj-status.el`
    - Do NOT run entire jj.el test suite
    - Verify all critical workflows pass

**Acceptance Criteria:**
- All feature-specific tests pass (approximately 24-66 tests total)
- Critical user workflows covered: status view → navigate → stage → refresh
- No more than 10 additional tests added by this task group
- Test fixtures created for integration testing
- Tests use mocking pattern from existing test-jj.el

---

#### Task Group 9: Documentation & Help Integration
**Recommended Skills:** Technical writing, Emacs documentation, transient menus
**Dependencies:** All previous task groups

- [x] 9.0 Complete documentation for magit-status feature
  - [x] 9.1 Update README.md with status buffer features
    - Add "Status Buffer" section under features
    - Document keybindings in a table format
    - Include ASCII art example of status buffer layout
    - Explain "last described revision" concept clearly
    - Provide staging workflow example
    - Keep documentation concise (1-2 paragraphs + table)
  - [x] 9.2 Update `jj-status-popup` transient menu
    - Add staging command to menu: ("s" "Stage file" jj-status-stage-file)
    - Add refresh command: ("g" "Refresh" jj-status-refresh)
    - Add navigation help text
    - Keep menu organized by action type
  - [x] 9.3 Add docstrings to all public functions
    - Ensure all `jj-status-*` commands have docstrings
    - Follow checkdoc conventions for docstring format
    - Include usage examples where helpful
    - Document keybindings in command docstrings
  - [x] 9.4 Run documentation validation
    - Run: `eask lint checkdoc` to validate docstrings
    - Run: `eask compile` to verify byte-compile passes
    - Fix any warnings or errors
    - Ensure package-lint passes (if used)
  - [x] 9.5 Create CHANGELOG entry
    - Add entry for magit-status feature
    - List key capabilities: sectioned layout, graph visualization, file staging
    - Note keybindings: n/p/s/g/RET
    - Reference spec for details

**Acceptance Criteria:**
- README updated with status buffer section
- All public functions have proper docstrings
- Checkdoc validation passes
- Byte-compilation succeeds with no warnings
- Transient menu includes new commands

---

## Execution Order

**Recommended implementation sequence:**

1. **Phase 1: Foundation** (Task Groups 1-2)
   - Build data collection and parsing infrastructure
   - Create foundation for all rendering and interaction

2. **Phase 2: Rendering** (Task Group 3)
   - Implement buffer display system
   - Make status buffer visually functional

3. **Phase 3: Interaction** (Task Groups 4-5)
   - Add navigation and staging capabilities
   - Make buffer interactive and useful

4. **Phase 4: Refresh & Polish** (Task Groups 6-7)
   - Implement refresh mechanisms
   - Integrate with existing commands

5. **Phase 5: Testing & Documentation** (Task Groups 8-9)
   - Fill test coverage gaps
   - Complete user-facing documentation

**Parallel execution opportunities:**
- Task Groups 1 and 2 could overlap (parsing can start while command execution is in progress)
- Task Groups 4 and 5 are independent and could be done in parallel
- Task Groups 8 and 9 could overlap (documentation while testing)

**Critical path:**
1 → 2 → 3 → (4 + 5) → 6 → 7 → (8 + 9)

## Complexity Estimates

**High Complexity (3-5 days):**
- Task Group 2: Output Parsing (complex string parsing, graph handling)
- Task Group 3: Buffer Rendering (text properties, faces, alignment)
- Task Group 5: File Staging (validation logic, error handling)

**Medium Complexity (2-3 days):**
- Task Group 1: Command Execution (straightforward, follows existing patterns)
- Task Group 4: Navigation (text property tracking, cursor management)
- Task Group 6: Buffer Refresh (cursor context preservation)

**Low Complexity (1-2 days):**
- Task Group 7: Auto-refresh (advice and hooks)
- Task Group 8: Testing (review and fill gaps)
- Task Group 9: Documentation (writing and validation)

**Total Estimated Effort:** 18-27 developer days

## Technical Notes

**Reusable Patterns from jj.el:**
- `jj--with-command` macro for all jj CLI invocations (lines 111-126)
- `jj--run-command` for low-level execution (lines 79-107)
- `jj-status-mode` as base mode (lines 62-66)
- Buffer naming: "jj: [project-name]" pattern (line 192)
- Error handling: `jj--handle-command-error` (lines 156-184)

**Testing with Buttercup:**
- Create `tests/test-jj-status.el` for new tests
- Mock `jj--run-command` using fixtures
- Follow patterns from existing `tests/test-jj.el`
- Use `describe`, `it`, `expect` structure
- Run with: `eask test buttercup`

**Emacs Version Compatibility:**
- Target: Emacs 28.1+ (per Package-Requires)
- Use lexical binding in all new code
- Avoid features newer than Emacs 28.1
- Test with `emacs -Q` to verify no external dependencies

**Performance Considerations:**
- Parse jj output once per refresh, cache results
- Use text properties efficiently (don't over-propertize)
- Minimize buffer re-rendering (only on explicit refresh)
- Test with repositories of 100+ revisions

**Integration Points:**
- Existing transient menus (jj-status-popup)
- Existing mode keybindings (jj-status-mode-map)
- Existing command functions (describe, abandon, new)
- Error handling infrastructure

## Success Metrics

**Feature Completion:**
- All 9 task groups completed and verified
- All task group acceptance criteria met
- Feature matches spec requirements

**Test Coverage:**
- 24-66 tests total for this feature
- All critical workflows tested (status → navigate → stage → refresh)
- Zero test failures in feature tests

**Quality Metrics:**
- `eask lint checkdoc` passes with no errors
- `eask compile` succeeds with no warnings
- Manual testing in real jj repository succeeds
- Buffer refresh completes in under 1 second

**User Experience:**
- Status buffer looks like Magit (visual similarity)
- Keybindings feel natural (n/p/s/g/RET/q)
- Error messages are clear and actionable
- No surprising behavior or edge case crashes
