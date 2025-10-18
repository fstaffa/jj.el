# Spec Requirements: Magit-like Status Buffer

## Initial Description
Improving the jj status buffer visualization to look similar to what magit shows. The feature should:
- Show staged changes
- List of revisions from last immutable revision
- Improve the overall visualization to be more like magit's status buffer

## Requirements Discussion

### First Round Questions

Based on the initial specification and the orchestrator's conversation flow, the following requirements were identified and clarified:

**Q1: What does "staged changes" mean in the context of jj?**
**Answer:** In jj, staging means targeting changes to a specific revision. The "last named revision" is the last revision that has a description set (not 'no description set'). Users can stage files to this revision using `jj squash` with the file name. Pressing 's' on a file in the status buffer should automatically stage it to this last described revision.

**Q2: How should revisions be displayed in the list?**
**Answer:** The status buffer should display revisions from the last immutable revision to the working copy. Each revision should show:
- Change ID (with bold/grey formatting based on jj's built-in prefix detection for unique parts)
- Description
- Branches/bookmarks (if any)
- File changes summary

**Q3: What visualization improvements are needed?**
**Answer:** The buffer should be structured like Magit's status view with:
- Clear sections for different types of information
- File-level navigation
- Keyboard shortcuts for common operations
- Visual indicators for change status
- Graph visualization using ASCII art similar to `jj log --graph` showing parent-child relationships

**Q4: Should the status buffer auto-refresh?**
**Answer:** Only after specific commands that would modify the view (e.g., describe, abandon, squash). Not on a timer or filesystem watch, to avoid performance issues.

### Follow-up Questions

**Follow-up 1: Staging Mechanism Clarification**
**Question:** How exactly should the staging mechanism work? Should pressing 's' on a file automatically determine the target revision, or should users be prompted?
**Answer:**
- "Last named revision" = last revision which has description set (not 'no description set')
- Use `jj squash` with the file name for staging
- Pressing 's' on a file should automatically stage to this last described revision (implicit behavior, no prompt needed)

**Follow-up 2: Bold and Grey Formatting for Change IDs**
**Question:** How should the bold/grey formatting for change IDs be determined?
**Answer:** Use jj's built-in prefix detection for unique parts. This likely means querying jj for the minimal unique prefix and formatting accordingly.

**Follow-up 3: Auto-refresh Trigger Commands**
**Question:** Which specific commands should trigger auto-refresh?
**Answer:** Only commands that would modify the view:
- describe (changes revision descriptions)
- abandon (removes revisions)
- squash (moves changes between revisions)
- Other modification commands as they're implemented

**Follow-up 4: Graph Visualization Details**
**Question:** What exactly should the ASCII art graph show?
**Answer:** Use ASCII art similar to `jj log --graph` with parent-child relationships. This should visualize the revision DAG (directed acyclic graph) showing how revisions relate to each other.

### Existing Code to Reference

**Similar Features Identified:**

Based on analysis of the existing jj.el codebase:

- **Status Buffer Foundation:** `jj-status-mode` (lines 62-66 in jj.el)
  - Already exists as a derived mode from special-mode
  - Provides read-only buffer with quit keybinding
  - Can be extended with new keybindings and display logic

- **Buffer Management Pattern:** `jj-status` function (lines 188-198)
  - Shows how to create/reuse buffers with project-specific naming
  - Uses `jj--with-command` macro for execution and error handling
  - Pattern to follow for refresh behavior

- **Transient Menu Pattern:** `jj-status-popup` (lines 389-399)
  - Shows existing menu structure for status buffer operations
  - Currently includes describe, abandon, log, new, fetch, push
  - Can be extended with new staging operations

- **Command Execution Infrastructure:**
  - `jj--run-command` (lines 79-107): Core command execution with stdout/stderr capture
  - `jj--with-command` macro (lines 111-126): Standardized command execution pattern
  - Error handling: Lines 109-184 show comprehensive error management

- **Log Display:** `jj--log-show` function (lines 260-270)
  - Similar pattern for displaying formatted jj output
  - Uses dedicated buffer with special mode
  - Can inform how to display revision graphs

- **Revset Support:** `jj--revset-read` and log popup (lines 272-291)
  - Shows how to handle revset queries
  - Relevant for determining "last immutable revision" and revision ranges

## Visual Assets

### Files Provided:
No visual assets provided.

### Visual Insights:
No visual assets to analyze. Implementation will reference Magit's status buffer as the design inspiration, which is a well-known pattern in the Emacs community.

## Requirements Summary

### Functional Requirements

**1. Status Buffer Structure**
- Display sections organized like Magit's status view
- Show revisions from last immutable revision to working copy
- Display file changes in a structured, navigable format
- Include ASCII graph visualization of revision relationships

**2. Revision Display**
- Each revision shows:
  - Change ID with bold/grey formatting (unique prefix detection)
  - Description text
  - Associated branches/bookmarks
  - File changes summary
  - Position in the revision graph

**3. File-Level Staging**
- Keybinding: 's' on a file to stage it
- Mechanism: `jj squash <filename>` to last described revision
- Target determination: Automatically find last revision with description set
- No user prompt needed (implicit target selection)

**4. Keyboard Navigation**
- Navigate between files and revisions with standard keys (n/p for next/prev)
- Jump to sections
- Quick access to common operations via single-key bindings
- Integration with existing jj-status-popup for advanced operations

**5. Auto-refresh Behavior**
- Refresh after: describe, abandon, squash commands
- Manual refresh: Available via keybinding (likely 'g')
- No automatic background refresh to avoid performance issues

**6. Graph Visualization**
- ASCII art similar to `jj log --graph`
- Show parent-child relationships
- Visualize revision DAG structure
- Position graph alongside revision information

### Technical Implementation Details

**1. Command Integration**
- Use `jj log` with appropriate revset to fetch revision list
- Query format: `--revisions "immutable()..@"` or similar
- Parse output for revision metadata
- Use `jj status` for file changes in working copy
- Use `jj squash --from <source> <filename>` for staging operations

**2. Revset Queries**
- Last immutable revision: Use `immutable()` revset
- Described revisions: Query for revisions where description is not "no description set"
- Revision range: `immutable()..@` for status display

**3. Formatting and Display**
- Change ID prefix detection: Parse jj output or use jj's template system
- Bold text: Use Emacs faces for emphasis
- Grey text: Use dimmed face for non-unique portions
- Section headers: Clear visual separation (possibly with face attributes)

**4. Buffer Mode Extensions**
- Extend `jj-status-mode` with new keybindings:
  - 's': Stage file at point
  - 'n'/'p': Navigate between items
  - 'g': Manual refresh
  - 'q': Quit (already exists)
  - '?': Show help popup (already exists)
- Add navigation helpers to track cursor position (files vs revisions)

**5. Integration Points**
- Reuse `jj--with-command` macro for all jj invocations
- Follow existing error handling patterns
- Integrate with existing transient popup system
- Maintain buffer naming convention: "jj: [project-name]"

### Reusability Opportunities

**Components to Reuse:**
- `jj-status-mode` as base mode (extend with new keybindings)
- `jj--with-command` macro for command execution
- `jj--run-command` for low-level jj CLI interaction
- Buffer management pattern from `jj-status` function
- Error handling infrastructure (validation, error buffer writing)
- Transient popup pattern for complex operations

**Backend Patterns to Follow:**
- Command execution: Always use `jj--with-command` wrapper
- Buffer creation: Follow project-name-based naming
- Error handling: Leverage existing `jj--handle-command-error`
- Revset queries: Use string formatting with proper escaping

**Similar Features to Model After:**
- Log display (`jj--log-show`): Pattern for formatting structured output
- Describe popup: Integration of transient menus with buffer refresh
- Abandon operation: Example of command + refresh workflow

### Scope Boundaries

**In Scope:**
- Enhanced status buffer with Magit-like structure
- Revision list display from immutable to working copy
- File-level staging with 's' keybinding
- ASCII graph visualization of revisions
- Auto-refresh after modification commands
- Keyboard navigation between files and revisions
- Change ID formatting with unique prefix detection
- Manual refresh capability

**Out of Scope (Future Enhancements):**
- Interactive staging of partial file changes (hunk-level)
- Visual diff display within status buffer
- File content preview
- Inline conflict resolution
- Mouse/click interaction (keyboard-first approach)
- Customizable section visibility
- Performance optimizations (caching, async operations)
- Multi-file staging operations (batch staging)
- Undo/redo for staging operations
- Status buffer themes/color schemes beyond basic formatting

**Deferred to Related Features:**
- Diff viewing: Covered by roadmap item #5 (Diff Viewer)
- Interactive rebase: Roadmap item #7
- Stack visualization: Roadmap items #10-13 (Phase 3)
- Conflict resolution UI: Roadmap item #9

### Technical Considerations

**1. Integration with Existing System**
- Must maintain compatibility with Emacs 29.1+ (current minimum version per tech-stack.md)
- Must work with transient.el 0.8.0+ and s.el 1.13.0+ (existing dependencies)
- Should integrate with Evil mode keybindings if present
- Must pass existing CI/CD checks (package-lint, checkdoc, byte-compile)

**2. Testing Requirements**
- Unit tests for staging logic (determining last described revision)
- Integration tests with mocked jj CLI responses
- Tests for buffer navigation and keybindings
- Tests for refresh behavior after commands
- Tests for Change ID formatting logic

**3. Performance Constraints**
- Avoid expensive jj queries on every navigation action
- Parse jj output efficiently for large repositories
- Minimize buffer re-rendering on partial updates
- Consider caching revision metadata during single status view session

**4. Error Handling**
- Handle case when no described revision exists (all revisions are "no description set")
- Handle empty repository (no revisions)
- Handle failed squash operations (conflicts, invalid targets)
- Handle malformed jj output gracefully

**5. User Experience**
- Status buffer should feel responsive (sub-second refresh)
- Clear visual feedback for staging operations
- Helpful error messages when staging fails
- Consistent with existing jj.el interaction patterns
- Intuitive for users familiar with Magit

**6. Documentation Needs**
- Update README with status buffer features
- Document keybindings in transient help popup
- Add docstrings for all new functions (checkdoc compliance)
- Include usage examples for staging workflow

### Product Context Alignment

**Mission Alignment:**
This feature directly supports jj.el's mission to be a "Magit-like Emacs interface for Jujutsu" by improving the core status buffer to match Magit's familiar, production-quality interface. It makes complex version control operations more intuitive, which aligns with making Jujutsu adoption easier for Emacs users.

**Roadmap Position:**
This spec maps to roadmap item #6: "Status View Enhancements" from Phase 2. It includes:
- File-level navigation in status buffer (in scope)
- Inline file staging/unstaging actions (in scope: staging with 's')
- Refresh on focus (modified to: refresh after modification commands)
- Quick access to diff viewing (out of scope: deferred to roadmap item #5)

**User Persona Fit:**
- **Senior Developer:** Benefits from Magit-like familiarity, reducing learning curve
- **Open Source Maintainer:** Gains efficient staging workflow for managing multiple changesets
- **Learning Developer:** Visual revision structure helps understand jj's stacked changesets model

**Differentiators Supported:**
- **Magit UX Familiarity:** Directly implements Magit-inspired status buffer structure
- **Stacked Changesets as First-Class:** Graph visualization makes changeset relationships visible
- **Built for Emacs Ecosystem:** Uses standard Emacs patterns (major modes, keybindings)

### Standards Compliance

The implementation must adhere to all standards defined in:
- `agent-os/standards/global/tech-stack.md`: Use Emacs Lisp with lexical binding, s.el, transient.el
- `agent-os/standards/global/coding-style.md`: Follow Emacs Lisp conventions
- `agent-os/standards/global/error-handling.md`: Use existing error handling macros
- `agent-os/standards/global/conventions.md`: Follow naming conventions (jj-- prefix for internal)
- `agent-os/standards/testing/test-writing.md`: Write Buttercup tests with proper mocking

No conflicts with existing standards identified. The feature extends existing patterns rather than introducing new paradigms.
