# Specification: Magit-like Status Buffer for jj.el

## Goal

Transform the jj status buffer into a Magit-inspired interface with sectioned layout, revision visualization with ASCII graphs, and file-level staging capabilities. This makes jj.el familiar to Magit users while exposing jj's stacked changesets model in an intuitive visual format.

## User Stories

- As a senior developer familiar with Magit, I want a status buffer that looks and feels like Magit so that I can be productive immediately without learning a new interface
- As an open source maintainer, I want to stage individual files to specific revisions with a single keystroke so that I can efficiently organize changes across my stack
- As a developer learning jj, I want to see an ASCII graph visualization of my revision stack so that I understand how my changes relate to each other
- As a user managing multiple revisions, I want to navigate between revisions and files using keyboard shortcuts so that I can quickly review and modify my work
- As a developer making changes, I want the status buffer to automatically refresh after modification commands so that I always see the current state

## Core Requirements

### Functional Requirements

**1. Sectioned Status Buffer Layout**
- Display three main sections: Working Copy, Revisions, Bookmarks
- Working Copy section shows files with status indicators (Added, Modified, Removed)
- Revisions section displays revision list from `immutable_heads()..@` using revset query
- Bookmarks section lists active bookmarks with associated revisions
- Each section has a clear header with visual separation

**2. Revision Visualization**
- Display ASCII graph using jj log --graph output format
- Show parent-child relationships in the revision DAG
- Each revision entry includes:
  - Change ID with bold unique prefix (using jj's built-in prefix detection)
  - Description text (or "no description set" placeholder)
  - Associated bookmarks/branches (if any)
  - File changes summary (number of files added/modified/removed)
- Graph positioned alongside revision information for spatial context

**3. File-Level Staging**
- Press 's' on any file to stage it to the last described revision
- "Last described revision" = most recent revision with description not equal to "no description set"
- Staging executes: `jj squash --from @ <filename>` targeting the identified revision
- Operation fails gracefully if target revision is immutable (with clear error message)
- No user prompt for target selection (automatic/implicit behavior)
- Visual feedback after successful staging

**4. Keyboard Navigation**
- 'n' / 'p': Navigate to next/previous item (file or revision)
- 'RET': Open diff for file or revision at point
- 's': Stage file at point
- 'g': Manual refresh of status buffer
- 'q': Quit status buffer window
- '?': Show help popup (existing jj-status-popup)
- Navigation wraps within sections but respects section boundaries

**5. Auto-refresh Behavior**
- Refresh status buffer after these commands:
  - `jj describe` (changes revision descriptions)
  - `jj abandon` (removes revisions)
  - `jj squash` (moves changes between revisions)
  - `jj new` (creates new revision)
- No automatic background refresh (no timers or file watchers)
- Manual refresh always available via 'g' keybinding
- Preserve cursor position after refresh when possible

**6. Change ID Formatting**
- Query jj for unique prefix length using template system
- Bold the unique prefix portion
- Grey (dimmed) the non-unique suffix
- Example: **qpvuntsm**qxuquz57 (if qpvuntsm is unique)
- Fallback to full change ID if prefix detection fails

### Non-Functional Requirements

**Performance**
- Status buffer refresh completes in under 1 second for repositories with up to 100 revisions
- Initial buffer render under 500ms for typical repositories
- Parse jj command output efficiently without blocking UI
- Cache revision metadata during single status view session

**Accessibility**
- All features accessible via keyboard (no mouse required)
- Clear visual hierarchy using Emacs faces
- Consistent with Emacs conventions for buffer navigation

**Compatibility**
- Works with Emacs 28.1+ (current minimum version)
- Integrates with existing transient.el 0.8.0+ dependency
- Compatible with Evil mode keybindings if present
- Maintains existing jj.el command execution patterns

## Visual Design

### Buffer Structure

```
jj: [project-name]

Working Copy Changes
  A  src/new-file.el
  M  src/existing.el
  R  src/old-file.el

Revisions (immutable_heads()..@)
  @  qpvuntsmqxuquz57  Working copy
  │  Added: 1  Modified: 1  Removed: 1
  │
  ◉  yqosqzytrlsw  Add new feature
  │  [main]
  │  Added: 2  Modified: 3
  │
  ◉  mzvwutvlkqwt  Fix bug in parser
  │  Added: 0  Modified: 1
  │
  ~  (immutable)

Bookmarks
  main      → yqosqzytrlsw
  dev       → mzvwutvlkqwt
```

### Section Headers
- Use Emacs face attributes for emphasis (bold, underline)
- Clear visual separation between sections (blank lines)
- Section names descriptive and concise

### File Status Indicators
- 'A': Added (new file)
- 'M': Modified (existing file changed)
- 'R': Removed (deleted file)
- '?': Untracked (not in version control)

### Graph Symbols
- '@': Working copy revision
- '◉': Regular revision
- '│': Parent-child connection (vertical)
- '~': Immutable boundary marker

### Color/Face Usage
- Bold: Unique Change ID prefix, section headers
- Dimmed/Grey: Non-unique Change ID suffix
- Default: Normal text (descriptions, file names)
- Consider bookmark highlighting (optional enhancement)

### Responsive Behavior
- Buffer width adapts to window size
- Long descriptions wrap appropriately
- Graph maintains alignment with revision info

## Reusable Components

### Existing Code to Leverage

**Buffer Infrastructure**
- `jj-status-mode`: Base major mode for status buffer (lines 62-66 in jj.el)
  - Already derived from special-mode with read-only protection
  - Extend keybindings for new navigation and staging commands
  - Reuse buffer-read-only management pattern

**Command Execution**
- `jj--with-command` macro (lines 111-126): Standardized command execution wrapper
  - Handles repository validation automatically
  - Provides consistent error handling
  - Returns structured result: (success stdout stderr exit-code)
- `jj--run-command` (lines 79-107): Low-level jj CLI invocation
  - Manages stdout/stderr separation using cons cell format
  - Executes from project root automatically
  - Includes debug logging support

**Error Handling**
- `jj--validate-repository` (lines 128-136): Repository detection
- `jj--handle-command-error` (lines 156-184): Error categorization and signaling
- `jj--write-error-buffer` (lines 138-154): Error context logging
- All follow established patterns for user-error vs error signaling

**Buffer Management**
- `jj-status` function pattern (lines 188-198): Buffer creation and display
  - Project-specific naming: "jj: [project-name]"
  - Buffer reuse with erase-buffer for refresh
  - switch-to-buffer for display

**Transient Integration**
- `jj-status-popup` (lines 389-399): Existing action menu
  - Extend with staging operations if needed
  - Maintain existing command structure
  - Integrate manual refresh trigger

**Related Functions**
- `jj--log-show` (lines 260-270): Pattern for displaying formatted jj output
- `jj--bookmarks-get` (lines 232-235): Bookmark list parsing
- `jj--revset-read` (lines 272-274): Revset input handling

### New Components Required

**Status Buffer Renderer**
- Parse jj log output with --graph flag for revision list
- Parse jj status output for working copy file changes
- Render sectioned layout with proper formatting
- Apply faces for Change ID formatting (bold/grey)
- Component doesn't exist yet; needs custom text insertion logic

**Navigation System**
- Track cursor position across sections and items
- Implement next/previous navigation with section awareness
- Identify item type at point (file vs revision)
- No existing component handles multi-section navigation

**Staging Logic**
- Find last described revision from revision list
- Validate target revision is not immutable
- Construct and execute jj squash command
- Handle staging errors specifically
- New logic required; existing commands don't implement staging

**Revision Metadata Parser**
- Parse jj log output with custom template for Change ID, description, bookmarks
- Extract unique prefix information from jj template output
- Build internal data structure for revision list
- No existing parser handles this specific format

**File Status Parser**
- Parse jj status output for file change list
- Extract status indicators (A/M/R) and file paths
- Build internal data structure for file list
- Simpler than current plain text display in `jj-status`

## Technical Approach

### Database
Not applicable (Emacs Lisp package, no database required)

### API

**jj CLI Commands to Execute**

1. Revision list query:
```bash
jj log --revisions "immutable_heads()..@" --graph --no-pager --color never \
  -T 'change_id ++ "\n" ++ description ++ "\n" ++ bookmarks ++ "\n"'
```

2. Working copy status:
```bash
jj status --no-pager --color never
```

3. Staging operation:
```bash
jj squash --from @ --into <target-revision-id> <filename>
```

4. Bookmark list:
```bash
jj bookmark list --no-pager --color never -T 'name ++ "\t" ++ change_id ++ "\n"'
```

**Data Flow**

1. User invokes `jj-status`
2. Execute jj commands in parallel (status + log + bookmarks)
3. Parse output into structured data
4. Render buffer sections in order: Working Copy, Revisions, Bookmarks
5. Set up keybindings and buffer-local state
6. Display buffer to user

### Frontend

**Major Mode Extension**

Extend `jj-status-mode` with new keybindings:
```elisp
(define-key jj-status-mode-map (kbd "n") #'jj-status-next-item)
(define-key jj-status-mode-map (kbd "p") #'jj-status-prev-item)
(define-key jj-status-mode-map (kbd "s") #'jj-status-stage-file)
(define-key jj-status-mode-map (kbd "g") #'jj-status-refresh)
(define-key jj-status-mode-map (kbd "RET") #'jj-status-show-diff)
```

**Buffer Rendering Functions**

- `jj-status--render-buffer`: Main rendering coordinator
- `jj-status--render-working-copy`: Render Working Copy section
- `jj-status--render-revisions`: Render Revisions section with graph
- `jj-status--render-bookmarks`: Render Bookmarks section
- `jj-status--format-change-id`: Apply bold/grey faces to Change ID
- `jj-status--parse-log-output`: Parse jj log output into data structure
- `jj-status--parse-status-output`: Parse jj status output into file list

**Navigation Functions**

- `jj-status-next-item`: Move to next file or revision
- `jj-status-prev-item`: Move to previous file or revision
- `jj-status--item-at-point`: Identify item type and data at cursor
- `jj-status--section-at-point`: Identify current section

**Staging Functions**

- `jj-status-stage-file`: Stage file at point to last described revision
- `jj-status--find-last-described-revision`: Locate target revision for staging
- `jj-status--validate-staging-target`: Check if target is mutable

**Refresh Function**

- `jj-status-refresh`: Re-execute queries and re-render buffer

**Data Structures**

Use alists or plists for internal representation:
```elisp
;; Revision structure
(:change-id "qpvuntsmqxuquz57"
 :unique-prefix "qpvuntsm"
 :description "Add new feature"
 :bookmarks ("main")
 :files-added 2
 :files-modified 3
 :files-removed 0
 :graph-prefix "@  ")

;; File structure
(:path "src/file.el"
 :status "M")
```

### Testing

**Unit Tests (Buttercup)**

Following existing test patterns in test-jj.el:

1. **Parser Tests**
   - Test `jj-status--parse-log-output` with fixture data
   - Test `jj-status--parse-status-output` with various status outputs
   - Test Change ID formatting logic
   - Use data-driven test pattern with plist test cases

2. **Navigation Tests**
   - Test next/previous navigation across sections
   - Test item identification at point
   - Mock buffer content and cursor position

3. **Staging Logic Tests**
   - Test finding last described revision
   - Test validation of immutable revisions
   - Mock jj command responses

4. **Rendering Tests**
   - Test buffer structure generation
   - Test section rendering with sample data
   - Verify face application

5. **Integration Tests**
   - Test full status buffer refresh workflow
   - Test staging with auto-refresh
   - Mock all jj commands using `jj-test-with-mocked-command`

**Test Fixtures**

Create fixtures in tests/fixtures/:
- `magit-status/sample-log-with-graph.txt`: jj log output with ASCII graph
- `magit-status/sample-status-with-files.txt`: jj status output with file changes
- `magit-status/sample-bookmarks.txt`: jj bookmark list output

**Test Coverage Goals**

- Core user flows: 100% (staging, navigation, refresh)
- Parsing functions: 100%
- Rendering functions: 80%+ (visual output harder to test)
- Error handling: Test immutable staging rejection
- Focus on critical paths per test-writing.md guidelines

## Out of Scope

The following features are explicitly excluded from this specification and may be addressed in future iterations:

**Deferred Features**
- Inline diff display within status buffer (roadmap item #5: Diff Viewer)
- Hunk-level staging (partial file staging)
- File content preview pane
- Inline conflict resolution UI (roadmap item #9)
- Section folding/expansion
- Multi-file batch staging operations
- Undo/redo for staging operations
- Customizable section visibility preferences

**Mouse/Click Interaction**
- Clicking on files or revisions (keyboard-first approach)
- Drag-and-drop operations
- Context menus

**Performance Optimizations**
- Asynchronous command execution (initial version is synchronous)
- Incremental rendering for very large repositories
- Background refresh or file watching

**Theming/Customization**
- Custom color schemes beyond basic faces
- User-configurable section order
- Customizable graph symbols

**Advanced Visualization**
- Interactive revision graph (clickable nodes)
- Minimap or overview of full stack
- Visual branch/bookmark indicators beyond text

## Success Criteria

1. **Magit Familiarity**: Magit users can navigate and understand the status buffer without reading documentation
2. **Staging Efficiency**: Staging a file takes a single keystroke ('s') and completes in under 2 seconds
3. **Visual Clarity**: ASCII graph accurately represents revision relationships and is readable at a glance
4. **Navigation Speed**: Moving between 10 files/revisions takes under 3 seconds using keyboard shortcuts
5. **Reliability**: Refresh after modification commands succeeds 100% of time in valid repositories
6. **Error Handling**: Attempting to stage to immutable revision shows clear, actionable error message
7. **Test Coverage**: Core user flows (staging, navigation, refresh) have 100% test coverage with passing tests
8. **Performance**: Status buffer renders in under 500ms for repositories with up to 50 revisions and 20 changed files

## Technical Considerations

### Integration with Existing System

**Compatibility Requirements**
- Emacs 28.1+ minimum version (per Package-Requires in jj.el)
- transient.el 0.8.0+ for popup menus
- s.el 1.13.0+ for string manipulation
- Works alongside existing jj.el commands without conflicts

**Code Standards Compliance**
- Use lexical binding in all new code
- Follow Emacs Lisp naming conventions (jj-- prefix for internal functions)
- Pass checkdoc validation (docstrings for all functions)
- Pass package-lint checks
- Pass byte-compile without warnings

**Evil Mode Integration**
- Standard keybindings work in Evil normal state
- Consider Evil-specific bindings if commonly requested
- Test with Evil mode enabled

### Error Handling

**Repository Validation**
- Use `jj--validate-repository` before all operations
- Signal `user-error` if not in jj repository
- Clear error message: "Not in a jj repository"

**Command Failures**
- Staging to immutable revision: Signal `user-error` with message "Cannot stage to immutable revision"
- Empty repository (no revisions): Display empty Revisions section gracefully
- No described revision exists: Disable staging with message "No described revision found"
- Malformed jj output: Log warning, display partial results if possible
- Network failure during fetch: Propagate error from jj CLI

**Parsing Errors**
- Handle unexpected jj output format gracefully
- Log parsing failures to *Messages* buffer when jj-debug-mode enabled
- Fall back to plain text display if parsing fails

### Performance Constraints

**Command Execution**
- Execute jj commands sequentially (no parallelization in initial version)
- Consider caching during single status buffer session
- Avoid re-parsing on cursor movement (parse once per refresh)

**Buffer Rendering**
- Minimize text properties to reduce rendering overhead
- Use bulk insertion instead of character-by-character
- Batch face applications where possible

**Large Repositories**
- Test with repositories containing 100+ revisions
- Consider pagination or truncation if performance degrades
- Document performance characteristics in README

### User Experience

**Visual Feedback**
- Show "Refreshing..." message during status refresh
- Indicate staging in progress with temporary message
- Preserve cursor position after refresh when item still exists
- Move cursor to nearest valid item if current item removed

**Error Messages**
- Use `user-error` for user-fixable issues (staging to immutable)
- Use `error` for system issues (jj binary not found)
- Include actionable guidance in error messages
- Write detailed context to `*jj-errors*` buffer for debugging

**Keyboard Shortcuts**
- Consistent with Magit where applicable (n/p/s/g/q)
- Discoverable via '?' help popup
- Work in both vanilla Emacs and Evil mode

### Documentation Needs

**User Documentation**
- Update README.md with status buffer features section
- Document keybindings in a table
- Include screenshot or ASCII art example
- Explain "last described revision" concept clearly
- Provide staging workflow example

**Developer Documentation**
- Docstrings for all public functions (checkdoc compliant)
- Code comments explaining parsing logic
- Document internal data structures (revision/file plists)
- Add comments for jj template syntax usage

**Help System**
- Update `jj-status-popup` with new commands
- Include staging command in transient menu
- Add descriptions for all keybindings

### Implementation Phases

**Phase 1: Basic Structure (Minimum Viable Product)**
- Sectioned buffer layout (Working Copy, Revisions, Bookmarks)
- Basic revision list (no graph yet)
- File status display
- Navigation (n/p)
- Manual refresh (g)

**Phase 2: Visualization**
- ASCII graph integration
- Change ID formatting (bold/grey)
- Improved section styling

**Phase 3: Staging**
- Find last described revision logic
- File staging with 's' key
- Immutable revision validation
- Auto-refresh after staging

**Phase 4: Polish**
- Auto-refresh after describe/abandon/new commands
- Cursor position preservation
- Comprehensive error handling
- Performance optimization

### Dependencies

**External (Already in Package-Requires)**
- Emacs 28.1+
- transient.el 0.8.0+ (transient menus)
- s.el 1.13.0+ (string manipulation)

**Internal (Existing jj.el Components)**
- `jj--with-command` macro
- `jj--run-command` function
- `jj--validate-repository` function
- `jj--handle-command-error` function
- `jj-status-mode` major mode
- `jj--get-project-name` function

**jj CLI Version**
- Requires jj with `--graph` flag support (v0.8.0+)
- Template syntax for Change ID and bookmarks
- `immutable_heads()` revset function

### Security Considerations

**Input Validation**
- Validate file paths before passing to jj squash
- Escape special characters in revset queries
- Sanitize user input in interactive prompts

**Command Injection**
- Use `split-string` for command argument construction (existing pattern)
- Never use shell evaluation (existing code uses call-process)
- Quote arguments appropriately

**File System Access**
- Operate only within jj repository root
- Use `jj--get-project-folder` for path validation
- No arbitrary file access outside repository

### Accessibility

**Screen Reader Support**
- Use semantic Emacs faces (existing pattern)
- Ensure text is readable without color
- Provide text alternatives for graph symbols if needed

**Keyboard-Only Operation**
- All features accessible via keyboard (already designed)
- No mouse-required operations
- Standard Emacs navigation patterns

**Visual Clarity**
- High contrast between sections
- Clear visual hierarchy
- Readable at default font sizes
- ASCII graph works with monospace fonts
