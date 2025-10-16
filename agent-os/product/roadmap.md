# Product Roadmap

## Currently Implemented Features

The following features are already functional in jj.el:
- Status view with buffer management
- Log viewing with filtering options (revsets, patch, stat, graph control)
- Describe command (edit commit messages)
- Abandon changes (including batch operations from trunk)
- New change creation with multi-parent support
- Git fetch and push operations with remote management
- Transient-based menus for all major operations
- Evil mode keybinding support

---

## Phase 1: Testing Foundation and Quality Infrastructure

1. [x] **Testing Framework Setup** - Establish Buttercup-based test suite with fixtures, mocks for jj CLI interactions, and test runners integrated into Eask. Include documentation on running tests locally and patterns for test organization. `M`

2. [ ] **CI/CD Pipeline** - Configure GitHub Actions workflow for automated testing on multiple Emacs versions (28.1, 29.x, 30.x), linting with package-lint and checkdoc, and automated test reporting on pull requests. `M`

3. [ ] **Core Function Test Coverage** - Write comprehensive tests for existing core utilities: project detection (jj--get-project-folder), command execution (jj--run-command), buffer management, and error handling paths. Aim for 80%+ coverage of critical functions. `L`

4. [ ] **Error Handling Standardization** - Implement consistent error handling across all jj CLI invocations with user-friendly error messages, logging for debugging, and graceful degradation when jj commands fail. `S`

---

## Phase 2: Core Workflow Enhancement

5. [ ] **Diff Viewer** - Build dedicated diff buffer mode with syntax highlighting, hunk navigation (n/p keys), staging/unstaging support, and integration with status and log views. Should support both unified and split diff display. `L`

6. [ ] **Status View Enhancements** - Add file-level navigation in status buffer, inline file staging/unstaging actions, refresh on focus, and quick access to diff viewing for individual files. Integrate with existing describe/abandon workflows. `M`

7. [ ] **Interactive Rebase** - Implement jj rebase command interface with Transient popup for source/destination selection, conflict detection and resolution workflow, and visual feedback on rebase progress. `L`

8. [ ] **Bookmark Management** - Create dedicated interface for bookmark operations: create, delete, rename, track/untrack remotes, and list with metadata (tracking status, ahead/behind counts). `M`

9. [ ] **Conflict Resolution UI** - Build buffer mode for viewing and resolving conflicts with side-by-side comparison, action keys for choosing left/right/both, and integration with jj resolve command. `L`

---

## Phase 3: Stacked Changesets Specialization

10. [ ] **Stack Visualization** - Design ASCII/Unicode-based visual representation of changeset stacks in dedicated buffer, showing parent-child relationships, change descriptions, and health indicators (conflicts, empty changes). `XL`

11. [ ] **Stack Navigation Commands** - Implement keyboard-driven navigation between changes in a stack (up/down parent/child), jump to stack root/tip, and preview change details without leaving stack view. `M`

12. [ ] **Stack Reordering** - Create interactive interface for reordering changes within a stack using transpositions, with visual feedback, conflict prediction, and undo support. `L`

13. [ ] **Multi-Change Operations** - Enable batch operations on changeset ranges (squash multiple changes, split change into multiple, bulk describe, bulk abandon) with preview and confirmation workflows. `L`

---

## Phase 4: Polish, Performance, and Distribution

14. [ ] **Performance Optimization** - Profile and optimize buffer refreshes, implement caching for expensive jj queries (log, bookmarks), and add async command execution for long-running operations like fetch/push. `M`

15. [ ] **Documentation Suite** - Write comprehensive user documentation including quickstart guide, feature walkthroughs with screenshots, keybinding reference, and comparison guide for Magit users. `M`

16. [ ] **Customization System** - Expose user customization options via defcustom: default log limits, preferred diff display, keybinding overrides, and template customization for log/status displays. `S`

17. [ ] **MELPA Preparation** - Ensure package-lint compliance, write proper package metadata, create installation documentation, and submit to MELPA stable for broader distribution. `S`

---

> Notes
> - Roadmap assumes Emacs 28.1+ as minimum version and Jujutsu CLI installed
> - Each item represents a complete, end-to-end feature that can be tested independently
> - Phases are ordered to build foundation (testing/quality) before expanding feature surface area
> - Stacked changeset features (Phase 3) are the key differentiator and come after core workflows are solid
> - Testing and quality remain ongoing concerns throughout all phases, not just Phase 1
