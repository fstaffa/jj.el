# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Added

- **Magit-like Status Buffer**: Comprehensive status buffer view with three-section layout (Working Copy Changes, Revisions, and Bookmarks)
  - ASCII graph visualization of revision stack showing parent-child relationships
  - File staging workflow: stage individual files to the last described revision using `s` key
  - Navigation system: move between files and revisions using `n` (next) and `p` (previous) keys
  - Refresh functionality: manually refresh buffer state with `g` key
  - Interactive transient menu (`?` key) with organized command groups for navigation and file operations
  - "Last described revision" concept: automatically targets most recent revision with a description for staging operations
  - Keybindings:
    - `n` - Move to next item (file or revision)
    - `p` - Move to previous item (file or revision)
    - `s` - Stage file at point to last described revision
    - `g` - Refresh status buffer
    - `RET` - Show diff (planned for future release)
    - `q` - Quit status buffer
    - `?` - Show help/command menu

For implementation details, see `agent-os/specs/2025-10-17-magit-like-status-buffer/spec.md`
