# Tech Stack

## Core Language & Runtime

### Language
- **Emacs Lisp**: Primary implementation language with lexical binding enabled
- **Minimum Version**: Emacs 29.1 (declared in Eask file)
- **Target Versions**: Emacs 29.4, 30.1, snapshot (for CI testing)

### Package Management
- **Eask**: Development task runner and dependency management
  - Build automation
  - Test execution
  - Linting and validation
  - Package distribution preparation
- **package.el**: Standard Emacs package system for user installation
- **ELPA/MELPA**: Distribution channels (MELPA planned for future)

---

## Dependencies

### Runtime Dependencies
- **s.el** (version 1.13.0+): String manipulation library
  - Used for: String joining, concatenation, splitting
  - Essential for: Command building, output parsing

- **transient.el** (version 0.8.0+): Transient menu framework
  - Used for: All interactive command popups and menus
  - Essential for: User interaction paradigm (Magit-style menus)

### Development Dependencies
- **buttercup**: BDD-style testing framework
  - Unit testing
  - Integration testing with mocked jj CLI
  - Test fixtures and assertions

- **undercover.el**: Code coverage reporting tool
  - Tracks line coverage during test execution
  - Generates SimpleCov-format JSON output
  - Integrates with GitHub Actions for coverage display

### Optional Integrations
- **Evil mode**: Vim-style keybindings (detected at runtime)
  - Provides: Normal mode keybindings for jj buffers
  - Integration: Conditional keybinding setup

---

## External Tools

### Version Control System
- **Jujutsu (jj)**: Core VCS being integrated
  - Version: Any recent stable release
  - Invoked via: Shell command execution
  - Communication: CLI flags for machine-readable output (`--no-pager`, `--color never`)

### Git Integration
- **Git**: Required for remote operations (jj's Git backend)
  - Used through: jj git fetch/push commands
  - Required for: Remote repository synchronization

---

## Development Environment

### Nix Integration
- **flake.nix**: Declarative development environment specification
  - Provides: Reproducible development shell
  - Includes: Emacs, Eask, jj, and other development tools

- **devenv**: Development environment manager
  - Used for: Simplified Nix-based development setup
  - Benefits: Consistent tooling across contributors

### Environment Manager
- **direnv**: Automatic environment activation
  - Configuration: .envrc file
  - Purpose: Auto-load Nix environment on directory entry

---

## Testing & Quality

### Test Framework
- **Buttercup**: Primary testing framework
  - Style: BDD (describe/it blocks)
  - Location: tests/ directory
  - Coverage: Unit and integration tests
  - Mocking: CLI command mocking for deterministic tests

### Linting & Validation
- **package-lint**: Package metadata and structure validation
  - Checks: Package header compliance, dependency declarations, package structure
  - Run via: `eask lint package`
  - CI Integration: Dedicated job on ubuntu-latest with Emacs 30.1
  - Fails on warnings for strict quality control

- **checkdoc**: Documentation string validation
  - Checks: Docstring formatting, completeness, style compliance
  - Run via: `eask lint checkdoc`
  - CI Integration: Dedicated job on ubuntu-latest with Emacs 30.1
  - Fails on warnings to enforce documentation standards

- **byte-compilation**: Emacs Lisp compilation checks
  - Detects: Undefined functions, free variables, compilation warnings
  - Run via: `eask compile --strict`
  - CI Integration: Dedicated job on ubuntu-latest with Emacs 30.1
  - Fails on warnings to catch potential runtime issues

---

## CI/CD

### Continuous Integration
- **GitHub Actions**: Automated testing and validation
  - Workflows: Located in `.github/workflows/test.yml`
  - Pipeline Structure:
    - **Test Matrix**: 9 jobs (3 platforms × 3 Emacs versions)
      - Platforms: ubuntu-latest, macos-latest, windows-latest
      - Emacs Versions: 29.4, 30.1, snapshot
      - Test Framework: buttercup via `eask test buttercup`
      - Snapshot tests marked as experimental (allowed to fail)
    - **Linting Jobs**: 3 parallel jobs on ubuntu-latest with Emacs 30.1
      - package-lint: Package structure validation
      - checkdoc: Documentation validation
      - byte-compile: Compilation warnings check
      - All configured with `fail-fast: false` to show all issues
    - **Coverage Reporting**: Integrated with test matrix
      - Runs on ubuntu-latest with Emacs 30.1 only
      - Tool: undercover.el for line coverage tracking
      - Display: GitHub Actions Job Summary (markdown table)
      - PR Integration: Automatic PR comment updates via peter-evans/create-or-update-comment
      - Error Handling: Non-blocking with graceful degradation
  - Triggers: push to master, pull_request, workflow_dispatch
  - Concurrency: cancel-in-progress enabled for efficiency
  - Total CI Runtime: Typically under 10 minutes

### Aggregation Job
- **all-checks-pass**: Meta-job ensuring all checks succeed
  - Purpose: Single required status check for PR merging
  - Dependencies: [test, package-lint, checkdoc, byte-compile]
  - Behavior: Always runs (even if dependencies fail) via `if: ${{ always() }}`
  - Verification: Uses jq to check all dependent jobs succeeded
  - Handles experimental test failures appropriately

---

## Distribution

### Current Strategy
- **Git Only**: Direct installation from GitHub repository
  - Method: package-vc-install (Emacs 29+) or manual clone + load-path
  - URL: https://github.com/fstaffa/jj.el
  - Audience: Early adopters, testers

### Planned Distribution
- **MELPA**: Community package archive
  - Status: Not yet submitted
  - Requirements: package-lint compliance, stable API
  - Benefit: Simplified installation, automatic updates

---

## Integration Architecture

### CLI Interaction Layer
- **Command Execution**: Shell-based invocation of jj commands
  - Function: jj--run-command
  - Working Directory: Automatic project root detection via .jj folder
  - Output: Captured via shell-command-to-string
  - Error Handling: Command exit codes, stderr parsing

### Buffer Management
- **Major Modes**: Custom derived modes for specialized buffers
  - jj-status-mode: Status view (derived from special-mode)
  - Future: jj-log-mode, jj-diff-mode, jj-stack-mode

- **Buffer Naming**: Contextual names with project identification
  - Pattern: "jj: [project-name]", "jj log: [project-name]"

### User Interface Layer
- **Transient Menus**: All user interactions via transient.el
  - Popups: Command-specific option selection
  - Scopes: Stateful data (e.g., multi-parent revision lists)
  - Readers: Custom input handlers for revsets, messages, etc.

---

## Monitoring & Debugging

### Current Tooling
- **message function**: User-facing notifications
  - Command execution feedback
  - Operation success/failure messages

- **Debug Logging**: None (planned for future)

### Planned Enhancements
- **Structured Logging**: Debug-level logging for troubleshooting
- **Performance Profiling**: Emacs profiler integration for optimization
- **Error Reporting**: Improved error messages with actionable suggestions

---

## Platform Support

### Supported Platforms
- **Linux**: Primary development and testing platform
- **macOS**: Expected to work (jj and Emacs both supported)
- **Windows**: Expected to work via WSL or native Windows Emacs

### Limitations
- Requires: Unix-style shell for command execution
- Git backend: Limited by jj's Git integration capabilities
- CLI dependency: All features require jj executable in PATH
