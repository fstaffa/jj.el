# Spec Requirements: core-function-test-coverage

## Initial Description
Write comprehensive tests for existing core utilities: project detection (jj--get-project-folder), command execution (jj--run-command), buffer management, and error handling paths. Aim for 80%+ coverage of critical functions.

## Requirements Discussion

### First Round Questions

**Q1: Testing Style and Pattern**
I assume we should use the same Buttercup testing approach with plist-based data-driven testing pattern that's already established in test-jj.el. Is that correct?

**Answer:** Yes, use the same approach (Buttercup with plist-based data-driven testing pattern)

**Q2: Coverage Priorities**
I'm thinking we should prioritize 80%+ coverage on critical paths (happy paths, common edge cases) rather than 100% coverage including unlikely scenarios. Should we concentrate on critical cases, or do you want exhaustive coverage of all edge cases?

**Answer:** Concentrate on critical cases

**Q3: Error Scenarios**
For error handling tests, should we cover:
- Invalid project folder (no .jj directory)
- Command execution failures (jj binary not found)
- Empty command outputs
- Malformed jj output
Or are there specific error scenarios you want prioritized?

**Answer:** No specific errors to add beyond the standard ones mentioned

**Q4: Test Fixtures Organization**
I assume we should organize test fixtures in subdirectories by category (e.g., tests/fixtures/errors/, tests/fixtures/edge-cases/) similar to how sample-bookmarks.txt exists. Should we organize in subdirectories or keep all fixtures flat in tests/fixtures/?

**Answer:** Organize in subdirectories (tests/fixtures/errors/ and tests/fixtures/edge-cases/)

**Q5: Coverage Reporting**
Should we configure the test suite to fail if coverage drops below 80%, or just report coverage metrics without failing the build?

**Answer:** Don't fail on coverage thresholds

**Q6: Defensive Error Handling**
Are we purely testing existing behavior, or should we also add defensive error handling code to jj.el functions where it's currently missing (e.g., checking if jj--get-project-folder returns nil before using it)?

**Answer:** Purely testing existing behavior (no new error handling code)

**Q7: Mocking Strategy for User Interactions**
For functions like jj--bookmarks-select (which uses completing-read), jj--revset-read (uses read-string), and jj--status-abandon-revset-from-trunk (uses y-or-n-p), what is common approach in elisp packages for testing?

**Answer:** The user asked for guidance on common Emacs Lisp testing approaches for mocking user interactions.

**Follow-up Answer (Based on Research):**
Based on the existing test suite patterns in test-helper.el and test-jj.el, the standard approach is to use `cl-letf` to mock Emacs Lisp functions at the function level. For user interaction functions:
- Mock `completing-read` to return predetermined selections
- Mock `read-string` to return predetermined input strings
- Mock `y-or-n-p` to return predetermined boolean responses

This is the idiomatic Emacs Lisp testing approach and aligns with the existing mocking strategy already used for `shell-command-to-string` and `jj--get-project-folder` in the test suite.

**Q8: Testing Philosophy**
Should we test these functions in isolation (unit tests with mocking), or test end-to-end user workflows (integration tests)?

**Answer:** Test end-to-end

**Q9: Scope Exclusions**
Are there any specific features or functions we should explicitly exclude from this test coverage effort?

**Answer:** Ignore Evil and Transient for now

### Existing Code to Reference

No similar existing features identified for reference.

### Follow-up Questions

None needed - all clarifications received.

## Visual Assets

### Files Provided:
No visual assets provided.

### Visual Insights:
Not applicable - this is a testing specification without UI components.

## Requirements Summary

### Functional Requirements

**Core Functions to Test:**
1. Project detection: `jj--get-project-folder`
2. Command execution: `jj--run-command`
3. Buffer management functions:
   - `jj-status` (creates and manages status buffer)
   - `jj--log-show` (creates and manages log buffer)
   - `jj-window-quit` (closes jj mode windows)
4. Error handling paths for the above functions

**User Interaction Functions to Test:**
- `jj--bookmarks-select` (uses `completing-read`)
- `jj--revset-read` (uses `read-string`)
- `jj--status-abandon-revset-from-trunk` (uses `y-or-n-p`)

**Testing Approach:**
- Use Buttercup testing framework
- Apply plist-based data-driven testing pattern (established in test-jj.el)
- Test end-to-end workflows, not isolated units
- Use `cl-letf` to mock Emacs Lisp functions:
  - `shell-command-to-string` for command execution
  - `jj--get-project-folder` for project detection
  - `completing-read`, `read-string`, `y-or-n-p` for user interactions
- Organize test fixtures in subdirectories:
  - `tests/fixtures/errors/` for error scenarios
  - `tests/fixtures/edge-cases/` for edge case scenarios

**Coverage Goals:**
- Target 80%+ coverage of critical functions
- Focus on critical paths (happy paths, common edge cases)
- Test standard error scenarios:
  - Invalid project folder (no .jj directory)
  - Command execution failures (jj binary not found)
  - Empty command outputs
  - Malformed jj output
- Do not fail build on coverage thresholds (report only)

**Test Organization:**
- Follow existing pattern in test-jj.el
- Create describe blocks for each function under test
- Use data-driven approach with plist test case definitions
- Use descriptive test names: "should [behavior] when [condition]"
- Keep tests isolated with mocked dependencies

### Reusability Opportunities

**Existing Test Infrastructure:**
- Reuse `test-helper.el` utilities:
  - `jj-test-with-mocked-command` for mocking shell commands
  - `jj-test-with-project-folder` for mocking project detection
  - `jj-test-load-fixture` for loading test fixtures
  - `jj-test-setup-temp-dir` and `jj-test-cleanup-temp-dir` for temporary directories

**Existing Test Patterns:**
- Plist-based data-driven test pattern from test-jj.el
- Fixture organization and naming conventions
- Mocking strategy using `cl-letf`

**New Mocking Utilities to Create:**
- Helper macro for mocking user interaction functions (completing-read, read-string, y-or-n-p)
- Could be added to test-helper.el for reusability

### Scope Boundaries

**In Scope:**
- Comprehensive tests for project detection (`jj--get-project-folder`)
- Comprehensive tests for command execution (`jj--run-command`)
- Comprehensive tests for buffer management functions:
  - `jj-status`
  - `jj--log-show`
  - `jj-window-quit`
- Error handling path tests for above functions
- Tests for user interaction functions:
  - `jj--bookmarks-select`
  - `jj--revset-read`
  - `jj--status-abandon-revset-from-trunk`
- End-to-end workflow testing approach
- Fixture organization in subdirectories
- Coverage reporting (non-blocking)
- Testing existing behavior only (no new error handling code)

**Out of Scope:**
- Evil integration testing
- Transient popup testing
- Adding new defensive error handling to production code
- Failing builds on coverage thresholds
- Testing unlikely edge cases beyond critical paths
- 100% exhaustive coverage (targeting 80%+ of critical functions)
- Unit testing in complete isolation (end-to-end approach instead)

### Technical Considerations

**Testing Framework:**
- Buttercup (already in use)
- Emacs 28.1+ (per package requirements)

**Mocking Strategy:**
- Use `cl-letf` for function-level mocking (Emacs Lisp standard)
- Mock at Emacs Lisp function boundaries, not shell command level
- Preserve existing mocking patterns from test-helper.el

**Test Fixtures:**
- Organize in subdirectories under `tests/fixtures/`:
  - `tests/fixtures/errors/` - error scenario outputs
  - `tests/fixtures/edge-cases/` - edge case outputs
- Use descriptive filenames following existing convention
- Keep existing flat fixtures for backward compatibility

**Integration Points:**
- Test functions that interact with:
  - File system (project folder detection)
  - Shell commands (jj command execution)
  - Emacs buffers (buffer creation and management)
  - User input (completing-read, read-string, y-or-n-p)

**Coverage Measurement:**
- Report coverage metrics
- Target 80%+ for critical functions
- Do not block on coverage thresholds

**Dependencies to Mock:**
- `locate-dominating-file` (for project detection)
- `shell-command-to-string` (for command execution)
- `completing-read` (for bookmark selection)
- `read-string` (for user text input)
- `y-or-n-p` (for user confirmation)
- Buffer operations (get-buffer-create, switch-to-buffer, etc.)
