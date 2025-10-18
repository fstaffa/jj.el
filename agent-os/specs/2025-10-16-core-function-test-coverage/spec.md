# Specification: Core Function Test Coverage

## Goal
Expand test coverage for jj.el core utilities to 80%+ by adding comprehensive end-to-end tests for project detection, command execution, buffer management, user interaction functions, and error handling paths using the established Buttercup testing framework with plist-based data-driven patterns.

## User Stories
- As a jj.el maintainer, I want comprehensive test coverage so that I can confidently refactor and extend functionality without introducing regressions
- As a contributor, I want clear test patterns to follow so that I can add tests for new features consistently
- As a developer, I want fast-running tests so that I can verify changes quickly during development
- As a project maintainer, I want coverage metrics reported so that I can track test quality over time

## Core Requirements

### Functional Requirements
- Achieve 80%+ test coverage of critical functions in jj.el
- Test core project detection function (jj--get-project-folder)
- Test core command execution function (jj--run-command)
- Test buffer management functions (jj-status, jj--log-show, jj-window-quit)
- Test user interaction functions (jj--bookmarks-select, jj--revset-read, jj--status-abandon-revset-from-trunk)
- Test error handling paths for invalid project folders, command failures, empty outputs, and malformed jj output
- Maintain existing plist-based data-driven test pattern from test-jj.el
- Use end-to-end testing approach rather than isolated unit tests
- Organize test fixtures in subdirectories for errors and edge cases
- Generate coverage reports without failing builds on coverage thresholds

### Non-Functional Requirements
- Tests must execute in under 2 seconds total
- All tests must run without requiring jj binary installation
- Tests must be completely isolated with no side effects
- Test code must be maintainable and follow existing patterns
- Coverage reporting must work with undercover.el and SimpleCov format

## Visual Design
Not applicable - this is a testing specification without UI components.

## Reusable Components

### Existing Code to Leverage
**Test Infrastructure (test-helper.el):**
- `jj-test-with-mocked-command` - Mock shell commands with predefined outputs
- `jj-test-with-project-folder` - Mock project folder detection
- `jj-test-load-fixture` - Load test fixtures from files
- `jj-test-setup-temp-dir` and `jj-test-cleanup-temp-dir` - Temporary directory management
- Mocking pattern using `cl-letf` for function-level mocking

**Existing Test Patterns (test-jj.el):**
- Plist-based data-driven test structure
- Fixture organization and naming conventions
- Test case structure with :description, :expected, :fixture/:output keys
- Describe blocks organized by function under test
- `dolist` iteration over test-cases to generate individual test specs

**Existing Test Fixtures:**
- `tests/fixtures/sample-bookmarks.txt` - Multi-line bookmark list
- `tests/fixtures/empty-bookmarks.txt` - Empty output
- `tests/fixtures/sample-log.txt` - Log command output
- `tests/fixtures/sample-status.txt` - Status command output

**Coverage Infrastructure:**
- `scripts/run-coverage-proper.el` - Coverage runner using undercover.el
- Eask script configuration for test and coverage commands
- SimpleCov JSON format for coverage reports

### New Components Required

**User Interaction Mocking Helper:**
- Macro to mock Emacs user interaction functions (completing-read, read-string, y-or-n-p)
- Should follow pattern of existing jj-test-with-mocked-command macro
- Add to test-helper.el for reusability
- Example: `jj-test-with-user-input` macro

**New Test Fixtures (tests/fixtures/errors/):**
- `invalid-project-no-jj.txt` - Error output when .jj directory missing
- `command-not-found.txt` - Error when jj binary not found
- `malformed-bookmark-output.txt` - Corrupted jj output
- `empty-log-output.txt` - Empty log result

**New Test Fixtures (tests/fixtures/edge-cases/):**
- `bookmarks-with-special-chars.txt` - Bookmarks with spaces, symbols
- `very-long-bookmark-name.txt` - Stress test bookmark parsing
- `status-with-many-changes.txt` - Large status output
- `log-single-revision.txt` - Minimal log output

**New Test Suites (add to test-jj.el):**
- `describe "jj--get-project-folder"` - Project detection tests
- `describe "jj-status"` - Status buffer creation and management
- `describe "jj--log-show"` - Log buffer creation and management
- `describe "jj-window-quit"` - Window closing behavior
- `describe "jj--bookmarks-select"` - Bookmark selection with completing-read
- `describe "jj--revset-read"` - Revset input with read-string
- `describe "jj--status-abandon-revset-from-trunk"` - Confirmation workflow with y-or-n-p

## Technical Approach

### Testing Framework
- Buttercup testing framework (already configured via Eask)
- Emacs 29.1+ (per package requirements in Eask file)
- Undercover.el for coverage measurement with SimpleCov JSON output
- Run tests via: `eask run script test`
- Run coverage via: `eask run script coverage`

### Mocking Strategy
**Function-Level Mocking with cl-letf:**
- Mock `locate-dominating-file` to return predefined project paths or nil
- Mock `shell-command-to-string` to return fixture data without executing commands
- Mock `completing-read` to return predetermined bookmark selections
- Mock `read-string` to return predetermined user input strings
- Mock `y-or-n-p` to return predetermined boolean responses
- Mock buffer operations (get-buffer-create, switch-to-buffer) to verify calls

**End-to-End Approach:**
- Test complete workflows from function entry to result
- Mock only at external boundaries (file system, shell, user input)
- Verify side effects (buffer creation, mode activation, command construction)
- Test function integration rather than isolated unit behavior

### Test Fixtures Organization

**Directory Structure:**
```
tests/fixtures/
├── sample-bookmarks.txt          [existing]
├── empty-bookmarks.txt           [existing]
├── sample-log.txt                [existing]
├── sample-status.txt             [existing]
├── errors/
│   ├── invalid-project-no-jj.txt
│   ├── command-not-found.txt
│   ├── malformed-bookmark-output.txt
│   └── empty-log-output.txt
└── edge-cases/
    ├── bookmarks-with-special-chars.txt
    ├── very-long-bookmark-name.txt
    ├── status-with-many-changes.txt
    └── log-single-revision.txt
```

**Fixture Naming Convention:**
- Use kebab-case descriptive names
- Include command context (bookmarks, log, status)
- Include scenario (empty, malformed, special-chars)
- Use .txt extension for text outputs

### Data-Driven Test Pattern

**Plist Structure for Test Cases:**
```elisp
(let ((test-cases
       '((:description "should handle normal case"
          :project-folder "/tmp/test/"
          :fixture "sample-status.txt"
          :expected expected-result)
         (:description "should handle error case"
          :project-folder nil
          :output ""
          :expected-error "No project folder"))))
  (dolist (test-case test-cases)
    (it (plist-get test-case :description)
      ;; Test implementation using plist-get
      )))
```

**Standard Plist Keys:**
- `:description` - Test case description (required)
- `:expected` - Expected result for assertions (required for success cases)
- `:expected-error` - Expected error message (required for error cases)
- `:fixture` - Fixture filename from tests/fixtures/
- `:output` - Inline output string (alternative to :fixture)
- `:project-folder` - Mock project folder path
- `:command` - Command string to test
- `:user-input` - Mocked user input for interaction functions
- `:confirm` - Boolean response for y-or-n-p mocking

### Coverage Measurement

**Coverage Tool Configuration:**
- Use undercover.el (already configured in Eask development dependencies)
- Report format: SimpleCov JSON (specified in run-coverage-proper.el)
- Output location: coverage/.resultset.json
- Coverage scope: jj.el main file only

**Coverage Goals:**
- Target 80%+ coverage for critical functions
- Measure line coverage via undercover.el
- Report coverage metrics for visibility
- Do NOT fail builds on coverage thresholds

**Excluded from Coverage:**
- Evil integration functions (out of scope)
- Transient popup definitions (out of scope)
- Commentary and documentation

### Functions to Test

**Priority 1 - Core Utilities (Critical):**
1. `jj--get-project-folder` - Project detection via locate-dominating-file
   - Test successful detection with .jj directory present
   - Test nil return when no .jj directory found
   - Test search from nested subdirectories

2. `jj--run-command` - Command execution from project root
   - Test command construction with proper arguments
   - Test execution from correct working directory
   - Test command output return
   - Test error when project folder is nil

3. `jj-status` - Status buffer creation and display
   - Test buffer creation with correct name format
   - Test buffer content from jj status command
   - Test jj-status-mode activation
   - Test buffer switching behavior

4. `jj--log-show` - Log buffer creation and display
   - Test buffer creation with correct name format
   - Test buffer content from jj log command
   - Test jj-status-mode activation
   - Test buffer switching behavior

5. `jj-window-quit` - Window closing
   - Test quit-window is called
   - Test window closes correctly

**Priority 2 - User Interaction Functions:**
1. `jj--bookmarks-select` - Bookmark selection with completing-read
   - Test completing-read called with bookmark list
   - Test return value matches user selection
   - Test with empty bookmark list

2. `jj--revset-read` - Revset input with read-string
   - Test read-string called with correct prompt
   - Test return value wrapped in quotes
   - Test empty string handling

3. `jj--status-abandon-revset-from-trunk` - Abandon workflow with confirmation
   - Test revset construction from trunk to selected bookmark
   - Test revision count calculation
   - Test y-or-n-p confirmation prompt
   - Test jj-status-abandon called when confirmed
   - Test no action when user declines

**Priority 3 - Error Handling Paths:**
1. Invalid project folder scenarios
   - jj--get-project-folder returns nil
   - jj--run-command called with nil project

2. Command execution failures
   - Empty command outputs
   - Malformed jj outputs

3. Buffer management errors
   - Buffer creation when buffer already exists
   - Mode activation on existing buffer

## Out of Scope

### Explicitly Excluded
- Evil integration testing (per requirements)
- Transient popup testing (per requirements)
- Adding new defensive error handling to production code (testing existing behavior only)
- Failing builds on coverage thresholds (report only)
- Testing unlikely edge cases beyond critical paths
- 100% exhaustive coverage (targeting 80%+ of critical functions)
- Unit testing in complete isolation (using end-to-end approach instead)

### Functions Not Prioritized
- `jj--get-project-name` - Already tested in test-jj.el
- `jj--bookmarks-get` - Already tested in test-jj.el
- `jj--log-count-revs` - Already tested in test-jj.el
- Transient popup definitions (jj-status-describe-popup, etc.)
- Command functions that just call jj--run-command (jj-status-describe, etc.)

### Future Enhancements
- Integration tests with actual jj binary
- Performance benchmarking of test suite
- Mutation testing for test quality
- Visual regression testing for buffer output formatting
- Testing Evil mode key bindings
- Testing Transient popup interactions

## Success Criteria

### Coverage Metrics
- At least 80% line coverage of jj.el core functions
- Coverage report generated in SimpleCov JSON format
- Coverage visible in CI/CD pipeline (not blocking)

### Test Quality
- All existing tests continue to pass
- New tests execute in under 2 seconds total
- Zero flaky tests (100% reproducible results)
- No external dependencies (jj binary not required)
- Complete test isolation (no side effects between tests)

### Code Quality
- All tests follow plist-based data-driven pattern
- Test names clearly describe behavior and conditions
- Fixtures organized in error/ and edge-cases/ subdirectories
- Mocking uses consistent cl-letf patterns from test-helper.el
- Test code passes Emacs Lisp linting (checkdoc)

### Documentation
- New test helper macros documented in test-helper.el header
- New test suites documented in test-jj.el header
- Fixture files have descriptive names indicating their purpose
- Test pattern examples added to test-helper.el commentary

### Maintainability
- New fixtures reusable across multiple test cases
- User interaction mocking helper reusable for future tests
- Test cases easily extendable via plist additions
- No duplication of test code across describe blocks
