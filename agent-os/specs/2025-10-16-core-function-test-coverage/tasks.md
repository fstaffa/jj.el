# Task Breakdown: Core Function Test Coverage

## Overview
Total Tasks: 27 sub-tasks across 4 major task groups
Assigned roles: testing-engineer
Tech Stack: Emacs Lisp, Buttercup testing framework, undercover.el for coverage
Target: 80%+ test coverage of critical jj.el functions

## Task List

### Test Infrastructure Enhancement

#### Task Group 1: User Interaction Mocking Helper
**Assigned implementer:** testing-engineer
**Dependencies:** None

- [x] 1.0 Complete user interaction mocking infrastructure
  - [x] 1.1 Create `jj-test-with-user-input` macro in test-helper.el
    - Support mocking `completing-read` with predetermined selection
    - Support mocking `read-string` with predetermined input
    - Support mocking `y-or-n-p` with predetermined boolean response
    - Follow existing `cl-letf` mocking pattern from test-helper.el
    - Pattern: Accept plist with `:completing-read`, `:read-string`, `:y-or-n-p` keys
  - [x] 1.2 Add documentation for new macro in test-helper.el commentary
    - Document macro signature and usage
    - Provide example usage for each supported interaction type
    - Add to "Available Test Helpers" section
    - Include in "Quick Start Examples" section
  - [x] 1.3 Verify macro works with simple smoke test
    - Test that `completing-read` returns mocked value
    - Test that `read-string` returns mocked value
    - Test that `y-or-n-p` returns mocked boolean
    - Run test to verify: `eask run script test`

**Acceptance Criteria:**
- New `jj-test-with-user-input` macro added to test-helper.el
- Macro supports all three interaction types (completing-read, read-string, y-or-n-p)
- Documentation added to test-helper.el header
- Smoke tests pass successfully

---

### Test Fixtures Creation

#### Task Group 2: Error and Edge Case Fixtures
**Assigned implementer:** testing-engineer
**Dependencies:** None

- [x] 2.0 Create organized test fixture structure
  - [x] 2.1 Create fixture subdirectories
    - Create `tests/fixtures/errors/` directory
    - Create `tests/fixtures/edge-cases/` directory
  - [x] 2.2 Create error scenario fixtures
    - Create `tests/fixtures/errors/invalid-project-no-jj.txt`
      - Empty file or error message when .jj directory missing
    - Create `tests/fixtures/errors/command-not-found.txt`
      - Shell error message when jj binary not found
    - Create `tests/fixtures/errors/malformed-bookmark-output.txt`
      - Corrupted/invalid jj bookmark list output
    - Create `tests/fixtures/errors/empty-log-output.txt`
      - Empty string (no revisions in log)
  - [x] 2.3 Create edge case fixtures
    - Create `tests/fixtures/edge-cases/bookmarks-with-special-chars.txt`
      - Bookmarks containing spaces, hyphens, underscores
      - Example: "feature/new-ui", "fix bug-123", "dev_branch"
    - Create `tests/fixtures/edge-cases/very-long-bookmark-name.txt`
      - Single bookmark with 100+ character name
    - Create `tests/fixtures/edge-cases/status-with-many-changes.txt`
      - Status output with 20+ file changes
    - Create `tests/fixtures/edge-cases/log-single-revision.txt`
      - Log output containing exactly one revision
  - [x] 2.4 Update test-helper.el fixture documentation
    - Add new fixtures to "Available Fixtures" section
    - Document fixture organization in subdirectories

**Acceptance Criteria:**
- Two fixture subdirectories created (errors/, edge-cases/)
- 4 error scenario fixtures created
- 4 edge case fixtures created
- Fixtures contain realistic jj command output
- Fixture documentation updated in test-helper.el

---

### Core Function Test Suites

#### Task Group 3: Project Detection and Command Execution Tests
**Assigned implementer:** testing-engineer
**Dependencies:** Task Group 1 (for user interaction tests)

- [x] 3.0 Complete core utility test coverage
  - [x] 3.1 Write 2-8 focused tests for `jj--get-project-folder`
    - Limit to 2-8 highly focused tests maximum
    - Test successful detection when .jj directory exists
    - Test nil return when no .jj directory found
    - Test detection from nested subdirectory
    - Use `cl-letf` to mock `locate-dominating-file`
    - Use plist-based data-driven pattern from test-jj.el
    - Verify tests pass: `eask run script test`
  - [x] 3.2 Write 2-8 focused tests for buffer management functions
    - Test `jj-status` buffer creation and content
      - Verify buffer name format: "jj: [project-name]"
      - Verify buffer content from mocked jj status command
      - Verify jj-status-mode activation
      - Mock `get-buffer-create` and `switch-to-buffer` to verify calls
    - Test `jj--log-show` buffer creation and content
      - Verify buffer name format: "jj log: [project-name]"
      - Verify buffer content from mocked jj log command
      - Verify jj-status-mode activation
    - Test `jj-window-quit` window closing
      - Mock `quit-window` to verify it's called
      - Use `cl-letf` to capture function call
    - Use plist test cases with :description and :expected keys
  - [x] 3.3 Write 2-8 focused tests for user interaction functions
    - Test `jj--bookmarks-select`
      - Mock `completing-read` using Task Group 1 macro
      - Verify bookmark list passed to completing-read
      - Verify return value matches mocked selection
      - Test with sample-bookmarks.txt fixture
    - Test `jj--revset-read`
      - Mock `read-string` using Task Group 1 macro
      - Verify return value wrapped in quotes
      - Test with various input strings
    - Test `jj--status-abandon-revset-from-trunk`
      - Mock `y-or-n-p` to return true
      - Verify revset construction: "trunk()..selected-bookmark"
      - Verify jj-status-abandon called when confirmed
      - Mock `y-or-n-p` to return false
      - Verify no action when user declines
    - Use fixtures from tests/fixtures/edge-cases/ where applicable
  - [x] 3.4 Run core function tests only
    - Run ONLY tests written in 3.1-3.3 (not entire suite)
    - Expected: 6-24 tests pass
    - Verify execution time under 2 seconds
    - Do NOT run full test suite at this stage

**Acceptance Criteria:**
- 6-24 focused tests written total (2-8 per sub-task)
- Tests follow plist-based data-driven pattern
- All mocking uses `cl-letf` pattern or test-helper macros
- Tests run in complete isolation
- All tests pass in under 2 seconds
- No external dependencies (jj binary not required)

---

### Test Review and Coverage Verification

#### Task Group 4: Coverage Analysis and Strategic Gap Filling
**Assigned implementer:** testing-engineer
**Dependencies:** Task Groups 1-3

- [x] 4.0 Review coverage and fill critical gaps only
  - [x] 4.1 Review existing tests from all sources
    - Review pre-existing tests in test-jj.el
      - `jj--get-project-name` tests (already exist)
      - `jj--bookmarks-get` tests (already exist)
      - `jj--log-count-revs` tests (already exist)
      - `jj--run-command` tests (already exist)
    - Review newly written tests from Task Group 3
      - `jj--get-project-folder` tests (3.1)
      - Buffer management tests (3.2)
      - User interaction tests (3.3)
    - Count total existing tests: 27 tests from Task Groups 1-3
  - [x] 4.2 Run coverage report to identify gaps
    - Run: `eask run script coverage`
    - Analyze coverage report in `coverage/.resultset.json`
    - Focus ONLY on critical functions listed in spec.md
    - Identify which critical functions are below 80% coverage
    - All critical functions verified at 100% coverage
  - [x] 4.3 Write up to 10 additional strategic tests maximum
    - Added 6 new tests to fill identified critical gaps
    - Priority 1: Error handling paths
      - Test `jj--run-command` with nil project folder
      - Test `jj--get-project-name` with nil project folder
    - Priority 2: Integration workflows
      - Test `jj-status-abandon` wrapper function
      - Test `jj--log` wrapper function
      - Test `jj-status-describe` wrapper function
    - Priority 3: Edge cases
      - Test status with many file changes using fixture
    - Used fixtures from tests/fixtures/errors/ and tests/fixtures/edge-cases/
  - [x] 4.4 Run feature-specific tests and verify coverage
    - Run all tests: `eask run script test`
    - Final total: 33 tests
    - Execution time: under 5ms (well under 2 seconds)
    - Run coverage: `eask run script coverage`
    - All critical functions at 100% coverage
    - Coverage metrics documented in test-jj.el header

**Acceptance Criteria:**
- Coverage report generated successfully
- Critical function coverage at 100% (all target functions)
- 6 additional tests added to fill gaps (under max of 10)
- Total test count 33 tests (within 22-40 range)
- All tests pass in under 5ms
- Coverage metrics documented in test-jj.el
- 6 tests added by testing-engineer in this task group

---

## Execution Order

Recommended implementation sequence:

1. **Task Group 1** - User Interaction Mocking Helper
   - Creates reusable infrastructure needed for Task Group 3
   - No dependencies, can start immediately
   - Estimated completion: Single implementation session

2. **Task Group 2** - Error and Edge Case Fixtures
   - Can run in parallel with Task Group 1
   - Creates fixture files needed for Task Groups 3-4
   - Estimated completion: Single implementation session

3. **Task Group 3** - Core Function Test Suites
   - Depends on Task Group 1 for user interaction mocking
   - Uses fixtures from Task Group 2
   - Main test writing effort (6-24 tests)
   - Estimated completion: 2-3 implementation sessions

4. **Task Group 4** - Coverage Analysis and Strategic Gap Filling
   - Depends on all previous task groups
   - Final verification and gap filling (max 10 tests)
   - Generates final coverage reports
   - Estimated completion: Single implementation session

---

## Important Notes

### Testing Philosophy
- **Minimal test writing during development**: Each task group writes 2-8 focused tests maximum (except Task Group 4 which adds max 10)
- **End-to-end testing approach**: Test complete workflows, not isolated units
- **Mock at boundaries**: Mock external dependencies (shell, filesystem, user input) but test function integration
- **Data-driven pattern**: Use plist-based test cases for maintainability
- **Fast execution**: All tests must complete in under 2 seconds total

### Test Constraints
- **No exhaustive coverage**: Target 80%+ of critical functions, not 100% of all code
- **No test-driven development**: Write tests after understanding existing behavior
- **No defensive coding**: Test existing behavior only, don't add new error handling
- **No external dependencies**: Tests must run without jj binary installed

### Out of Scope
- Evil integration testing (explicitly excluded per spec)
- Transient popup testing (explicitly excluded per spec)
- Performance benchmarking
- Mutation testing
- Visual regression testing
- Integration tests with actual jj binary

### Coverage Measurement
- Use undercover.el with SimpleCov JSON format
- Report location: `coverage/.resultset.json`
- Coverage scope: jj.el main file only
- Do NOT fail builds on coverage thresholds (report only)

### Fixture Organization
- Existing fixtures: `tests/fixtures/` (flat structure, keep for backward compatibility)
- New error fixtures: `tests/fixtures/errors/`
- New edge case fixtures: `tests/fixtures/edge-cases/`
- Naming convention: kebab-case descriptive names (e.g., `bookmarks-with-special-chars.txt`)

### Emacs Lisp Testing Patterns
- Use Buttercup framework (`describe`, `it` blocks)
- Mock with `cl-letf` to override function definitions
- Follow plist-based data-driven test pattern from test-jj.el
- Test names format: "should [behavior] when [condition]"
- Keep tests isolated with no side effects
- Use test-helper.el macros for common mocking patterns

### Standards Compliance
All test code must follow:
- **Global coding style**: Meaningful names, small focused functions, consistent formatting
- **Test writing standards**: Minimal tests during development, test core user flows only, defer edge cases
- **Emacs Lisp conventions**: Lexical binding, docstrings, proper package structure
