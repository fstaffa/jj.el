# Task Breakdown: Test Data-Driven Refactor

## Overview
Total Tasks: 4 phases with 16 sub-tasks
Assigned roles: testing-engineer (primary refactoring work)

This refactoring converts all test suites in tests/test-jj.el from duplicative individual test cases to a data-driven approach using plist-based test tables with dolist iteration.

## Task List

### Phase 1: Simple Test Suite (Pattern Validation)

#### Task Group 1: Refactor jj--get-project-name Tests
**Assigned implementer:** testing-engineer
**Dependencies:** None

- [x] 1.0 Complete jj--get-project-name refactoring
  - [x] 1.1 Convert jj--get-project-name test suite to data-driven format
    - Create plist-based test-cases table with 2 test cases
    - Keys: `:description`, `:project-folder`, `:expected`
    - Use explicit `dolist` loop to generate `it` blocks
    - Preserve existing test assertions exactly
    - Keep helper macro usage: `jj-test-with-project-folder`
    - Follow plist format from spec.md example
  - [x] 1.2 Simplify comments for jj--get-project-name suite
    - Remove verbose Given/When/Then comments from individual tests
    - Keep describe-level comment: "Test cases for project name extraction from various path formats"
    - Remove inline explanatory comments (data table is self-documenting)
  - [x] 1.3 Run tests to validate pattern
    - Execute: `eask run script test`
    - Verify all 2 tests in this suite pass
    - Confirm test execution time remains under 1 second
    - Verify test output shows clear descriptive names from `:description` keys

**Acceptance Criteria:**
- Test suite uses plist-based data table with dolist
- All 2 test cases pass with identical behavior
- Comments simplified (no Given/When/Then)
- Test output remains clear and readable
- Pattern validated as working correctly

### Phase 2: Parsing Functions (Core Refactoring)

#### Task Group 2: Refactor jj--bookmarks-get Tests
**Assigned implementer:** testing-engineer
**Dependencies:** Task Group 1

- [x] 2.0 Complete jj--bookmarks-get refactoring
  - [x] 2.1 Convert jj--bookmarks-get test suite to data-driven format
    - Create plist-based test-cases table with 3 test cases
    - Keys: `:description`, `:fixture`, `:output`, `:expected`
    - Use `:fixture` for "sample-bookmarks.txt" case
    - Use `:output` for inline string cases (empty and whitespace)
    - Implement conditional logic: load fixture if `:fixture` present, else use `:output`
    - Use explicit `dolist` loop to generate `it` blocks
    - Preserve helper macro usage: `jj-test-with-mocked-command`, `jj-test-with-project-folder`
    - Extract common cmd-string outside loop as it's constant across cases
  - [x] 2.2 Simplify comments for jj--bookmarks-get suite
    - Remove verbose Given/When/Then comments from individual tests
    - Keep describe-level comment: "Test cases for bookmark parsing with various command outputs"
    - Remove inline explanatory comments
  - [x] 2.3 Run tests after refactoring
    - Execute: `eask run script test`
    - Verify all 3 tests in this suite pass
    - Confirm no performance degradation

**Acceptance Criteria:**
- Test suite uses plist-based data table with dolist
- All 3 test cases pass with identical behavior
- Fixture and inline output handling both work correctly
- Comments simplified appropriately
- Common command string extracted to reduce duplication

#### Task Group 3: Refactor jj--log-count-revs Tests
**Assigned implementer:** testing-engineer
**Dependencies:** Task Group 2

- [x] 3.0 Complete jj--log-count-revs refactoring
  - [x] 3.1 Convert jj--log-count-revs test suite to data-driven format
    - Create plist-based test-cases table with 2 test cases
    - Keys: `:description`, `:revset`, `:output`, `:expected`
    - Use `:output` for inline strings ("aaa" and "")
    - Use `:revset` for the revset parameter ("trunk()..main")
    - Use explicit `dolist` loop to generate `it` blocks
    - Preserve helper macro usage: `jj-test-with-mocked-command`, `jj-test-with-project-folder`
    - Build cmd-string dynamically using revset from test case
  - [x] 3.2 Simplify comments for jj--log-count-revs suite
    - Remove verbose Given/When/Then comments from individual tests
    - Keep describe-level comment: "Test cases for revision counting from log output"
    - Remove inline explanatory comments
  - [x] 3.3 Run tests after refactoring
    - Execute: `eask run script test`
    - Verify all 2 tests in this suite pass
    - Confirm test execution remains fast

**Acceptance Criteria:**
- Test suite uses plist-based data table with dolist
- All 2 test cases pass with identical behavior
- Revset parameter correctly extracted from test cases
- Command string correctly built dynamically per test case
- Comments simplified appropriately

### Phase 3: Command Construction Tests

#### Task Group 4: Refactor jj--run-command Tests
**Assigned implementer:** testing-engineer
**Dependencies:** Task Group 3

- [x] 4.0 Complete jj--run-command refactoring
  - [x] 4.1 Convert jj--run-command test suite to data-driven format
    - Create plist-based test-cases table with 2 test cases
    - Keys: `:description`, `:command`, `:project-folder`, `:verify-type`, `:expected`
    - Use `:verify-type` to distinguish between 'command and 'directory verification
    - Use explicit `dolist` loop to generate `it` blocks
    - Both test cases capture command and directory in same structure
    - Use conditional assertion based on `:verify-type` value
    - Preserve `cl-letf` mocking pattern for `shell-command-to-string`
    - Preserve helper macro usage: `jj-test-with-project-folder`
  - [x] 4.2 Simplify comments for jj--run-command suite
    - Remove verbose Given/When/Then comments from individual tests
    - Keep describe-level comment: "Test cases for command construction and execution context"
    - Remove inline explanatory comments
  - [x] 4.3 Run full test suite
    - Execute: `eask run script test`
    - Verify all 9 tests across all 4 suites pass
    - Confirm total execution time under 1 second
    - Verify test output is clear and readable

**Acceptance Criteria:**
- Test suite uses plist-based data table with dolist
- All 2 test cases pass with identical behavior
- Verification type switching works correctly (command vs directory)
- Comments simplified appropriately
- All 9 tests across entire file pass

### Phase 4: Documentation and Cleanup

#### Task Group 5: Update Documentation and Final Validation
**Assigned implementer:** testing-engineer
**Dependencies:** Task Group 4

- [x] 5.0 Complete documentation updates and final validation
  - [x] 5.1 Update file header commentary in test-jj.el
    - Replace "Test Patterns" section with "Data-Driven Test Pattern"
    - Add example showing plist structure and dolist usage
    - Document standard plist keys: `:description`, `:expected`, `:fixture`, `:output`, etc.
    - Explain when to use fixtures vs inline data
    - Update "Adding New Tests" section to reference data-driven approach
    - Include example of adding new test case to existing table
  - [x] 5.2 Review and verify all comment simplifications
    - Ensure no verbose Given/When/Then comments remain
    - Confirm describe-level comments are present and helpful
    - Verify data table comments explain test case variations
    - Check that file header documentation is comprehensive
  - [x] 5.3 Final test suite validation
    - Execute: `eask run script test`
    - Verify all 9 tests pass (2 + 3 + 2 + 2)
    - Confirm execution time under 1 second
    - Verify test output shows clear, descriptive test names
    - Check that test behavior is identical to original
  - [x] 5.4 Code quality review
    - Verify DRY principle achieved (60-70% duplication reduction)
    - Confirm plist structure consistent across all suites
    - Check that all test suites follow same organizational pattern
    - Ensure code is maintainable and self-documenting

**Acceptance Criteria:**
- File header documentation updated with data-driven pattern examples
- Standard plist keys documented clearly
- All comments appropriately simplified
- All 9 tests pass with identical behavior
- No performance degradation (under 1 second execution)
- Code duplication reduced by 60-70%
- Pattern consistent across all four test suites
- Test output clear and readable

## Execution Order

Recommended implementation sequence:
1. **Phase 1:** Simple Test Suite - jj--get-project-name (Task Group 1)
2. **Phase 2:** Parsing Functions - jj--bookmarks-get and jj--log-count-revs (Task Groups 2-3)
3. **Phase 3:** Command Construction - jj--run-command (Task Group 4)
4. **Phase 4:** Documentation and Cleanup (Task Group 5)

## Important Notes

### Plist Structure Guidelines
- **Required keys:** `:description` (test name), `:expected` (expected result)
- **Optional keys:** Use as needed: `:fixture`, `:output`, `:project-folder`, `:command`, `:revset`, `:verify-type`
- **Access pattern:** Use `plist-get` to extract values
- **Naming:** Use keyword symbols (`:key-name`) for all keys

### Test Behavior Preservation
- All existing test assertions must pass identically
- No changes to helper macros in test-helper.el
- No changes to fixture files
- Same mocking patterns maintained
- No new test cases added (only restructuring)

### Comment Strategy
- **Remove:** Verbose Given/When/Then inline comments
- **Keep:** Describe-level suite documentation
- **Add:** Data table explanation comments
- **Update:** File header with data-driven pattern examples

### Validation Checkpoints
After each phase:
- Run: `eask run script test`
- Verify: All tests pass with same assertions
- Check: Test output is readable and descriptive
- Confirm: No performance degradation

### Standards Compliance
- **DRY Principle:** Eliminate code duplication by extracting test logic to single implementation per suite
- **Test Behavior:** Focus on inputs/outputs, maintain all existing assertions
- **Clear Test Names:** Descriptions in `:description` keys must be explicit and readable
- **Fast Execution:** Maintain existing mocking strategy, keep execution under 1 second
- **Emacs Lisp Idioms:** Use plist structure and plist-get for idiomatic code
