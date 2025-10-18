# Specification: Test Data-Driven Refactor

## Goal
Refactor the entire test suite in `tests/test-jj.el` to use a data-driven approach that eliminates code duplication, improves maintainability, and enhances test readability while preserving all existing test behavior and coverage.

## User Stories
- As a test maintainer, I want to add new test cases by simply adding entries to a data table, so that I can expand test coverage without duplicating test logic
- As a developer reading tests, I want to see all test cases for a function at a glance in a structured table, so that I can quickly understand what scenarios are being tested
- As a contributor, I want a consistent pattern for writing tests across the entire suite, so that I can write new tests following established conventions

## Core Requirements

### Functional Requirements
- Convert all four test suites to data-driven format: `jj--get-project-name`, `jj--bookmarks-get`, `jj--log-count-revs`, and `jj--run-command`
- Use plist-based data structures with keyword keys for test case definitions
- Generate multiple `it` blocks from data tables using explicit `dolist` loops
- Maintain Buttercup's `describe/it` block structure for test output readability
- Preserve all existing test assertions and expected behavior
- Continue using existing fixture files for command output data
- Keep using existing test helper macros (`jj-test-with-mocked-command`, `jj-test-with-project-folder`)

### Non-Functional Requirements
- All existing tests must pass after refactoring with identical assertions
- Test execution time should remain under 1 second
- Reduced code duplication following DRY principle from coding standards
- Self-documenting test cases through structured data tables
- No performance degradation in test suite execution

## Visual Design
Not applicable - this is a backend testing refactoring.

## Reusable Components

### Existing Code to Leverage
- **Test Helpers**: All existing helper macros from `test-helper.el`
  - `jj-test-with-mocked-command` for command mocking
  - `jj-test-with-project-folder` for project detection mocking
  - `jj-test-load-fixture` for loading fixture data
- **Fixture Files**: Continue using `tests/fixtures/` directory
  - `sample-bookmarks.txt` - Multi-line bookmark output
  - `empty-bookmarks.txt` - Empty bookmark list
  - `sample-log.txt` - Log command output
  - `sample-status.txt` - Status command output
- **Test Framework**: Buttercup's `describe`, `it`, and `expect` functions

### New Components Required
None - this refactoring reuses all existing test infrastructure and only reorganizes test structure.

## Technical Approach

### Data Structure Format
Use **plist (property list)** format for test case definitions:

```elisp
(let ((test-cases
       '((:description "should extract project name from path"
          :project-folder "/home/user/projects/my-repo/"
          :expected "my-repo")
         (:description "should handle path with trailing slash"
          :project-folder "/home/user/projects/test-project/"
          :expected "test-project"))))
  (dolist (test-case test-cases)
    (it (plist-get test-case :description)
      ;; test implementation
      )))
```

**Rationale for plists:**
- Property lists are idiomatic in Emacs Lisp
- Keywords (`:description`, `:expected`) provide self-documentation
- `plist-get` offers clean, readable access syntax
- Easy to extend with additional fields per test case
- Familiar to Emacs Lisp developers

### Standard Plist Keys Convention

**Required Keys:**
- `:description` (string) - Test case description used in `it` block name
- `:expected` (any) - Expected result for assertion

**Optional Keys (use as needed per test suite):**
- `:fixture` (string) - Fixture filename in `tests/fixtures/` directory
- `:output` (string) - Inline output string (alternative to fixture)
- `:project-folder` (string) - Mock project folder path
- `:command` (string) - Command string if it varies per test case
- `:revset` (string) - Revset argument for log functions

### Test Organization Pattern

Each `describe` block follows this structure:

```elisp
(describe "function-name"
  ;; Brief comment about what this suite tests
  (let ((test-cases
         '((:description "test case 1 description"
            :key1 value1
            :key2 value2
            :expected expected-result-1)
           (:description "test case 2 description"
            :key1 value3
            :key2 value4
            :expected expected-result-2))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        ;; Extract values using plist-get
        (let* ((key1 (plist-get test-case :key1))
               (key2 (plist-get test-case :key2))
               (expected (plist-get test-case :expected)))
          ;; Execute test logic with extracted values
          ;; Make assertion
          (expect (function-under-test key1 key2) :to-equal expected))))))
```

### Before/After Examples

#### Example 1: jj--get-project-name (Simple Case)

**Before:**
```elisp
(describe "jj--get-project-name"
  (it "should extract project name from path"
    ;; Tests basic project name extraction
    ;; Given: Project folder is /home/user/projects/my-repo/
    ;; When: jj--get-project-name is called
    ;; Then: Should return "my-repo"
    (jj-test-with-project-folder "/home/user/projects/my-repo/"
      (expect (jj--get-project-name) :to-equal "my-repo")))

  (it "should handle path with trailing slash"
    ;; Tests that trailing slash is handled correctly
    ;; Given: Project folder has trailing slash
    ;; When: jj--get-project-name is called
    ;; Then: Should still extract correct project name
    (jj-test-with-project-folder "/home/user/projects/test-project/"
      (expect (jj--get-project-name) :to-equal "test-project"))))
```

**After:**
```elisp
(describe "jj--get-project-name"
  ;; Test cases for project name extraction from various path formats
  (let ((test-cases
         '((:description "should extract project name from path"
            :project-folder "/home/user/projects/my-repo/"
            :expected "my-repo")
           (:description "should handle path with trailing slash"
            :project-folder "/home/user/projects/test-project/"
            :expected "test-project"))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (jj-test-with-project-folder (plist-get test-case :project-folder)
          (expect (jj--get-project-name) :to-equal (plist-get test-case :expected)))))))
```

#### Example 2: jj--bookmarks-get (With Fixtures)

**Before:**
```elisp
(describe "jj--bookmarks-get"
  (it "should parse multiple bookmarks from output"
    ;; Tests parsing multiple bookmark names from fixture
    ;; Given: jj bookmark list returns multiple bookmarks
    ;; When: jj--bookmarks-get is called
    ;; Then: Should return list of bookmark names
    (let ((bookmark-output (jj-test-load-fixture "sample-bookmarks.txt"))
          (cmd-string "jj --no-pager --color never bookmark list -T 'name ++ \"\n\"'"))
      (jj-test-with-mocked-command
        (list (cons cmd-string bookmark-output))
        (jj-test-with-project-folder "/tmp/test"
          (let ((bookmarks (jj--bookmarks-get)))
            (expect bookmarks :to-equal '("dev-branch" "feature-branch" "main")))))))

  (it "should handle empty bookmark list"
    ;; Tests parsing when no bookmarks exist
    ;; Given: jj bookmark list returns empty output
    ;; When: jj--bookmarks-get is called
    ;; Then: Should return empty list
    (let ((cmd-string "jj --no-pager --color never bookmark list -T 'name ++ \"\n\"'"))
      (jj-test-with-mocked-command
        (list (cons cmd-string ""))
        (jj-test-with-project-folder "/tmp/test"
          (let ((bookmarks (jj--bookmarks-get)))
            (expect bookmarks :to-equal nil))))))

  (it "should handle bookmark output with whitespace"
    ;; Tests parsing handles extra newlines/whitespace
    ;; Given: jj bookmark list output contains extra whitespace
    ;; When: jj--bookmarks-get is called
    ;; Then: Should filter empty strings and return clean list
    (let ((cmd-string "jj --no-pager --color never bookmark list -T 'name ++ \"\n\"'"))
      (jj-test-with-mocked-command
        (list (cons cmd-string "main\n\n"))
        (jj-test-with-project-folder "/tmp/test"
          (let ((bookmarks (jj--bookmarks-get)))
            (expect bookmarks :to-equal '("main"))))))))
```

**After:**
```elisp
(describe "jj--bookmarks-get"
  ;; Test cases for bookmark parsing with various command outputs
  (let ((test-cases
         '((:description "should parse multiple bookmarks from output"
            :fixture "sample-bookmarks.txt"
            :expected ("dev-branch" "feature-branch" "main"))
           (:description "should handle empty bookmark list"
            :output ""
            :expected nil)
           (:description "should handle bookmark output with whitespace"
            :output "main\n\n"
            :expected ("main")))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((cmd-string "jj --no-pager --color never bookmark list -T 'name ++ \"\n\"'")
               (output (if (plist-get test-case :fixture)
                          (jj-test-load-fixture (plist-get test-case :fixture))
                        (plist-get test-case :output))))
          (jj-test-with-mocked-command
            (list (cons cmd-string output))
            (jj-test-with-project-folder "/tmp/test"
              (expect (jj--bookmarks-get) :to-equal (plist-get test-case :expected)))))))))
```

#### Example 3: jj--run-command (Command Construction)

**Before:**
```elisp
(describe "jj--run-command"
  (it "should construct command with proper arguments"
    ;; Tests that jj--run-command builds correct command string
    ;; Given: A simple command like "status"
    ;; When: jj--run-command is called
    ;; Then: Should construct "jj --no-pager --color never status"
    (let ((captured-command nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd)
                   (setq captured-command cmd)
                   "test output")))
        (jj-test-with-project-folder "/tmp/test"
          (jj--run-command "status")
          (expect captured-command :to-equal "jj --no-pager --color never status")))))

  (it "should execute command from project folder"
    ;; Tests that commands execute from correct directory
    ;; Given: Project folder is set to specific directory
    ;; When: jj--run-command is called
    ;; Then: default-directory should be set to project folder
    (let ((captured-directory nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (_cmd)
                   (setq captured-directory default-directory)
                   "test output")))
        (jj-test-with-project-folder "/tmp/test-project/"
          (jj--run-command "status")
          (expect captured-directory :to-equal "/tmp/test-project/"))))))
```

**After:**
```elisp
(describe "jj--run-command"
  ;; Test cases for command construction and execution context
  (let ((test-cases
         '((:description "should construct command with proper arguments"
            :command "status"
            :project-folder "/tmp/test"
            :verify-type command
            :expected "jj --no-pager --color never status")
           (:description "should execute command from project folder"
            :command "status"
            :project-folder "/tmp/test-project/"
            :verify-type directory
            :expected "/tmp/test-project/"))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let ((captured-command nil)
              (captured-directory nil))
          (cl-letf (((symbol-function 'shell-command-to-string)
                     (lambda (cmd)
                       (setq captured-command cmd)
                       (setq captured-directory default-directory)
                       "test output")))
            (jj-test-with-project-folder (plist-get test-case :project-folder)
              (jj--run-command (plist-get test-case :command))
              (if (eq (plist-get test-case :verify-type) 'command)
                  (expect captured-command :to-equal (plist-get test-case :expected))
                (expect captured-directory :to-equal (plist-get test-case :expected))))))))))
```

### Comment Simplification Strategy

**Remove:**
- Verbose Given/When/Then style comments from individual test cases
- Inline explanatory comments that duplicate test descriptions
- Boilerplate comments explaining common patterns

**Keep:**
- File header commentary explaining overall test organization
- Brief describe-level comments explaining the function being tested
- Data table comments explaining the test case structure
- Example of data-driven pattern in file header

**Add:**
- Updated file header documentation showing data-driven pattern
- Brief comment above each test-cases list explaining what variations are tested

### Migration Strategy

Execute refactoring in this order:

**Phase 1: Simple Test Suite (Validate Pattern)**
1. Refactor `jj--get-project-name` (2 test cases, simplest structure)
2. Run tests to validate pattern works: `eask run script test`
3. Verify test output is clear and readable

**Phase 2: Parsing Functions (Core Refactoring)**
4. Refactor `jj--bookmarks-get` (3 test cases, uses fixtures and inline data)
5. Refactor `jj--log-count-revs` (2 test cases, similar to bookmarks)
6. Run tests after each refactoring

**Phase 3: Command Construction Tests**
7. Refactor `jj--run-command` (2 test cases, different verification approach)
8. Run full test suite

**Phase 4: Documentation and Cleanup**
9. Update file header commentary with data-driven pattern examples
10. Simplify comments throughout
11. Final test run to ensure all tests pass

**Validation After Each Phase:**
- Run: `eask run script test`
- Verify: All tests pass with same assertions
- Check: Test output is readable and descriptive
- Confirm: No performance degradation

## Out of Scope

### Not Included in This Refactor
- Changes to `test-helper.el` utilities or helper macros
- Creation of custom test macros like `it-with-data`
- Moving test data to separate files outside `test-jj.el`
- Modifications to fixture file contents in `tests/fixtures/`
- Changes to the source code being tested (`jj.el`)
- Adding new test cases beyond existing coverage
- Refactoring other test files (if they exist)
- Changing test framework from Buttercup
- Modifications to Eask test runner configuration

### Explicitly NOT Doing
- Custom macro creation - keeping explicit `dolist` loops for simplicity
- Inline fixture data - continuing to use existing fixture files
- Test behavior changes - preserving exact same assertions
- Test helper modifications - no changes to mocking infrastructure
- New test coverage - only restructuring existing tests

## Success Criteria

### Functional Success
- All 9 existing test cases pass after refactoring
- All test assertions remain identical in behavior
- Test execution completes in under 1 second (no performance regression)
- `eask run script test` command succeeds with 100% pass rate

### Code Quality Success
- Test code duplication reduced by approximately 60-70%
- Each test suite's logic defined once with data cases in tables
- Plist-based data structures used consistently across all test suites
- Clear, self-documenting test case descriptions in data tables

### Maintainability Success
- New test cases can be added by simply adding plist entries to data tables
- Test logic changes require modification in only one place per describe block
- Pattern is consistent across all four test suites
- File header documentation clearly explains data-driven pattern with examples

### Readability Success
- Test case variations visible at a glance in structured data tables
- Comments simplified with focus on describe-level documentation
- Test descriptions in plists are clear and descriptive
- Buttercup test output remains clear with descriptive test names
