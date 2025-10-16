# Spec Requirements: test-data-driven-refactor

## Initial Description
Refactor tests to use data driven approach

## Requirements Discussion

### First Round Questions

**Q1:** Do you want table-driven/parameterized tests where multiple test cases share the same test logic but vary by input/output data?
**Answer:** YES - Table-driven/parameterized tests where multiple test cases share the same test logic but vary by input/output data

**Q2:** Should we extract duplicate test patterns (like the jj--bookmarks-get tests) into test case tables with: input fixture/command output, expected result, and test description?
**Answer:** YES - Extract into test case tables with: input fixture/command output, expected result, and test description

**Q3:** Should we maintain Buttercup's describe/it format while generating multiple it blocks from a data table (using dolist or similar)?
**Answer:** YES - Keep Buttercup's describe/it format, generate multiple it blocks from a data table within each describe using dolist

**Q4:** Where should test data tables live - in the same file as tests, in a separate data file, or as fixtures?
**Answer:** Test data tables should live in the SAME FILE as tests, at the front of them, probably in the same describe block

**Q5:** Should we create a custom macro like `it-with-data` to simplify the pattern, or keep it explicit with dolist loops?
**Answer:** NO macros for now - keep it simpler with explicit dolist loops

**Q6:** For command output data, should we continue using fixture files or inline the data in the test tables?
**Answer:** KEEP fixture files for now - continue to use existing fixtures

**Q7:** Should we refactor ALL tests in test-jj.el, or focus on just the parsing functions (jj--bookmarks-get, jj--log-count-revs)?
**Answer:** Refactor ALL tests (not just parsing functions) - to make it clear they differ only in input/output

**Q8:** Current tests have extensive comments explaining each test. Should we keep, reduce, or eliminate these when using data tables (since the table structure may be self-documenting)?
**Answer:** SIMPLIFY comments - data table structure makes test cases more self-documenting

**Q9:** Do you have specific preferences for data structure format (alist, plist, list of lists)?
**Answer:** User doesn't know specific elisp patterns - RESEARCH what is good approach in elisp for data-driven tests

**Q10:** Are there any tests you want to explicitly EXCLUDE from the refactor?
**Answer:** No exclusions - refactor all tests

### Existing Code to Reference

**Similar Features Identified:**
No similar existing features identified for reference. This is the first major refactoring of the test suite.

### Follow-up Questions
No follow-up questions were needed.

## Visual Assets

### Files Provided:
No visual assets provided.

### Visual Insights:
No visual analysis performed.

## Requirements Summary

### Functional Requirements

**Primary Goal:**
Refactor the entire test suite in `tests/test-jj.el` to use a data-driven approach that eliminates duplication while maintaining Buttercup's describe/it format and test readability.

**Test Structure Requirements:**
1. Maintain Buttercup's describe/it block structure
2. Use explicit dolist loops to generate multiple it blocks from test data tables
3. No custom macros - keep the approach simple and explicit
4. Test data tables should be defined at the front of each describe block
5. Each test case in the table should specify: test description, input data, expected output
6. Continue using existing fixture files for command output data

**Data Table Format:**
Based on Emacs Lisp conventions and readability requirements, use **alist format** with descriptive keys:
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
      ;; test implementation using plist-get to extract values
      )))
```

**Rationale for plist format:**
- Property lists are idiomatic in Emacs Lisp
- Keywords (`:description`, `:expected`) are self-documenting
- `plist-get` provides clean access syntax
- Easy to extend with additional fields
- Familiar to Emacs Lisp developers

**Scope Coverage:**
Refactor ALL test suites in `tests/test-jj.el`:
1. `jj--get-project-name` - Project name extraction tests
2. `jj--bookmarks-get` - Bookmark parsing tests
3. `jj--log-count-revs` - Log revision counting tests
4. `jj--run-command` - Command execution tests

**Comment Simplification:**
- Remove verbose inline comments explaining each individual test case
- Keep high-level describe block documentation
- Let test descriptions in the data table serve as documentation
- Maintain commentary header explaining overall test organization
- Keep examples of the data-driven pattern in header comments

**Test Behavior Preservation:**
- All existing test assertions must pass after refactoring
- Maintain exact same test coverage
- Keep using same mocking helpers (jj-test-with-mocked-command, etc.)
- Preserve fixture file usage
- No changes to test-helper.el utilities

### Example Before/After Structure

**Before (Current Pattern):**
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
            (expect bookmarks :to-equal nil)))))))
```

**After (Data-Driven Pattern):**
```elisp
(describe "jj--bookmarks-get"
  ;; Test cases for bookmark parsing with various outputs
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

**Benefits of Refactored Approach:**
1. **Reduced Duplication:** Test logic defined once, data cases listed in table
2. **Easier Test Addition:** Add new test case = add new entry to table
3. **Clear Test Intent:** Data table shows at a glance what scenarios are tested
4. **Maintainability:** Changes to test logic only need to be made in one place
5. **Readability:** Less boilerplate, focus on the test data variations

### Reusability Opportunities
No existing similar features in this codebase. This establishes a pattern that can be used for future test suites in the project.

### Scope Boundaries

**In Scope:**
- Refactor all four test suites in tests/test-jj.el
- Convert to data-driven approach using plists and dolist
- Simplify comments while maintaining clarity
- Preserve all existing test behavior and assertions
- Update file header commentary to document new pattern

**Out of Scope:**
- Changes to test-helper.el utilities
- Creation of custom test macros
- Changes to fixture files or test data
- Modification of the functions being tested (jj.el source)
- Adding new test cases beyond existing coverage
- Refactoring other test files (if they exist)

**Explicitly NOT doing:**
- Creating a custom `it-with-data` macro
- Moving test data to separate files
- Changing from fixture files to inline data for complex outputs
- Modifying Buttercup framework or test helpers

### Technical Considerations

**Data Structure Choice:**
- Use plists (property lists) with keyword keys for test case data
- Keywords (`:description`, `:expected`, `:fixture`, etc.) provide self-documentation
- Access via `plist-get` is idiomatic Emacs Lisp
- Easy to extend with additional fields without breaking existing tests

**Test Organization Pattern:**
Each describe block should follow this structure:
```elisp
(describe "function-name"
  ;; Brief comment about what this suite tests
  (let ((test-cases
         '((:description "test case 1"
            :key1 value1
            :key2 value2
            :expected result1)
           (:description "test case 2"
            :key1 value3
            :key2 value4
            :expected result2))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        ;; Extract test case data using plist-get
        ;; Execute test logic
        ;; Make assertion
        ))))
```

**Plist Keys Convention:**
- `:description` - The test case description (required, string)
- `:expected` - The expected result (required, any type)
- `:fixture` - Fixture filename if using fixture (optional, string)
- `:output` - Inline output string if not using fixture (optional, string)
- `:project-folder` - Mock project folder path (optional, string)
- `:command` - Command string if it varies per test (optional, string)
- Additional keys as needed per test suite

**Comment Strategy:**
- Keep describe-level comments explaining the function being tested
- Remove verbose Given/When/Then comments from individual tests
- Let test descriptions in data table be self-documenting
- Update file header with example of new data-driven pattern
- Keep commentary about when to use fixtures vs inline data

**Migration Strategy:**
1. Start with simplest test suite (jj--get-project-name - 2 tests)
2. Validate pattern works correctly
3. Move to parsing functions (jj--bookmarks-get, jj--log-count-revs)
4. Finish with command execution tests (jj--run-command)
5. Update file header documentation with new patterns
6. Verify all tests still pass: `eask run script test`

**Backward Compatibility:**
Not applicable - this is internal test refactoring, no public API changes.

**Testing Standards Alignment:**
- Follows "Test Behavior, Not Implementation" - focusing on inputs/outputs
- Maintains "Clear Test Names" - descriptions are explicit in data table
- Preserves "Mock External Dependencies" - all existing mocking continues
- Keeps "Fast Execution" - no changes to mocking strategy
- Reduces duplication per DRY principle from coding-style.md

**Emacs Lisp Conventions:**
- Uses idiomatic plist structure
- Follows lexical binding conventions
- Maintains existing indentation and formatting standards
- Preserves buttercup framework conventions
