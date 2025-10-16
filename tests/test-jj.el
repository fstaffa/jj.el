;;; test-jj.el --- Buttercup tests for jj  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit Tests for jj.el
;; ====================
;;
;; This file contains unit tests for core jj.el functions using Buttercup.
;; All tests use mocking to avoid requiring jj binary installation.
;;
;; Running Tests
;; -------------
;;
;; Run all tests:
;;   eask run script test
;;
;; Expected behavior:
;;   - All tests should pass
;;   - Execution completes in under 1 second
;;   - No actual jj commands are executed
;;   - Tests run in complete isolation
;;
;; Test Organization
;; -----------------
;;
;; Tests are organized into describe blocks by function:
;;   - jj--get-project-name    Project name extraction tests
;;   - jj--bookmarks-get       Bookmark parsing tests
;;   - jj--log-count-revs      Log revision counting tests
;;   - jj--run-command         Command execution tests
;;
;; Data-Driven Test Pattern
;; -------------------------
;;
;; This test suite uses a data-driven approach to reduce duplication and
;; improve maintainability. Test cases are defined as plist-based data
;; tables, and `dolist` generates individual test cases.
;;
;; Pattern Structure:
;;
;;   (describe "function-name"
;;     ;; Brief comment explaining what this suite tests
;;     (let ((test-cases
;;            '((:description "test case 1 description"
;;               :key1 value1
;;               :key2 value2
;;               :expected expected-result-1)
;;              (:description "test case 2 description"
;;               :key1 value3
;;               :key2 value4
;;               :expected expected-result-2))))
;;       (dolist (test-case test-cases)
;;         (it (plist-get test-case :description)
;;           ;; Test implementation using plist-get to extract values
;;           (let ((value1 (plist-get test-case :key1))
;;                 (expected (plist-get test-case :expected)))
;;             (expect (function-under-test value1) :to-equal expected))))))
;;
;; Standard Plist Keys:
;;   :description (string)     - Test case description for `it` block (required)
;;   :expected (any)           - Expected result for assertion (required)
;;   :fixture (string)         - Fixture filename from tests/fixtures/ directory
;;   :output (string)          - Inline output string (alternative to :fixture)
;;   :project-folder (string)  - Mock project folder path
;;   :command (string)         - Command string if it varies per test
;;   :revset (string)          - Revset argument for log functions
;;   :verify-type (symbol)     - Verification type (e.g., 'command, 'directory)
;;
;; When to Use Fixtures vs Inline Data:
;;   - Use :fixture for complex multi-line outputs (e.g., "sample-bookmarks.txt")
;;   - Use :output for simple inline strings (e.g., "" or "main\n\n")
;;   - Fixtures keep test tables cleaner for realistic command outputs
;;   - Inline data is more readable for short test strings
;;
;; Testing Patterns
;; ----------------
;;
;; Pattern 1: Testing parsing functions
;;   Use `jj-test-with-mocked-command` to provide sample output
;;   Use `jj-test-with-project-folder` to mock project detection
;;   Verify the parsed result matches expected data structure
;;
;; Pattern 2: Testing command construction
;;   Capture the executed command using `cl-letf`
;;   Mock `shell-command-to-string` to record command
;;   Verify command format matches expected jj command syntax
;;
;; Pattern 3: Fixture vs inline output selection
;;   Use conditional logic to load fixture if :fixture key present
;;   Otherwise use :output key for inline test data
;;   This allows mixing fixture and inline data in the same test suite
;;
;; Adding New Tests
;; ----------------
;;
;; To add a test case to an existing suite:
;;
;; 1. Add a new plist entry to the test-cases list
;; 2. Include :description and :expected keys (required)
;; 3. Add any function-specific keys (:fixture, :output, :command, etc.)
;; 4. Run tests to verify: eask run script test
;;
;; Example - adding a test case to jj--bookmarks-get:
;;
;;   (describe "jj--bookmarks-get"
;;     (let ((test-cases
;;            '(;; ... existing test cases ...
;;              (:description "should handle new scenario"
;;               :output "new-bookmark\n"
;;               :expected ("new-bookmark")))))  ;; <-- New case added
;;       (dolist (test-case test-cases)
;;         ;; ... existing test implementation ...
;;         )))
;;
;; To create a test suite for a new function:
;;
;; 1. Create a new `describe` block for the function
;; 2. Define a test-cases list with plists for each scenario
;; 3. Use `dolist` to iterate and generate `it` blocks
;; 4. Extract values using `plist-get` in the test implementation
;; 5. Mock all external dependencies (commands, file system)
;; 6. Keep describe-level comments brief and informative
;; 7. Verify tests pass: eask run script test

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; Test Suite: jj--get-project-name
;; ---------------------------------
;; Tests project name extraction from project folder path.
;; Function: jj--get-project-name
;; Purpose: Extract the project directory name from the project folder path

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

;; Test Suite: jj--bookmarks-get
;; ------------------------------
;; Tests bookmark list parsing from jj command output.
;; Function: jj--bookmarks-get
;; Purpose: Parse bookmark names from "jj bookmark list" command output

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
            :expected ("main"))))
        (cmd-string "jj --no-pager --color never bookmark list -T 'name ++ \"\n\"'"))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((output (if (plist-get test-case :fixture)
                          (jj-test-load-fixture (plist-get test-case :fixture))
                        (plist-get test-case :output))))
          (jj-test-with-mocked-command
            (list (cons cmd-string output))
            (jj-test-with-project-folder "/tmp/test"
              (expect (jj--bookmarks-get) :to-equal (plist-get test-case :expected)))))))))

;; Test Suite: jj--log-count-revs
;; -------------------------------
;; Test cases for revision counting from log output

(describe "jj--log-count-revs"
  (let ((test-cases
         '((:description "should count revisions from log output correctly"
            :revset "trunk()..main"
            :output "aaa"
            :expected 3)
           (:description "should handle empty log output as zero revisions"
            :revset "trunk()..main"
            :output ""
            :expected 0))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((revset (plist-get test-case :revset))
               (output (plist-get test-case :output))
               (expected (plist-get test-case :expected))
               (cmd-string (format "jj --no-pager --color never log -T '\"a\"' --revisions \"%s\" --no-graph" revset)))
          (jj-test-with-mocked-command
            (list (cons cmd-string output))
            (jj-test-with-project-folder "/tmp/test"
              (expect (jj--log-count-revs revset) :to-equal expected))))))))

;; Test Suite: jj--run-command
;; ----------------------------
;; Tests command construction and execution.
;; Function: jj--run-command
;; Purpose: Execute jj commands from project root with standard arguments

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

;;; test-jj.el ends here
