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
;; Test Patterns
;; -------------
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
;; Pattern 3: Using fixture data
;;   Load realistic jj output using `jj-test-load-fixture`
;;   Use fixtures for complex multi-line outputs
;;   Keep inline strings for simple single-line outputs
;;
;; Adding New Tests
;; ----------------
;;
;; When adding tests for new functions:
;;
;; 1. Create a new `describe` block for the function
;; 2. Add tests that cover the main behavior
;; 3. Use descriptive test names: "should [behavior] when [condition]"
;; 4. Mock all external dependencies (commands, file system)
;; 5. Keep tests focused and independent
;; 6. Verify tests pass: eask run script test
;;
;; Example test structure:
;;
;;   (describe "jj--new-function"
;;     (it "should parse output correctly"
;;       (let ((cmd "jj --no-pager --color never new-command"))
;;         (jj-test-with-mocked-command
;;           (list (cons cmd "sample output"))
;;           (jj-test-with-project-folder "/tmp/test"
;;             (expect (jj--new-function) :to-equal "expected result"))))))

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

;; Test Suite: jj--bookmarks-get
;; ------------------------------
;; Tests bookmark list parsing from jj command output.
;; Function: jj--bookmarks-get
;; Purpose: Parse bookmark names from "jj bookmark list" command output

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

;; Test Suite: jj--log-count-revs
;; -------------------------------
;; Tests revision counting from jj log output.
;; Function: jj--log-count-revs
;; Purpose: Count number of revisions in a given revset by counting characters

(describe "jj--log-count-revs"
  (it "should count revisions from log output correctly"
    ;; Tests counting revisions from log output
    ;; Given: jj log returns "aaa" (3 revisions with template "a")
    ;; When: jj--log-count-revs is called with a revset
    ;; Then: Should return 3 (length of output)
    (let ((cmd-string "jj --no-pager --color never log -T '\"a\"' --revisions \"trunk()..main\" --no-graph"))
      (jj-test-with-mocked-command
        (list (cons cmd-string "aaa"))
        (jj-test-with-project-folder "/tmp/test"
          (expect (jj--log-count-revs "trunk()..main") :to-equal 3)))))

  (it "should handle empty log output as zero revisions"
    ;; Tests counting when revset is empty
    ;; Given: jj log returns empty string (no revisions)
    ;; When: jj--log-count-revs is called
    ;; Then: Should return 0
    (let ((cmd-string "jj --no-pager --color never log -T '\"a\"' --revisions \"trunk()..main\" --no-graph"))
      (jj-test-with-mocked-command
        (list (cons cmd-string ""))
        (jj-test-with-project-folder "/tmp/test"
          (expect (jj--log-count-revs "trunk()..main") :to-equal 0))))))

;; Test Suite: jj--run-command
;; ----------------------------
;; Tests command construction and execution.
;; Function: jj--run-command
;; Purpose: Execute jj commands from project root with standard arguments

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

;;; test-jj.el ends here
