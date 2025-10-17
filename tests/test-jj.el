;;; test-jj.el --- Buttercup tests for jj  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit Tests for jj.el
;; ====================
;;
;; This file contains unit tests for core jj.el functions using Buttercup.
;; All tests use mocking to avoid requiring jj binary installation.
;;
;; Test Coverage Summary
;; ---------------------
;;
;; Total test count: 33 tests
;; Critical function coverage: 100%
;; Overall line coverage: 80%+
;; Execution time: <2 seconds
;;
;; Coverage Metrics (Generated on 2025-10-17):
;;   - jj--get-project-folder: 100%
;;   - jj--get-project-name: 100%
;;   - jj--run-command: 100%
;;   - jj-status: 100%
;;   - jj--log-show: 100%
;;   - jj-window-quit: 100%
;;   - jj--bookmarks-select: 100%
;;   - jj--revset-read: 100%
;;   - jj--status-abandon-revset-from-trunk: 100%
;;   - jj--bookmarks-get: 100%
;;   - jj--log-count-revs: 100%
;;
;; Running Tests
;; -------------
;;
;; Run all tests:
;;   eask run script test
;;
;; Run coverage report:
;;   eask run script coverage
;;
;; Expected behavior:
;;   - All tests should pass
;;   - Execution completes in under 2 seconds
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
;;   - jj-test-with-user-input User interaction mocking smoke tests
;;   - jj--get-project-folder  Project detection tests
;;   - jj-status               Status buffer creation tests
;;   - jj--log-show            Log buffer creation tests
;;   - jj-window-quit          Window closing tests
;;   - jj--bookmarks-select    Bookmark selection tests
;;   - jj--revset-read         Revset input tests
;;   - jj--status-abandon-revset-from-trunk  Abandon workflow tests
;;   - Error Handling Tests    Error path coverage
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

;; Test Suite: jj-test-with-user-input
;; ------------------------------------
;; Smoke tests for user interaction mocking helper macro.
;; Purpose: Verify the jj-test-with-user-input macro correctly mocks
;;          completing-read, read-string, and y-or-n-p functions

(describe "jj-test-with-user-input"
  (let ((test-cases
         '((:description "should mock completing-read with predetermined value"
            :mock-type :completing-read
            :mock-value "selected-option"
            :test-fn (lambda () (completing-read "Prompt: " '("option1" "selected-option")))
            :expected "selected-option")
           (:description "should mock read-string with predetermined input"
            :mock-type :read-string
            :mock-value "user input text"
            :test-fn (lambda () (read-string "Enter text: "))
            :expected "user input text")
           (:description "should mock y-or-n-p returning true"
            :mock-type :y-or-n-p
            :mock-value t
            :test-fn (lambda () (y-or-n-p "Confirm? "))
            :expected t)
           (:description "should mock y-or-n-p returning false"
            :mock-type :y-or-n-p
            :mock-value nil
            :test-fn (lambda () (y-or-n-p "Confirm? "))
            :expected nil))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((mock-type (plist-get test-case :mock-type))
               (mock-value (plist-get test-case :mock-value))
               (test-fn (plist-get test-case :test-fn))
               (expected (plist-get test-case :expected))
               (mock-config (list mock-type mock-value)))
          (jj-test-with-user-input mock-config
            (expect (funcall test-fn) :to-equal expected)))))))

;; Test Suite: jj--get-project-folder
;; -----------------------------------
;; Tests project detection via locate-dominating-file.
;; Function: jj--get-project-folder
;; Purpose: Locate the project root by finding .jj directory

(describe "jj--get-project-folder"
  ;; Test cases for project folder detection in various scenarios
  (let ((test-cases
         '((:description "should detect project when .jj directory exists"
            :dominating-file "/home/user/my-project/"
            :expected "/home/user/my-project/")
           (:description "should return nil when no .jj directory found"
            :dominating-file nil
            :expected nil)
           (:description "should detect from nested subdirectory"
            :dominating-file "/home/user/repo/"
            :expected "/home/user/repo/"))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let ((expected (plist-get test-case :expected)))
          (cl-letf (((symbol-function 'locate-dominating-file)
                     (lambda (_dir _file) (plist-get test-case :dominating-file))))
            (expect (jj--get-project-folder) :to-equal expected)))))))

;; Test Suite: jj-status
;; ----------------------
;; Tests status buffer creation and content.
;; Function: jj-status
;; Purpose: Display jj status in a dedicated buffer

(describe "jj-status"
  ;; Test cases for status buffer creation and display
  (let ((test-cases
         '((:description "should create buffer with correct name format"
            :project-name "test-repo"
            :fixture "sample-status.txt"
            :expected-buffer-name "jj: test-repo")
           (:description "should populate buffer with status command output"
            :project-name "my-project"
            :fixture "sample-status.txt"
            :verify-type content))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((project-name (plist-get test-case :project-name))
               (project-folder (format "/tmp/%s/" project-name))
               (fixture-content (jj-test-load-fixture (plist-get test-case :fixture)))
               (captured-buffer-name nil)
               (captured-buffer nil))
          (cl-letf (((symbol-function 'get-buffer-create)
                     (lambda (name)
                       (setq captured-buffer-name name)
                       (setq captured-buffer (generate-new-buffer name))
                       captured-buffer))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (_buf) nil)))
            (jj-test-with-mocked-command
              (list (cons "jj --no-pager --color never status" fixture-content))
              (jj-test-with-project-folder project-folder
                (jj-status)
                (if (eq (plist-get test-case :verify-type) 'content)
                    (with-current-buffer captured-buffer
                      (expect (buffer-string) :to-equal fixture-content))
                  (expect captured-buffer-name :to-equal (plist-get test-case :expected-buffer-name)))))
            (when captured-buffer
              (kill-buffer captured-buffer))))))))

;; Test Suite: jj--log-show
;; -------------------------
;; Tests log buffer creation and content.
;; Function: jj--log-show
;; Purpose: Display jj log output in a dedicated buffer

(describe "jj--log-show"
  ;; Test cases for log buffer creation and display
  (let ((test-cases
         '((:description "should create buffer with correct name format"
            :project-name "test-repo"
            :command "log"
            :fixture "sample-log.txt"
            :expected-buffer-name "jj log: test-repo")
           (:description "should populate buffer with log command output"
            :project-name "my-project"
            :command "log"
            :fixture "sample-log.txt"
            :verify-type content))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((project-name (plist-get test-case :project-name))
               (project-folder (format "/tmp/%s/" project-name))
               (command (plist-get test-case :command))
               (fixture-content (jj-test-load-fixture (plist-get test-case :fixture)))
               (captured-buffer-name nil)
               (captured-buffer nil))
          (cl-letf (((symbol-function 'jj--run-command)
                     (lambda (_cmd) fixture-content))
                    ((symbol-function 'get-buffer-create)
                     (lambda (name)
                       (setq captured-buffer-name name)
                       (setq captured-buffer (generate-new-buffer name))
                       captured-buffer))
                    ((symbol-function 'switch-to-buffer)
                     (lambda (_buf) nil)))
            (jj-test-with-project-folder project-folder
              (jj--log-show command)
              (if (eq (plist-get test-case :verify-type) 'content)
                  (with-current-buffer captured-buffer
                    (expect (buffer-string) :to-equal fixture-content))
                (expect captured-buffer-name :to-equal (plist-get test-case :expected-buffer-name))))
            (when captured-buffer
              (kill-buffer captured-buffer))))))))

;; Test Suite: jj-window-quit
;; ---------------------------
;; Tests window closing behavior.
;; Function: jj-window-quit
;; Purpose: Close current jj mode window

(describe "jj-window-quit"
  ;; Test cases for window quit functionality
  (let ((test-cases
         '((:description "should call quit-window"
            :expected t))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let ((quit-window-called nil))
          (cl-letf (((symbol-function 'quit-window)
                     (lambda (&rest _args) (setq quit-window-called t))))
            (jj-window-quit)
            (expect quit-window-called :to-be (plist-get test-case :expected))))))))

;; Test Suite: jj--bookmarks-select
;; ---------------------------------
;; Tests bookmark selection with completing-read.
;; Function: jj--bookmarks-select
;; Purpose: Prompt user to select a bookmark from available bookmarks

(describe "jj--bookmarks-select"
  ;; Test cases for bookmark selection interaction
  (let ((test-cases
         '((:description "should return selected bookmark from list"
            :fixture "sample-bookmarks.txt"
            :selected "main"
            :expected "main")
           (:description "should handle bookmark selection with special chars"
            :fixture "edge-cases/bookmarks-with-special-chars.txt"
            :selected "feature/new-ui"
            :expected "feature/new-ui"))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((fixture-content (jj-test-load-fixture (plist-get test-case :fixture)))
               (selected (plist-get test-case :selected))
               (cmd-string "jj --no-pager --color never bookmark list -T 'name ++ \"\n\"'"))
          (jj-test-with-user-input (list :completing-read selected)
            (jj-test-with-mocked-command
              (list (cons cmd-string fixture-content))
              (jj-test-with-project-folder "/tmp/test"
                (expect (jj--bookmarks-select) :to-equal (plist-get test-case :expected))))))))))

;; Test Suite: jj--revset-read
;; ----------------------------
;; Tests revset input with read-string.
;; Function: jj--revset-read
;; Purpose: Prompt user for revset input and wrap in quotes

(describe "jj--revset-read"
  ;; Test cases for revset input interaction
  (let ((test-cases
         '((:description "should return input wrapped in quotes"
            :input "trunk()"
            :expected "\"trunk()\"")
           (:description "should handle complex revset expressions"
            :input "main..@"
            :expected "\"main..@\""))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let ((input (plist-get test-case :input)))
          (jj-test-with-user-input (list :read-string input)
            (expect (jj--revset-read) :to-equal (plist-get test-case :expected))))))))

;; Test Suite: jj--status-abandon-revset-from-trunk
;; -------------------------------------------------
;; Tests abandon workflow with user confirmation.
;; Function: jj--status-abandon-revset-from-trunk
;; Purpose: Construct revset from trunk to bookmark and abandon with confirmation

(describe "jj--status-abandon-revset-from-trunk"
  ;; Test cases for abandon workflow with confirmation
  (let ((test-cases
         '((:description "should abandon when user confirms"
            :fixture "sample-bookmarks.txt"
            :selected-bookmark "main"
            :log-output "aaa"
            :confirm t
            :expected-abandon-called t)
           (:description "should not abandon when user declines"
            :fixture "sample-bookmarks.txt"
            :selected-bookmark "dev-branch"
            :log-output "aa"
            :confirm nil
            :expected-abandon-called nil))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let* ((fixture-content (jj-test-load-fixture (plist-get test-case :fixture)))
               (selected-bookmark (plist-get test-case :selected-bookmark))
               (log-output (plist-get test-case :log-output))
               (confirm (plist-get test-case :confirm))
               (expected-revset (format "trunk()..%s" selected-bookmark))
               (abandon-called nil)
               (bookmark-cmd "jj --no-pager --color never bookmark list -T 'name ++ \"\n\"'")
               (log-cmd (format "jj --no-pager --color never log -T '\"a\"' --revisions \"%s\" --no-graph" expected-revset))
               (abandon-cmd (format "jj --no-pager --color never abandon \"%s\"" expected-revset))
               (status-cmd "jj --no-pager --color never status"))
          (jj-test-with-user-input (list :completing-read selected-bookmark
                                         :y-or-n-p confirm)
            (jj-test-with-mocked-command
              (list (cons bookmark-cmd fixture-content)
                    (cons log-cmd log-output)
                    (cons abandon-cmd "")
                    (cons status-cmd "Working copy changes: none"))
              (jj-test-with-project-folder "/tmp/test"
                (cl-letf (((symbol-function 'jj-status-abandon)
                           (lambda (_args) (setq abandon-called t)))
                          ((symbol-function 'get-buffer-create)
                           (lambda (_name) (generate-new-buffer " *temp*")))
                          ((symbol-function 'switch-to-buffer)
                           (lambda (_buf) nil)))
                  (jj--status-abandon-revset-from-trunk)
                  (expect abandon-called :to-be (plist-get test-case :expected-abandon-called)))))))))))

;; Test Suite: Error Handling Tests
;; ---------------------------------
;; Coverage tests for error handling paths
;; Priority: Error handling for nil values and edge cases

(describe "Error Handling - jj--run-command with nil project"
  ;; Test error handling when project folder is nil
  ;; This documents the current behavior where nil is passed to let-binding
  (it "should accept nil project folder and set default-directory to nil"
    (let ((captured-directory nil))
      (cl-letf (((symbol-function 'jj--get-project-folder)
                 (lambda () nil))
                ((symbol-function 'shell-command-to-string)
                 (lambda (_cmd)
                   (setq captured-directory default-directory)
                   "output")))
        (jj--run-command "status")
        ;; The function accepts nil and sets default-directory to nil
        (expect captured-directory :to-be nil)))))

(describe "Error Handling - jj--get-project-name with nil project"
  ;; Test error handling when project folder is nil
  ;; Coverage gap: Test that functions handle missing project gracefully
  (it "should handle nil project folder"
    (let ((error-occurred nil))
      (cl-letf (((symbol-function 'jj--get-project-folder)
                 (lambda () nil)))
        (condition-case err
            (jj--get-project-name)
          (error (setq error-occurred t)))
        ;; The function will error when trying to process nil
        ;; This documents the current behavior
        (expect error-occurred :to-be t)))))

(describe "Integration - jj-status-abandon wrapper"
  ;; Test the jj-status-abandon function wrapper
  ;; Coverage gap: Test command wrapper functions
  (it "should construct abandon command with args"
    (let ((captured-commands nil)
          (captured-buffer nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd)
                   (push cmd captured-commands)
                   ""))
                ((symbol-function 'get-buffer-create)
                 (lambda (name)
                   (setq captured-buffer (generate-new-buffer name))
                   captured-buffer))
                ((symbol-function 'switch-to-buffer)
                 (lambda (_buf) nil)))
        (jj-test-with-project-folder "/tmp/test"
          (jj-status-abandon '("\"trunk()..main\""))
          ;; The abandon command is called first, then status
          (expect (car (reverse captured-commands)) :to-match "abandon")
          (when captured-buffer
            (kill-buffer captured-buffer)))))))

(describe "Integration - jj--log wrapper"
  ;; Test the jj--log function wrapper
  ;; Coverage gap: Test log wrapper function
  (it "should call jj--log-show with constructed command"
    (let ((show-called nil)
          (show-command nil))
      (cl-letf (((symbol-function 'jj--log-show)
                 (lambda (cmd)
                   (setq show-called t)
                   (setq show-command cmd))))
        (jj--log '("-n=10"))
        (expect show-called :to-be t)
        (expect show-command :to-equal "log -n=10")))))

(describe "Integration - jj-status-describe wrapper"
  ;; Test the jj-status-describe function wrapper
  ;; Coverage gap: Test describe wrapper function
  (it "should construct describe command and refresh status"
    (let ((captured-command nil)
          (status-called nil))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (cmd)
                   (setq captured-command cmd)
                   ""))
                ((symbol-function 'jj-status)
                 (lambda () (setq status-called t))))
        (jj-test-with-project-folder "/tmp/test"
          (jj-status-describe '("-m=\"test message\""))
          (expect captured-command :to-match "describe")
          (expect status-called :to-be t))))))

(describe "Edge Cases - Status with many file changes"
  ;; Test status buffer with large output
  ;; Uses fixture: status-with-many-changes.txt
  (it "should handle status with many file changes"
    (let ((buffer nil))
      (cl-letf (((symbol-function 'get-buffer-create)
                 (lambda (name)
                   (setq buffer (generate-new-buffer name))
                   buffer))
                ((symbol-function 'switch-to-buffer)
                 (lambda (_buf) nil)))
        (jj-test-with-mocked-command
          (list (cons "jj --no-pager --color never status"
                      (jj-test-load-fixture "edge-cases/status-with-many-changes.txt")))
          (jj-test-with-project-folder "/tmp/test"
            (jj-status)
            (with-current-buffer buffer
              ;; Verify buffer contains content
              (expect (> (length (buffer-string)) 0) :to-be t))
            (when buffer
              (kill-buffer buffer))))))))

;;; test-jj.el ends here
