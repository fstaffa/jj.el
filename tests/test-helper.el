;;; test-helper.el --- Test utilities for jj.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Test Helper Utilities for jj.el
;; ================================
;;
;; This file provides test helper utilities for jj.el testing using Buttercup.
;; Tests run in complete isolation without requiring jj binary installation.
;;
;; Mocking Strategy
;; ----------------
;;
;; All external command execution is mocked using `cl-letf` to override
;; `shell-command-to-string`. This ensures:
;;   - Tests run without jj binary installed
;;   - Tests are fast (no actual process execution)
;;   - Tests are reproducible (deterministic outputs)
;;   - Tests run in complete isolation
;;
;; Available Test Helpers
;; -----------------------
;;
;; 1. `jj-test-with-mocked-command` - Mock jj command execution
;; 2. `jj-test-with-project-folder` - Mock project folder detection
;; 3. `jj-test-with-user-input` - Mock user interaction functions
;; 4. `jj-test-load-fixture` - Load sample jj output from fixture files
;; 5. `jj-test-setup-temp-dir` - Create isolated temporary directories
;; 6. `jj-test-cleanup-temp-dir` - Clean up temporary directories
;;
;; Quick Start Examples
;; --------------------
;;
;; Example 1: Test a function that runs a jj command
;;
;;   (describe "jj--bookmarks-get"
;;     (it "should parse bookmark list"
;;       (jj-test-with-mocked-command
;;         '(("jj --no-pager --color never bookmark list -T 'name ++ \"\\n\"'"
;;            . "main\ndev-branch\n"))
;;         (jj-test-with-project-folder "/tmp/test"
;;           (expect (jj--bookmarks-get) :to-equal '("main" "dev-branch"))))))
;;
;; Example 2: Test with fixture data
;;
;;   (describe "jj--bookmarks-get"
;;     (it "should parse bookmarks from fixture"
;;       (let ((fixture (jj-test-load-fixture "sample-bookmarks.txt"))
;;             (cmd "jj --no-pager --color never bookmark list -T 'name ++ \"\\n\"'"))
;;         (jj-test-with-mocked-command
;;           (list (cons cmd fixture))
;;           (jj-test-with-project-folder "/tmp/test"
;;             (expect (jj--bookmarks-get) :to-equal '("dev-branch" "feature-branch" "main")))))))
;;
;; Example 3: Test command construction
;;
;;   (describe "jj--run-command"
;;     (it "should construct correct command"
;;       (let ((captured-command nil))
;;         (cl-letf (((symbol-function 'shell-command-to-string)
;;                    (lambda (cmd)
;;                      (setq captured-command cmd)
;;                      "output")))
;;           (jj-test-with-project-folder "/tmp/test"
;;             (jj--run-command "status")
;;             (expect captured-command :to-equal "jj --no-pager --color never status"))))))
;;
;; Example 4: Test user interaction with completing-read
;;
;;   (describe "jj--bookmarks-select"
;;     (it "should return selected bookmark"
;;       (jj-test-with-user-input (:completing-read "main")
;;         (expect (completing-read "Select: " '("main" "dev")) :to-equal "main"))))
;;
;; Example 5: Test user interaction with read-string
;;
;;   (describe "jj--revset-read"
;;     (it "should return user input"
;;       (jj-test-with-user-input (:read-string "trunk()")
;;         (expect (read-string "Revset: ") :to-equal "trunk()"))))
;;
;; Example 6: Test user confirmation with y-or-n-p
;;
;;   (describe "jj--status-abandon"
;;     (it "should proceed when user confirms"
;;       (jj-test-with-user-input (:y-or-n-p t)
;;         (expect (y-or-n-p "Abandon?") :to-be t))))
;;
;; Writing New Tests
;; -----------------
;;
;; When adding tests for new jj.el functions:
;;
;; 1. Use `describe` blocks to group related tests
;; 2. Use descriptive test names: "should [expected behavior] when [condition]"
;; 3. Mock all external dependencies using helper macros
;; 4. Use fixture files for complex jj command outputs
;; 5. Keep tests focused on one behavior per test
;; 6. Ensure tests run in isolation without side effects
;;
;; Running Tests
;; -------------
;;
;; Run all tests:
;;   eask run script test
;;
;; Expected output:
;;   - All tests should pass
;;   - Execution time should be under 1 second
;;   - No actual jj commands should be executed
;;
;; Available Fixtures
;; ------------------
;;
;; Fixture files are organized in the tests/fixtures/ directory with
;; subdirectories for different categories:
;;
;; Main fixtures (tests/fixtures/):
;;   - sample-bookmarks.txt    Sample bookmark list output
;;   - empty-bookmarks.txt     Empty bookmark list
;;   - sample-log.txt          Sample log output
;;   - sample-status.txt       Sample status output
;;
;; Error scenario fixtures (tests/fixtures/errors/):
;;   - invalid-project-no-jj.txt     Error when .jj directory missing
;;   - command-not-found.txt         Shell error when jj binary not found
;;   - malformed-bookmark-output.txt Corrupted/invalid jj output
;;   - empty-log-output.txt          Empty log result (no revisions)
;;
;; Edge case fixtures (tests/fixtures/edge-cases/):
;;   - bookmarks-with-special-chars.txt  Bookmarks with spaces, hyphens, slashes
;;   - very-long-bookmark-name.txt       Single bookmark with 100+ char name
;;   - status-with-many-changes.txt      Status output with 20+ file changes
;;   - log-single-revision.txt           Log output with exactly one revision
;;
;; Loading fixtures from subdirectories:
;;   Use relative paths like "errors/command-not-found.txt" or
;;   "edge-cases/bookmarks-with-special-chars.txt" with jj-test-load-fixture

;;; Code:

(require 'buttercup)
(require 'cl-lib)

(defvar jj-test--temp-dir nil
  "Temporary directory path for current test.")

(defvar jj-test--fixtures-dir
  (expand-file-name "fixtures/"
                    (file-name-directory
                     (or load-file-name buffer-file-name)))
  "Absolute path to test fixtures directory.")

(defmacro jj-test-with-mocked-command (command-to-output &rest body)
  "Execute BODY with mocked shell commands.

COMMAND-TO-OUTPUT is an alist mapping command strings to their outputs.
Each entry should be a cons cell: (COMMAND-STRING . OUTPUT-STRING)

This macro mocks `shell-command-to-string' to return predefined outputs
instead of executing actual shell commands. Uses exact string matching.

Example:
  (jj-test-with-mocked-command
    \\='((\"jj --no-pager --color never status\" . \"Working copy changes:\"))
    (expect (jj--run-command \"status\") :to-equal \"Working copy changes:\"))

The macro will signal an error if an unexpected command is executed,
helping catch issues where commands don't match expected format.

Usage Notes:
  - Command strings must match exactly (use string-equal)
  - Multiple commands can be mocked by providing multiple alist entries
  - Combine with `jj-test-with-project-folder' to fully isolate tests
  - Unexpected commands will signal an error with the command string"
  (declare (indent 1))
  `(cl-letf (((symbol-function 'shell-command-to-string)
              (lambda (command)
                (let ((result (cl-assoc command ,command-to-output :test #'string-equal)))
                  (if result
                      (cdr result)
                    (error "Unexpected command: %s" command))))))
     ,@body))

(defmacro jj-test-with-user-input (mock-config &rest body)
  "Execute BODY with mocked user interaction functions.

MOCK-CONFIG is a plist specifying mocked return values for user input
functions. Supported keys:
  :completing-read VALUE  - Mock completing-read to return VALUE
  :read-string VALUE      - Mock read-string to return VALUE
  :y-or-n-p BOOLEAN       - Mock y-or-n-p to return BOOLEAN

Multiple interaction types can be mocked simultaneously by providing
multiple keys in the plist.

Examples:
  Mock completing-read:
    (jj-test-with-user-input (:completing-read \"main\")
      (expect (completing-read \"Select: \" \\='(\"main\" \"dev\"))
              :to-equal \"main\"))

  Mock read-string:
    (jj-test-with-user-input (:read-string \"trunk()\")
      (expect (read-string \"Revset: \") :to-equal \"trunk()\"))

  Mock y-or-n-p:
    (jj-test-with-user-input (:y-or-n-p t)
      (expect (y-or-n-p \"Proceed?\") :to-be t))

  Mock multiple interactions:
    (jj-test-with-user-input (:completing-read \"feature\"
                              :y-or-n-p t)
      ;; Test code using both completing-read and y-or-n-p
      )

Usage Notes:
  - Only mocks the functions for keys present in MOCK-CONFIG
  - Mocked functions ignore their arguments and return predetermined values
  - Useful for testing interactive functions without user interaction
  - Combine with other test helpers for complete test isolation"
  (declare (indent 1))
  (let ((config-sym (make-symbol "config")))
    `(let ((,config-sym ,mock-config))
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _args) (plist-get ,config-sym :completing-read)))
                 ((symbol-function 'read-string)
                  (lambda (&rest _args) (plist-get ,config-sym :read-string)))
                 ((symbol-function 'y-or-n-p)
                  (lambda (&rest _args) (plist-get ,config-sym :y-or-n-p))))
         ,@body))))

(defun jj-test-setup-temp-dir ()
  "Create and return an isolated temporary directory for testing.

Creates a new temporary directory with a unique name prefixed with
\\'jj-test-\\'. The directory path is stored in `jj-test--temp-dir' for
later cleanup.

Returns the absolute path to the created temporary directory.

Example:
  (let ((temp-dir (jj-test-setup-temp-dir)))
    ;; Use temp-dir for test
    (jj-test-cleanup-temp-dir))

Note: Always call `jj-test-cleanup-temp-dir' after use to avoid
leaving temporary directories."
  (setq jj-test--temp-dir (make-temp-file "jj-test-" t))
  jj-test--temp-dir)

(defun jj-test-cleanup-temp-dir ()
  "Clean up temporary test directory created by `jj-test-setup-temp-dir'.

Deletes the temporary directory and all its contents if it exists.
Resets `jj-test--temp-dir' to nil.

Safe to call even if no temporary directory exists.

Example in Buttercup test:
  (describe \"My test suite\"
    (before-each
      (jj-test-setup-temp-dir))
    (after-each
      (jj-test-cleanup-temp-dir))
    (it \"does something with temp dir\"
      ...))"
  (when (and jj-test--temp-dir
             (file-exists-p jj-test--temp-dir))
    (delete-directory jj-test--temp-dir t)
    (setq jj-test--temp-dir nil)))

(defmacro jj-test-with-project-folder (folder &rest body)
  "Execute BODY with `jj--get-project-folder' mocked to return FOLDER.

FOLDER should be an absolute path string that will be returned by
`jj--get-project-folder' during the execution of BODY.

This allows testing functions that depend on project folder detection
without requiring an actual .jj directory to exist.

Example:
  (jj-test-with-project-folder \"/tmp/my-test-project/\"
    (expect (jj--get-project-name) :to-equal \"my-test-project\"))

Usage Notes:
  - FOLDER should end with a trailing slash for consistency
  - Combine with `jj-test-with-mocked-command' for complete isolation
  - The mocked folder doesn\\'t need to actually exist on filesystem
  - All functions that call `jj--get-project-folder' will use FOLDER"
  (declare (indent 1))
  `(cl-letf (((symbol-function 'jj--get-project-folder)
              (lambda () ,folder)))
     ,@body))

(defun jj-test-load-fixture (filename)
  "Load and return fixture content from FILENAME.

FILENAME should be relative to the fixtures directory (tests/fixtures/).
Supports loading fixtures from subdirectories using relative paths like
\"errors/command-not-found.txt\" or \"edge-cases/bookmarks-with-special-chars.txt\".

Returns the full content of the fixture file as a string.

Signals an error if the fixture file doesn\\'t exist.

Example:
  (let ((bookmarks (jj-test-load-fixture \"sample-bookmarks.txt\")))
    (expect bookmarks :to-equal \"dev-branch\\nfeature-branch\\nmain\\n\"))

  (let ((error-msg (jj-test-load-fixture \"errors/command-not-found.txt\")))
    (expect error-msg :to-match \"No such file or directory\"))

Available Fixtures:
  Main fixtures:
    - sample-bookmarks.txt  Sample bookmark list output
    - empty-bookmarks.txt   Empty bookmark list
    - sample-log.txt        Sample log output
    - sample-status.txt     Sample status output

  Error scenario fixtures (errors/):
    - invalid-project-no-jj.txt     Error when .jj directory missing
    - command-not-found.txt         Shell error when jj binary not found
    - malformed-bookmark-output.txt Corrupted/invalid jj output
    - empty-log-output.txt          Empty log result (no revisions)

  Edge case fixtures (edge-cases/):
    - bookmarks-with-special-chars.txt  Bookmarks with special characters
    - very-long-bookmark-name.txt       Single bookmark with 100+ char name
    - status-with-many-changes.txt      Status output with 20+ file changes
    - log-single-revision.txt           Log output with exactly one revision

Creating New Fixtures:
  1. Create file in tests/fixtures/ or appropriate subdirectory
  2. Add realistic jj command output
  3. Use descriptive filename (e.g., sample-[command].txt)"
  (let ((fixture-path (expand-file-name filename jj-test--fixtures-dir)))
    (unless (file-exists-p fixture-path)
      (error "Fixture file not found: %s" fixture-path))
    (with-temp-buffer
      (insert-file-contents fixture-path)
      (buffer-string))))

(provide 'test-helper)
;;; test-helper.el ends here
