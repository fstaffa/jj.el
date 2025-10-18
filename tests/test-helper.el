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
;; `call-process`. This ensures:
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
;; Example 1: Test a function that runs a jj command with new return format
;;
;;   (describe "jj--bookmarks-get"
;;     (it "should parse bookmark list"
;;       (jj-test-with-mocked-command
;;         '(("jj" ("--no-pager" "--color" "never" "bookmark" "list" "-T" "name ++ \"\\n\"")
;;            :exit-code 0
;;            :stdout "main\\ndev-branch\\n"
;;            :stderr ""))
;;         (jj-test-with-project-folder "/tmp/test"
;;           (expect (jj--bookmarks-get) :to-equal '("main" "dev-branch"))))))
;;
;; Example 2: Test with fixture data
;;
;;   (describe "jj--bookmarks-get"
;;     (it "should parse bookmarks from fixture"
;;       (let ((fixture (jj-test-load-fixture "sample-bookmarks.txt")))
;;         (jj-test-with-mocked-command
;;           '(("jj" ("--no-pager" "--color" "never" "bookmark" "list" "-T" "name ++ \"\\n\"")
;;              :exit-code 0
;;              :stdout fixture
;;              :stderr ""))
;;           (jj-test-with-project-folder "/tmp/test"
;;             (expect (jj--bookmarks-get) :to-equal '("dev-branch" "feature-branch" "main")))))))
;;
;; Example 3: Test command failure with stderr
;;
;;   (describe "jj--run-command"
;;     (it "should handle command failure"
;;       (jj-test-with-mocked-command
;;         '(("jj" ("--no-pager" "--color" "never" "invalid")
;;            :exit-code 1
;;            :stdout ""
;;            :stderr "Error: unknown command"))
;;         (jj-test-with-project-folder "/tmp/test"
;;           (let ((result (jj--run-command "invalid")))
;;             (expect (car result) :to-be nil)
;;             (expect (cadddr result) :to-equal 1))))))
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
;;   eask test buttercup
;;
;; Expected output:
;;   - All tests should pass
;;   - Execution time should be under 2 seconds
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

(defmacro jj-test-with-mocked-command (command-specs &rest body)
  "Execute BODY with mocked call-process for jj commands.

COMMAND-SPECS is a list of command specifications, where each spec has the form:
  (PROGRAM ARGS-LIST :exit-code EXIT :stdout STDOUT :stderr STDERR)

PROGRAM is the program name (e.g., \"jj\")
ARGS-LIST is a list of argument strings
EXIT is the exit code (integer)
STDOUT is the stdout output string
STDERR is the stderr output string

This macro mocks `call-process' to return predefined outputs based on
command matching. Commands are matched by program name and args list.

Example:
  (jj-test-with-mocked-command
    '((\"jj\" (\"--no-pager\" \"--color\" \"never\" \"status\")
       :exit-code 0
       :stdout \"Working copy changes:\\n\"
       :stderr \"\"))
    (expect (jj--run-command \"status\") :to-equal '(t \"Working copy changes:\\n\" \"\" 0)))

The macro will signal an error if an unexpected command is executed,
helping catch issues where commands don't match expected format.

Usage Notes:
  - Command matching uses equal for program and args comparison
  - Multiple commands can be mocked by providing multiple specs
  - Combine with `jj-test-with-project-folder' to fully isolate tests
  - Unexpected commands will signal an error with the command details"
  (declare (indent 1))
  (let ((specs-sym (make-symbol "specs")))
    `(let ((,specs-sym ,command-specs))
       (cl-letf (((symbol-function 'call-process)
                  (lambda (program infile destination _display &rest args)
                    (let ((spec (cl-find-if
                                 (lambda (s)
                                   (and (equal program (car s))
                                        (equal args (cadr s))))
                                 ,specs-sym)))
                      (if spec
                          (let ((exit-code (plist-get (cddr spec) :exit-code))
                                (stdout (plist-get (cddr spec) :stdout))
                                (stderr (plist-get (cddr spec) :stderr)))
                            (when destination
                              (let ((stdout-buf (cond
                                                 ;; Cons cell: (stdout . stderr)
                                                 ((consp destination) (car destination))
                                                 ;; Single buffer
                                                 (t destination)))
                                    (stderr-buf (when (consp destination)
                                                  ;; For cons cell, cdr is the stderr buffer
                                                  (cdr destination))))
                                (when stdout-buf
                                  (with-current-buffer stdout-buf
                                    (insert stdout)))
                                (when stderr-buf
                                  (with-current-buffer stderr-buf
                                    (insert stderr)))))
                            exit-code)
                        (error "Unexpected command: %s %s" program (string-join args " ")))))))
         ,@body))))

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

(defmacro jj-test-with-status-buffer (mock-outputs &rest body)
  "Execute BODY with a real jj-status buffer created using mocked jj commands.

MOCK-OUTPUTS is a plist with keys:
  :log-output       - Output from 'jj log' command (for revisions)
  :status-output    - Output from 'jj status' command (for files)
  :bookmark-output  - Output from 'jj bookmark list' command (for bookmarks)

This macro:
1. Mocks jj--run-command to return the specified outputs for each command
2. Calls jj-status to create a real status buffer
3. Switches to that buffer and executes BODY in its context
4. Automatically cleans up the buffer after BODY completes

The created buffer is a fully functional jj-status buffer with:
- Proper text properties (jj-item) for navigation
- Correct buffer-local mode (jj-status-mode)
- Parsed data structures in buffer-local variables

Example:
  (jj-test-with-status-buffer
    (:log-output \"@  qpvuntsm | Working copy | main\\n\"
     :status-output \"Working copy changes:\\nM  file.txt\\n\"
     :bookmark-output \"main: qpvuntsm abc123 desc\\n\")
    ;; Test navigation in the real buffer
    (goto-char (point-min))
    (jj-status-next-item)
    (expect (get-text-property (point) 'jj-item) :not :to-be nil))

Usage Notes:
  - The buffer is created with all standard jj-status initialization
  - BODY executes in the context of the jj-status buffer
  - All three outputs must be provided (use empty strings if needed)
  - The buffer is automatically cleaned up after BODY completes
  - Combine with jj-test-with-project-folder for complete isolation"
  (declare (indent 1))
  (let ((log-output (plist-get mock-outputs :log-output))
        (status-output (plist-get mock-outputs :status-output))
        (bookmark-output (plist-get mock-outputs :bookmark-output))
        (buffer-sym (make-symbol "buffer")))
    `(let ((,buffer-sym nil))
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'jj--run-command)
                        (lambda (args)
                          ;; Match command based on first argument after global args
                          ;; args is always a list like '("log" ...) or '("status") or '("bookmark" "list")
                          (let ((cmd (car args)))
                            (cond
                             ;; Match "log" command
                             ((equal cmd "log")
                              (list t ,log-output "" 0))
                             ;; Match "status" command
                             ((equal cmd "status")
                              (list t ,status-output "" 0))
                             ;; Match "bookmark" command
                             ((equal cmd "bookmark")
                              (list t ,bookmark-output "" 0))
                             (t
                              (error "Unexpected jj command in test: %s" args)))))))
               (jj-test-with-project-folder "/tmp/test-project/"
                 ;; Call jj-status which will create the buffer
                 (jj-status)
                 (setq ,buffer-sym (current-buffer))
                 ;; Execute test body in the created buffer
                 ,@body)))
         ;; Cleanup: kill the buffer if it was created
         (when (and ,buffer-sym (buffer-live-p ,buffer-sym))
           (kill-buffer ,buffer-sym))))))

(provide 'test-helper)
;;; test-helper.el ends here
