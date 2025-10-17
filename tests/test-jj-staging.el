;;; test-jj-staging.el --- Buttercup tests for jj staging functionality  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit Tests for jj staging functionality
;; =========================================
;;
;; This file contains tests for the file staging system.
;; Tests are organized by task group following the spec in
;; agent-os/specs/2025-10-17-magit-like-status-buffer/
;;
;; Test Coverage Summary
;; ---------------------
;;
;; Task Group 5: File Staging System (2-8 tests)
;;   - jj-status--find-last-described-revision
;;   - jj-status--validate-staging-target
;;   - jj-status-stage-file
;;
;; Running Tests
;; -------------
;;
;; Run staging tests:
;;   eask test buttercup tests/test-jj-staging.el
;;
;; Run all tests:
;;   eask test buttercup
;;
;; Expected behavior:
;;   - All tests should pass
;;   - Tests run in complete isolation
;;   - No actual jj commands are executed

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; Task Group 5: File Staging System Tests
;; ----------------------------------------
;; Tests for the staging functionality including finding last described
;; revision, validation, and staging command execution.

(describe "Task Group 5: File Staging System"

  (describe "jj-status--find-last-described-revision"
    (it "should find most recent revision with description"
      (let ((revisions '((:change-id "abc123" :description "no description set")
                         (:change-id "def456" :description "Add feature")
                         (:change-id "ghi789" :description "Fix bug"))))
        (let ((result (jj-status--find-last-described-revision revisions)))
          (expect (plist-get result :change-id) :to-equal "def456")
          (expect (plist-get result :description) :to-equal "Add feature"))))

    (it "should return nil when no described revision exists"
      (let ((revisions '((:change-id "abc123" :description "no description set")
                         (:change-id "def456" :description "no description set"))))
        (expect (jj-status--find-last-described-revision revisions) :to-be nil)))

    (it "should iterate from top of list (most recent)"
      (let ((revisions '((:change-id "new123" :description "Latest change")
                         (:change-id "old456" :description "Old change"))))
        (let ((result (jj-status--find-last-described-revision revisions)))
          (expect (plist-get result :change-id) :to-equal "new123"))))

    (it "should return nil for empty revision list"
      (expect (jj-status--find-last-described-revision '()) :to-be nil)))

  (describe "jj-status--validate-staging-target"
    (it "should return t when revision is mutable"
      (jj-test-with-mocked-command
        '(("jj" ("--no-pager" "--color" "never" "log" "-r" "\"immutable_heads()\"" "-T" "'change_id'" "--no-graph")
           :exit-code 0
           :stdout "other123\nother456\n"
           :stderr ""))
        (jj-test-with-project-folder "/tmp/test"
          (expect (jj-status--validate-staging-target "abc123") :to-be t))))

    (it "should return nil when revision is immutable"
      (jj-test-with-mocked-command
        '(("jj" ("--no-pager" "--color" "never" "log" "-r" "\"immutable_heads()\"" "-T" "'change_id'" "--no-graph")
           :exit-code 0
           :stdout "abc123\ndef456\n"
           :stderr ""))
        (jj-test-with-project-folder "/tmp/test"
          (expect (jj-status--validate-staging-target "abc123") :to-be nil)))))

  (describe "jj-status-stage-file"
    (it "should error when not on a file"
      (with-temp-buffer
        (jj-status-mode)
        (expect (jj-status-stage-file) :to-throw 'user-error)))

    (it "should error when no described revision found"
      (with-temp-buffer
        (jj-status-mode)
        ;; Set buffer-local revisions with no described revision
        (setq-local jj-status--revisions
                    '((:change-id "abc123" :description "no description set")))
        ;; Insert a file item
        (let ((inhibit-read-only t))
          (insert "  M  test.txt\n")
          (jj-status--mark-item-bounds (point-min) (point)
                                       '(:path "test.txt" :status "M")))
        (goto-char (point-min))
        (expect (jj-status-stage-file) :to-throw 'user-error)))

    (it "should error when target revision is immutable"
      (jj-test-with-mocked-command
        '(("jj" ("--no-pager" "--color" "never" "log" "-r" "\"immutable_heads()\"" "-T" "'change_id'" "--no-graph")
           :exit-code 0
           :stdout "def456\n"
           :stderr ""))
        (jj-test-with-project-folder "/tmp/test"
          (with-temp-buffer
            (jj-status-mode)
            ;; Set buffer-local revisions
            (setq-local jj-status--revisions
                        '((:change-id "def456" :description "Add feature")))
            ;; Insert a file item
            (let ((inhibit-read-only t))
              (insert "  M  test.txt\n")
              (jj-status--mark-item-bounds (point-min) (point)
                                           '(:path "test.txt" :status "M")))
            (goto-char (point-min))
            (expect (jj-status-stage-file) :to-throw 'user-error)))))

    (it "should execute squash command and refresh on success"
      (let ((squash-executed nil)
            (refresh-called nil))
        (jj-test-with-mocked-command
          '(("jj" ("--no-pager" "--color" "never" "log" "-r" "\"immutable_heads()\"" "-T" "'change_id'" "--no-graph")
             :exit-code 0
             :stdout "other123\n"
             :stderr "")
            ("jj" ("--no-pager" "--color" "never" "squash" "--from" "@" "--into" "def456" "test.txt")
             :exit-code 0
             :stdout "Squashed changes\n"
             :stderr ""))
          (jj-test-with-project-folder "/tmp/test"
            (cl-letf (((symbol-function 'jj-status-refresh)
                       (lambda () (setq refresh-called t))))
              (with-temp-buffer
                (jj-status-mode)
                ;; Set buffer-local revisions
                (setq-local jj-status--revisions
                            '((:change-id "def456" :description "Add feature")))
                ;; Insert a file item
                (let ((inhibit-read-only t))
                  (insert "  M  test.txt\n")
                  (jj-status--mark-item-bounds (point-min) (point)
                                               '(:path "test.txt" :status "M")))
                (goto-char (point-min))
                (jj-status-stage-file)
                (expect refresh-called :to-be t)))))))))

;;; test-jj-staging.el ends here
