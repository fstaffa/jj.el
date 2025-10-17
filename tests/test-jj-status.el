;;; test-jj-status.el --- Buttercup tests for jj-status magit-like buffer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Unit Tests for jj-status magit-like buffer feature
;; ===================================================
;;
;; This file contains tests for the magit-like status buffer feature.
;; Tests are organized by task group following the spec in
;; agent-os/specs/2025-10-17-magit-like-status-buffer/
;;
;; Test Coverage Summary
;; ---------------------
;;
;; Task Group 1: Command Execution Infrastructure (2-8 tests)
;;   - jj-status--fetch-revision-list
;;   - jj-status--fetch-working-copy-status
;;   - jj-status--fetch-bookmark-list
;;
;; Running Tests
;; -------------
;;
;; Run all magit-status tests:
;;   eask test buttercup tests/test-jj-status.el
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

;; Task Group 1: Command Execution Infrastructure Tests
;; -----------------------------------------------------
;; Tests for the three fetch functions that execute jj commands
;; and return raw output strings.

(describe "Task Group 1: Command Execution Infrastructure"

  (describe "jj-status--fetch-revision-list"
    (it "should execute jj log command with graph and custom template"
      (let ((expected-cmd "log --revisions \"immutable_heads()..@\" --graph -T 'change_id ++ \"\\n\" ++ description ++ \"\\n\" ++ bookmarks ++ \"\\n\"'")
            (sample-output "@  qpvuntsmqxuquz57\nWorking copy\n\n")
            (captured-command nil))
        (cl-letf (((symbol-function 'jj--run-command)
                   (lambda (cmd)
                     (setq captured-command cmd)
                     (list t sample-output "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (let ((result (jj-status--fetch-revision-list)))
              (expect captured-command :to-equal expected-cmd)
              (expect result :to-equal sample-output))))))

    (it "should return raw command output string"
      (let ((sample-output "◉  yqosqzytrlsw\nAdd feature\nmain\n"))
        (cl-letf (((symbol-function 'jj--run-command)
                   (lambda (_cmd) (list t sample-output "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (expect (jj-status--fetch-revision-list) :to-equal sample-output)))))

    (it "should use jj--with-command for error handling"
      (let ((validation-called nil))
        (cl-letf (((symbol-function 'jj--validate-repository)
                   (lambda ()
                     (setq validation-called t)
                     "/tmp/test/"))
                  ((symbol-function 'jj--run-command)
                   (lambda (_cmd) (list t "output" "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (jj-status--fetch-revision-list)
            (expect validation-called :to-be t))))))

  (describe "jj-status--fetch-working-copy-status"
    (it "should execute jj status command"
      (let ((expected-cmd "status")
            (sample-output "Working copy changes:\nA src/file.el\n")
            (captured-command nil))
        (cl-letf (((symbol-function 'jj--run-command)
                   (lambda (cmd)
                     (setq captured-command cmd)
                     (list t sample-output "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (let ((result (jj-status--fetch-working-copy-status)))
              (expect captured-command :to-equal expected-cmd)
              (expect result :to-equal sample-output))))))

    (it "should return raw command output string"
      (let ((sample-output "Working copy changes:\nM src/existing.el\n"))
        (cl-letf (((symbol-function 'jj--run-command)
                   (lambda (_cmd) (list t sample-output "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (expect (jj-status--fetch-working-copy-status) :to-equal sample-output))))))

  (describe "jj-status--fetch-bookmark-list"
    (it "should execute jj bookmark list command with custom template"
      (let ((expected-cmd "bookmark list -T 'name ++ \"\\t\" ++ change_id ++ \"\\n\"'")
            (sample-output "main\tqpvuntsmqxuquz57\ndev\tyqosqzytrlsw\n")
            (captured-command nil))
        (cl-letf (((symbol-function 'jj--run-command)
                   (lambda (cmd)
                     (setq captured-command cmd)
                     (list t sample-output "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (let ((result (jj-status--fetch-bookmark-list)))
              (expect captured-command :to-equal expected-cmd)
              (expect result :to-equal sample-output))))))

    (it "should return raw command output string"
      (let ((sample-output "feature\tmzvwutvlkqwt\n"))
        (cl-letf (((symbol-function 'jj--run-command)
                   (lambda (_cmd) (list t sample-output "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (expect (jj-status--fetch-bookmark-list) :to-equal sample-output)))))

    (it "should use jj--with-command error handling pattern"
      (let ((validation-called nil))
        (cl-letf (((symbol-function 'jj--validate-repository)
                   (lambda ()
                     (setq validation-called t)
                     "/tmp/test/"))
                  ((symbol-function 'jj--run-command)
                   (lambda (_cmd) (list t "output" "" 0))))
          (jj-test-with-project-folder "/tmp/test"
            (jj-status--fetch-bookmark-list)
            (expect validation-called :to-be t)))))))

;;; test-jj-status.el ends here
