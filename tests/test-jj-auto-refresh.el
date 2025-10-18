;;; test-jj-auto-refresh.el --- Tests for jj auto-refresh  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Task Group 7: Auto-refresh Integration
;; ================================================
;;
;; This file tests the auto-refresh functionality that triggers status
;; buffer updates after modification commands like describe, abandon, and new.
;;
;; These commands already call jj-status at their end, which serves as the
;; refresh mechanism. These tests verify that this integration works correctly.
;;
;; Test Coverage:
;;   - Describe command calls jj-status (auto-refresh)
;;   - Abandon command calls jj-status (auto-refresh)
;;   - New command calls jj-status (auto-refresh)
;;
;; Running Tests:
;;   eask test buttercup

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; Helper function to build expected args
(defun jj-test--build-args (command-list)
  "Build args list from COMMAND-LIST the same way jj--run-command does."
  (append '("--no-pager" "--color" "never") (flatten-tree command-list)))

;; Test Suite: Auto-refresh after jj-status-describe
;; --------------------------------------------------
;; Tests that describe command calls jj-status for refresh

(describe "Auto-refresh integration: jj-status-describe"
  (it "should call jj-status after successful describe command"
    (let ((buffer-created nil)
          (describe-cmd '("describe" "-m" "Test message"))
          (status-cmd '("status")))
      (jj-test-with-mocked-command
        (list (list "jj" (jj-test--build-args describe-cmd)
                    :exit-code 0
                    :stdout ""
                    :stderr "")
              (list "jj" (jj-test--build-args status-cmd)
                    :exit-code 0
                    :stdout "Working copy changes:\nM  test.txt\n"
                    :stderr ""))
        (cl-letf (((symbol-function 'switch-to-buffer)
                   (lambda (_buf) (setq buffer-created t) nil)))
          (jj-test-with-project-folder "/tmp/test/"
            (jj-status-describe '("-m" "Test message"))
            ;; Verify that jj-status was called (buffer created)
            (expect buffer-created :to-be t)))))))

;; Test Suite: Auto-refresh after jj-status-abandon
;; -------------------------------------------------
;; Tests that abandon command calls jj-status for refresh

(describe "Auto-refresh integration: jj-status-abandon"
  (it "should call jj-status after successful abandon command"
    (let ((buffer-created nil)
          (abandon-cmd '("abandon" "trunk()..main"))
          (status-cmd '("status")))
      (jj-test-with-mocked-command
        (list (list "jj" (jj-test--build-args abandon-cmd)
                    :exit-code 0
                    :stdout ""
                    :stderr "")
              (list "jj" (jj-test--build-args status-cmd)
                    :exit-code 0
                    :stdout "Working copy changes:\n"
                    :stderr ""))
        (cl-letf (((symbol-function 'switch-to-buffer)
                   (lambda (_buf) (setq buffer-created t) nil)))
          (jj-test-with-project-folder "/tmp/test/"
            (jj-status-abandon '("trunk()..main"))
            ;; Verify that jj-status was called (buffer created)
            (expect buffer-created :to-be t)))))))

;; Test Suite: Auto-refresh after jj--new
;; --------------------------------------
;; Tests that new command calls jj-status for refresh

(describe "Auto-refresh integration: jj--new"
  (it "should call jj-status after successful new command"
    (let ((buffer-created nil)
          (new-cmd '("new" "-m" "New change"))
          (status-cmd '("status")))
      (jj-test-with-mocked-command
        (list (list "jj" (jj-test--build-args new-cmd)
                    :exit-code 0
                    :stdout ""
                    :stderr "")
              (list "jj" (jj-test--build-args status-cmd)
                    :exit-code 0
                    :stdout "Working copy changes:\n"
                    :stderr ""))
        (cl-letf (((symbol-function 'switch-to-buffer)
                   (lambda (_buf) (setq buffer-created t) nil))
                  ((symbol-function 'transient-scope)
                   (lambda () nil)))
          (jj-test-with-project-folder "/tmp/test/"
            (jj--new '("-m" "New change"))
            ;; Verify that jj-status was called (buffer created)
            (expect buffer-created :to-be t)))))))

;;; test-jj-auto-refresh.el ends here
