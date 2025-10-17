;;; test-jj-status-refresh.el --- Tests for jj status refresh  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Task Group 6: Buffer Refresh System
;; =============================================
;;
;; This file tests the buffer refresh functionality for the magit-like
;; status buffer feature. Tests cover manual refresh, cursor position
;; preservation, and refresh after staging operations.
;;
;; Test Coverage:
;;   - Manual refresh with 'g' key
;;   - Cursor position preservation
;;   - Cursor context save/restore
;;   - Refresh after data changes
;;
;; Running Tests:
;;   eask test buttercup tests/test-jj-status-refresh.el

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; Helper function to build expected args
(defun jj-test--build-args (command-string)
  "Build args list from COMMAND-STRING the same way jj--run-command does."
  (append '("--no-pager" "--color" "never") (split-string command-string)))

;; Test Suite: jj-status--save-cursor-context
;; -------------------------------------------
;; Tests cursor context saving functionality

(describe "jj-status--save-cursor-context"
  (it "should return plist with current position and item data"
    (with-temp-buffer
      (insert "Test line with item\n")
      (goto-char (point-min))
      (put-text-property (point-min) (point-max) 'jj-item '(:path "test.txt" :status "M"))
      (let ((context (jj-status--save-cursor-context)))
        (expect (plist-get context :line) :to-equal 1)
        (expect (plist-get context :item) :to-equal '(:path "test.txt" :status "M")))))

  (it "should handle nil item at point"
    (with-temp-buffer
      (insert "Test line without item\n")
      (goto-char (point-min))
      (let ((context (jj-status--save-cursor-context)))
        (expect (plist-get context :line) :to-equal 1)
        (expect (plist-get context :item) :to-be nil)))))

;; Test Suite: jj-status--restore-cursor-context
;; ----------------------------------------------
;; Tests cursor context restoration functionality

(describe "jj-status--restore-cursor-context"
  (it "should restore cursor to same item when it exists"
    (with-temp-buffer
      (insert "Line 1\n")
      (let ((start (point)))
        (insert "Line 2 with item\n")
        (put-text-property start (point) 'jj-item '(:path "test.txt" :status "M")))
      (insert "Line 3\n")
      (goto-char (point-min))
      ;; Save context from line 2
      (forward-line 1)
      (let ((context (jj-status--save-cursor-context)))
        ;; Move away
        (goto-char (point-max))
        ;; Restore should move back to line 2
        (jj-status--restore-cursor-context context)
        (expect (line-number-at-pos) :to-equal 2))))

  (it "should move to first item when original item not found"
    (with-temp-buffer
      (let ((start1 (point)))
        (insert "First item\n")
        (put-text-property start1 (point) 'jj-item '(:path "first.txt" :status "A")))
      (let ((start2 (point)))
        (insert "Second item\n")
        (put-text-property start2 (point) 'jj-item '(:path "second.txt" :status "M")))
      (goto-char (point-min))
      ;; Create context for non-existent item
      (let ((context '(:line 5 :item (:path "missing.txt" :status "R"))))
        (jj-status--restore-cursor-context context)
        ;; Should move to first item
        (expect (line-number-at-pos) :to-equal 1)))))

;; Test Suite: jj-status-refresh
;; ------------------------------
;; Tests manual refresh command

(describe "jj-status-refresh"
  (it "should re-fetch data and re-render buffer"
    (let ((refresh-count 0)
          (buffer nil)
          (log-cmd "log --revisions \"immutable_heads()..@\" --graph -T 'change_id ++ \"\\n\" ++ description ++ \"\\n\" ++ bookmarks ++ \"\\n\"'")
          (status-cmd "status")
          (bookmark-cmd "bookmark list -T 'name ++ \"\\t\" ++ change_id ++ \"\\n\"'"))
      (jj-test-with-mocked-command
        (list (list "jj" (jj-test--build-args log-cmd)
                    :exit-code 0
                    :stdout "@  qpvuntsm\nWorking copy\n\n"
                    :stderr "")
              (list "jj" (jj-test--build-args status-cmd)
                    :exit-code 0
                    :stdout "Working copy changes:\nM  test.txt\n"
                    :stderr "")
              (list "jj" (jj-test--build-args bookmark-cmd)
                    :exit-code 0
                    :stdout "main\tqpvuntsm\n"
                    :stderr ""))
        (jj-test-with-project-folder "/tmp/test/"
          (setq buffer (get-buffer-create "jj: test"))
          (with-current-buffer buffer
            (jj-status-mode)
            (jj-status-refresh)
            (expect (buffer-string) :to-match "Working Copy Changes")
            (expect (buffer-string) :to-match "Revisions")
            (expect (buffer-string) :to-match "Bookmarks"))))
      (when buffer (kill-buffer buffer))))

  (it "should preserve cursor position when item still exists"
    (let ((buffer nil)
          (log-cmd "log --revisions \"immutable_heads()..@\" --graph -T 'change_id ++ \"\\n\" ++ description ++ \"\\n\" ++ bookmarks ++ \"\\n\"'")
          (status-cmd "status")
          (bookmark-cmd "bookmark list -T 'name ++ \"\\t\" ++ change_id ++ \"\\n\"'"))
      (jj-test-with-mocked-command
        (list (list "jj" (jj-test--build-args log-cmd)
                    :exit-code 0
                    :stdout "@  qpvuntsm\nWorking copy\n\n"
                    :stderr "")
              (list "jj" (jj-test--build-args status-cmd)
                    :exit-code 0
                    :stdout "Working copy changes:\nM  test.txt\nA  new.txt\n"
                    :stderr "")
              (list "jj" (jj-test--build-args bookmark-cmd)
                    :exit-code 0
                    :stdout "main\tqpvuntsm\n"
                    :stderr "")
              ;; Second refresh
              (list "jj" (jj-test--build-args log-cmd)
                    :exit-code 0
                    :stdout "@  qpvuntsm\nWorking copy\n\n"
                    :stderr "")
              (list "jj" (jj-test--build-args status-cmd)
                    :exit-code 0
                    :stdout "Working copy changes:\nM  test.txt\nA  new.txt\n"
                    :stderr "")
              (list "jj" (jj-test--build-args bookmark-cmd)
                    :exit-code 0
                    :stdout "main\tqpvuntsm\n"
                    :stderr ""))
        (jj-test-with-project-folder "/tmp/test/"
          (setq buffer (get-buffer-create "jj: test"))
          (with-current-buffer buffer
            (jj-status-mode)
            (jj-status-refresh)
            ;; Move to a file item
            (goto-char (point-min))
            (search-forward "M  test.txt" nil t)
            (let ((line-before (line-number-at-pos)))
              ;; Refresh again
              (jj-status-refresh)
              ;; Cursor should stay near same item
              (expect (line-number-at-pos) :to-be-close-to line-before 2)))))
      (when buffer (kill-buffer buffer)))))

;; Test Suite: jj-status entry point integration
;; ----------------------------------------------
;; Tests that jj-status uses new rendering pipeline

(describe "jj-status entry point"
  (it "should use new rendering pipeline with fetch-parse-render workflow"
    (let ((buffer nil)
          (log-cmd "log --revisions \"immutable_heads()..@\" --graph -T 'change_id ++ \"\\n\" ++ description ++ \"\\n\" ++ bookmarks ++ \"\\n\"'")
          (status-cmd "status")
          (bookmark-cmd "bookmark list -T 'name ++ \"\\t\" ++ change_id ++ \"\\n\"'"))
      (jj-test-with-mocked-command
        (list (list "jj" (jj-test--build-args log-cmd)
                    :exit-code 0
                    :stdout "@  qpvuntsm\nWorking copy\n\n"
                    :stderr "")
              (list "jj" (jj-test--build-args status-cmd)
                    :exit-code 0
                    :stdout "Working copy changes:\nM  test.txt\n"
                    :stderr "")
              (list "jj" (jj-test--build-args bookmark-cmd)
                    :exit-code 0
                    :stdout "main\tqpvuntsm\n"
                    :stderr ""))
        (cl-letf (((symbol-function 'switch-to-buffer)
                   (lambda (buf) (setq buffer buf) nil)))
          (jj-test-with-project-folder "/tmp/test/"
            (jj-status)
            (with-current-buffer buffer
              ;; Verify structured sections exist
              (expect (buffer-string) :to-match "Working Copy Changes")
              (expect (buffer-string) :to-match "Revisions")
              (expect (buffer-string) :to-match "Bookmarks")
              ;; Verify buffer-local data is set
              (expect jj-status--parsed-data :to-be-truthy)))))
      (when buffer (kill-buffer buffer)))))

;;; test-jj-status-refresh.el ends here
