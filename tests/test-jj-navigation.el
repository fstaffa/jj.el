;;; test-jj-navigation.el --- Tests for jj-status navigation  -*- lexical-binding: t; -*-

;;; Commentary:
;; High-level integration tests for Navigation System
;; Tests navigation functions on real jj-status buffers
;;
;; NOTE: Some tests are skipped on Emacs 29.x due to text property
;; handling differences in special-mode derived buffers.

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; No helper function needed - we'll use assume directly

(describe "jj-status-next-item"
  (it "should navigate to first file in Working Copy Changes section"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n◉  yqosqzyt | Add feature | main\n"
       :status-output "Working copy changes:\nM file1.txt\nA file2.txt\n"
       :bookmark-output "main: yqosqzyt abc123 Add feature\n")
      ;; Start at beginning of buffer
      (goto-char (point-min))
      ;; Navigate to first item
      (jj-status-next-item)
      ;; Should be on a file item
      (let ((item (get-text-property (point) 'jj-item)))
        (expect item :not :to-be nil)
        (expect (plist-get item :status) :to-equal "M")
        (expect (plist-get item :path) :to-equal "file1.txt"))))

  (it "should navigate between multiple files"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\nA file2.txt\nR file3.txt\n"
       :bookmark-output "")
      (goto-char (point-min))
      ;; Navigate to first file
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file1.txt")
      ;; Navigate to second file
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file2.txt")
      ;; Navigate to third file
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file3.txt")))

  (it "should navigate to revisions in Revisions section"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n◉  yqosqzyt | Add feature | main\n◉  mzvwutvl | Fix bug | \n"
       :status-output "Working copy changes:\nM file1.txt\n"
       :bookmark-output "")
      ;; Start in files section
      (goto-char (point-min))
      (jj-status-next-item)
      ;; Should be on file
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file1.txt")
      ;; Navigate to first revision
      (jj-status-next-item)
      (let ((item (get-text-property (point) 'jj-item)))
        (expect (plist-get item :change-id) :to-equal "qpvuntsm"))
      ;; Navigate to second revision
      (jj-status-next-item)
      (let ((item (get-text-property (point) 'jj-item)))
        (expect (plist-get item :change-id) :to-equal "yqosqzyt"))))

  (it "should wrap around to beginning when at end of buffer"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\n"
       :bookmark-output "")
      ;; Navigate to the file
      (goto-char (point-min))
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file1.txt")
      ;; Navigate to revision
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :change-id) :to-equal "qpvuntsm")
      ;; At end, should wrap to first item (file1.txt)
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file1.txt")))

  (it "should skip non-item lines (headers, blank lines)"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\n"
       :bookmark-output "")
      ;; Start at point-min (on header)
      (goto-char (point-min))
      ;; First next-item should skip header and go to file
      (jj-status-next-item)
      (let ((item (get-text-property (point) 'jj-item)))
        (expect item :not :to-be nil)
        (expect (plist-get item :path) :to-equal "file1.txt")))))

(describe "jj-status-prev-item"
  (it "should navigate backwards from revision to file"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\n"
       :bookmark-output "")
      ;; Navigate to revision first
      (goto-char (point-min))
      (jj-status-next-item)
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :change-id) :to-equal "qpvuntsm")
      ;; Now go back to file
      (jj-status-prev-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file1.txt")))

  (it "should navigate backwards through multiple files"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\nA file2.txt\nR file3.txt\n"
       :bookmark-output "")
      ;; Navigate to last file
      (goto-char (point-min))
      (jj-status-next-item)
      (jj-status-next-item)
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file3.txt")
      ;; Navigate backwards
      (jj-status-prev-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file2.txt")
      (jj-status-prev-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file1.txt")))

  (it "should wrap around to end when at beginning"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\n"
       :bookmark-output "")
      ;; Navigate to first file
      (goto-char (point-min))
      (jj-status-next-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :path) :to-equal "file1.txt")
      ;; Go backwards - should wrap to last item (revision)
      (jj-status-prev-item)
      (expect (plist-get (get-text-property (point) 'jj-item) :change-id) :to-equal "qpvuntsm"))))

(describe "jj-status--item-at-point"
  (it "should return file item when on file line"
    (assume (not (version< emacs-version "30.0")) "Text properties unreliable in Emacs 29.x special-mode buffers")
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\n"
       :bookmark-output "")
      (goto-char (point-min))
      (jj-status-next-item)
      (let ((result (jj-status--item-at-point)))
        (expect (plist-get result :type) :to-be 'file)
        (expect (plist-get (plist-get result :data) :path) :to-equal "file1.txt")
        (expect (plist-get (plist-get result :data) :status) :to-equal "M"))))

  (it "should return revision item when on revision line"
    (assume (not (version< emacs-version "30.0")) "Text properties unreliable in Emacs 29.x special-mode buffers")
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n◉  yqosqzyt | Add feature | main\n"
       :status-output "Working copy changes:\n"
       :bookmark-output "")
      (goto-char (point-min))
      (jj-status-next-item)
      (let ((result (jj-status--item-at-point)))
        (expect (plist-get result :type) :to-be 'revision)
        (expect (plist-get (plist-get result :data) :change-id) :to-equal "qpvuntsm")
        (expect (plist-get (plist-get result :data) :description) :to-equal "Working copy"))))

  (it "should return nil when on non-item line (header)"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\n"
       :bookmark-output "")
      ;; Position on header
      (goto-char (point-min))
      (let ((result (jj-status--item-at-point)))
        (expect (plist-get result :type) :to-be nil)
        (expect (plist-get result :data) :to-be nil)))))

(describe "jj-status-show-diff"
  (it "should show placeholder message for files"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\nM file1.txt\n"
       :bookmark-output "")
      ;; Navigate to file
      (goto-char (point-min))
      (jj-status-next-item)
      ;; Call show-diff - should not error, just show message
      (expect (jj-status-show-diff) :not :to-throw)))

  (it "should show placeholder message for revisions"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\n"
       :bookmark-output "")
      ;; Navigate to revision
      (goto-char (point-min))
      (jj-status-next-item)
      ;; Call show-diff - should not error
      (expect (jj-status-show-diff) :not :to-throw)))

  (it "should handle being called on non-item lines"
    (jj-test-with-status-buffer
      (:log-output "@  qpvuntsm | Working copy | \n"
       :status-output "Working copy changes:\n"
       :bookmark-output "")
      ;; Position on header (non-item)
      (goto-char (point-min))
      ;; Should not error
      (expect (jj-status-show-diff) :not :to-throw))))

;;; test-jj-navigation.el ends here
