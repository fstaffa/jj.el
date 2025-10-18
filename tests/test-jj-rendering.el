;;; test-jj-rendering.el --- Tests for jj.el rendering functions  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Unit Tests for jj.el Buffer Rendering Functions
;; ===============================================
;;
;; This file contains focused tests for Task Group 3: Buffer Rendering System.
;; Tests verify section rendering, face application, and formatting logic.
;;
;; Test Coverage:
;; - Section header rendering with proper faces
;; - Change ID formatting with bold/grey faces
;; - Working copy section rendering
;; - Revisions section rendering with graph
;; - Bookmarks section rendering
;; - Complete buffer rendering coordination
;;
;; Running Tests:
;; -------------
;;
;; Run these tests only:
;;   eask test buttercup tests/test-jj-rendering.el
;;
;; Run all tests:
;;   eask test buttercup

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; Test Suite: jj-status--render-section-header
;; ---------------------------------------------
;; Tests section header rendering with proper face application

(describe "jj-status--render-section-header"
  (it "should insert header with jj-status-section-heading face"
    (with-temp-buffer
      (jj-status--render-section-header "Test Section")
      (goto-char (point-min))
      (expect (get-text-property (point) 'face) :to-be 'jj-status-section-heading)
      (expect (buffer-substring-no-properties (point-min) (line-end-position))
              :to-equal "Test Section")))

  (it "should add blank line after header"
    (with-temp-buffer
      (jj-status--render-section-header "Test Section")
      (expect (buffer-string) :to-equal "Test Section\n\n"))))

;; Test Suite: jj-status--format-change-id
;; ----------------------------------------
;; Tests change ID formatting with bold prefix and grey suffix

(describe "jj-status--format-change-id"
  (it "should apply bold face to first 8 characters"
    (let* ((change-id "qpvuntsmqxuquz57")
           (formatted (jj-status--format-change-id change-id))
           (prefix-end 8))
      (expect (get-text-property 0 'face formatted) :to-be 'jj-status-change-id-unique)
      (expect (substring-no-properties formatted 0 prefix-end) :to-equal "qpvuntsm")))

  (it "should apply grey face to remaining suffix"
    (let* ((change-id "qpvuntsmqxuquz57")
           (formatted (jj-status--format-change-id change-id)))
      (expect (get-text-property 8 'face formatted) :to-be 'jj-status-change-id-suffix)
      (expect (substring-no-properties formatted 8) :to-equal "qxuquz57")))

  (it "should handle short change IDs without suffix"
    (let* ((change-id "qpvuntsm")
           (formatted (jj-status--format-change-id change-id)))
      (expect (get-text-property 0 'face formatted) :to-be 'jj-status-change-id-unique)
      (expect (substring-no-properties formatted) :to-equal "qpvuntsm"))))

;; Test Suite: jj-status--render-working-copy
;; -------------------------------------------
;; Tests working copy section rendering

(describe "jj-status--render-working-copy"
  (it "should render files with status and path"
    (with-temp-buffer
      (let ((files '((:path "file1.txt" :status "M")
                     (:path "file2.el" :status "A"))))
        (jj-status--render-working-copy files)
        (expect (buffer-string) :to-match "Working Copy Changes")
        (expect (buffer-string) :to-match "M  file1.txt")
        (expect (buffer-string) :to-match "A  file2.el"))))

  (it "should display 'no changes' for empty file list"
    (with-temp-buffer
      (jj-status--render-working-copy nil)
      (expect (buffer-string) :to-match "Working Copy Changes")
      (expect (buffer-string) :to-match "(no changes)"))))

;; Test Suite: jj-status--render-revisions
;; ----------------------------------------
;; Tests revisions section rendering with graph

(describe "jj-status--render-revisions"
  (it "should render revision with graph, change ID, and description"
    (with-temp-buffer
      (let ((revisions '((:graph-line "@  "
                          :change-id "qpvuntsm"
                          :description "Working copy"
                          :bookmarks nil))))
        (jj-status--render-revisions revisions)
        (expect (buffer-string) :to-match "Revisions (immutable_heads()..@)")
        (expect (buffer-string) :to-match "@")
        (expect (buffer-string) :to-match "qpvuntsm")
        (expect (buffer-string) :to-match "Working copy"))))

  (it "should render bookmarks when present"
    (with-temp-buffer
      (let ((revisions '((:graph-line "◉  "
                          :change-id "yqosqzyt"
                          :description "Add feature"
                          :bookmarks ("main" "dev")))))
        (jj-status--render-revisions revisions)
        (expect (buffer-string) :to-match "\\[main\\]")
        (expect (buffer-string) :to-match "\\[dev\\]"))))

  (it "should display 'no revisions' for empty list"
    (with-temp-buffer
      (jj-status--render-revisions nil)
      (expect (buffer-string) :to-match "(no revisions)"))))

;; Test Suite: jj-status--render-bookmarks
;; ----------------------------------------
;; Tests bookmarks section rendering

(describe "jj-status--render-bookmarks"
  (it "should render bookmarks with name and change ID"
    (with-temp-buffer
      (let ((bookmarks '((:name "main" :change-id "qpvuntsm")
                         (:name "dev" :change-id "yqosqzyt"))))
        (jj-status--render-bookmarks bookmarks)
        (expect (buffer-string) :to-match "Bookmarks")
        (expect (buffer-string) :to-match "main.*qpvuntsm")
        (expect (buffer-string) :to-match "dev.*yqosqzyt"))))

  (it "should display 'no bookmarks' for empty list"
    (with-temp-buffer
      (jj-status--render-bookmarks nil)
      (expect (buffer-string) :to-match "(no bookmarks)"))))

;; Test Suite: jj-status--render-buffer
;; -------------------------------------
;; Tests complete buffer rendering coordination

(describe "jj-status--render-buffer"
  (it "should render all sections in correct order"
    (with-temp-buffer
      (jj-test-with-project-folder "/tmp/test-project/"
        (let ((revisions '((:graph-line "@  " :change-id "qpvuntsm"
                            :description "Working copy" :bookmarks nil)))
              (files '((:path "test.txt" :status "M")))
              (bookmarks '((:name "main" :change-id "qpvuntsm"))))
          (jj-status--render-buffer revisions files bookmarks)
          ;; Verify buffer title
          (expect (buffer-string) :to-match "jj: test-project")
          ;; Verify section order
          (let ((content (buffer-string)))
            (let ((wc-pos (string-match "Working Copy" content))
                  (rev-pos (string-match "Revisions" content))
                  (bm-pos (string-match "Bookmarks" content)))
              (expect (< wc-pos rev-pos) :to-be t)
              (expect (< rev-pos bm-pos) :to-be t)))))))

  (it "should clear existing buffer content"
    (with-temp-buffer
      (jj-test-with-project-folder "/tmp/test/"
        (insert "Old content")
        (jj-status--render-buffer nil nil nil)
        (expect (buffer-string) :not :to-match "Old content")))))

;;; test-jj-rendering.el ends here
