;;; test-jj-status-integration.el --- Integration tests for jj-status  -*- lexical-binding: t; -*-

;;; Commentary:

;; Integration Tests for jj-status magit-like buffer feature
;; ==========================================================
;;
;; This file contains strategic integration tests to fill critical coverage gaps
;; identified in Task Group 8: Test Review & Gap Analysis.
;;
;; Test Coverage:
;;   - End-to-end workflow integration tests
;;   - Error handling for malformed jj output
;;   - Integration of fetch + parse + render pipeline
;;   - Complex scenarios with fixtures
;;
;; Running Tests:
;;   eask test buttercup

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; Helper function to build expected args
(defun jj-test--build-args (command-string)
  "Build args list from COMMAND-STRING the same way jj--run-command does."
  (append '("--no-pager" "--color" "never") (split-string command-string)))

;; Test Suite: Fetch-Parse-Render Pipeline Integration
;; ----------------------------------------------------
;; Tests the complete data flow from fetch to render

(describe "Integration: Fetch-parse-render pipeline"
  (it "should correctly parse and render complex log with graph"
    (let ((log-fixture (jj-test-load-fixture "magit-status/sample-log-with-graph.txt"))
          (parsed-data nil))
      ;; Test parsing
      (setq parsed-data (jj-status--parse-log-output log-fixture))

      ;; Verify parsing extracted correct number of revisions
      (expect (length parsed-data) :to-equal 5)

      ;; Verify first revision (working copy)
      (let ((first-rev (car parsed-data)))
        (expect (plist-get first-rev :graph-line) :to-equal "@  ")
        (expect (plist-get first-rev :change-id) :to-equal "qpvuntsmqxuquz57")
        (expect (plist-get first-rev :description) :to-equal "Working copy"))

      ;; Verify second revision (with description and bookmark)
      (let ((second-rev (cadr parsed-data)))
        (expect (plist-get second-rev :change-id) :to-equal "yqosqzytrlsw1234")
        (expect (plist-get second-rev :description) :to-equal "Add new feature")
        (expect (plist-get second-rev :bookmarks) :to-equal '("main")))

      ;; Test rendering
      (with-temp-buffer
        (jj-status--render-revisions parsed-data)

        ;; Verify graph characters rendered
        (expect (buffer-string) :to-match "@")
        (expect (buffer-string) :to-match "◉")
        (expect (buffer-string) :to-match "│")

        ;; Verify change IDs rendered
        (expect (buffer-string) :to-match "qpvuntsm")
        (expect (buffer-string) :to-match "yqosqzyt")

        ;; Verify descriptions rendered
        (expect (buffer-string) :to-match "Working copy")
        (expect (buffer-string) :to-match "Add new feature")

        ;; Verify bookmarks rendered
        (expect (buffer-string) :to-match "\\[main\\]")
        (expect (buffer-string) :to-match "\\[dev\\]"))))

  (it "should correctly parse and render status with multiple file types"
    (let ((status-fixture (jj-test-load-fixture "magit-status/sample-status-with-files.txt"))
          (parsed-data nil))
      ;; Test parsing
      (setq parsed-data (jj-status--parse-status-output status-fixture))

      ;; Verify parsing extracted all files
      (expect (length parsed-data) :to-equal 5)

      ;; Verify different status types
      (let ((added-files (cl-remove-if-not
                          (lambda (f) (string= (plist-get f :status) "A"))
                          parsed-data))
            (modified-files (cl-remove-if-not
                             (lambda (f) (string= (plist-get f :status) "M"))
                             parsed-data))
            (removed-files (cl-remove-if-not
                            (lambda (f) (string= (plist-get f :status) "R"))
                            parsed-data)))
        (expect (length added-files) :to-equal 2)
        (expect (length modified-files) :to-equal 2)
        (expect (length removed-files) :to-equal 1))

      ;; Test rendering
      (with-temp-buffer
        (jj-status--render-working-copy parsed-data)

        ;; Verify all status indicators rendered
        (expect (buffer-string) :to-match "A  src/new-feature.el")
        (expect (buffer-string) :to-match "M  src/core.el")
        (expect (buffer-string) :to-match "R  src/deprecated.el")))))

;; Test Suite: Error Handling
;; ---------------------------
;; Tests graceful handling of malformed output

(describe "Integration: Malformed output handling"
  (it "should handle malformed log output without crashing"
    (let ((malformed-log (jj-test-load-fixture "magit-status/malformed-log-output.txt")))
      ;; Should not throw error
      (expect (jj-status--parse-log-output malformed-log) :not :to-throw)))

  (it "should handle empty status output"
    (let ((empty-status (jj-test-load-fixture "magit-status/empty-status.txt")))
      (let ((parsed (jj-status--parse-status-output empty-status)))
        (expect parsed :to-equal nil)))))

;; Test Suite: No Described Revision Scenario
;; -------------------------------------------
;; Tests staging behavior when no revision has a description

(describe "Integration: No described revision"
  (it "should find no described revision when all are placeholder"
    (let ((log-no-desc (jj-test-load-fixture "magit-status/sample-log-no-description.txt")))
      (let* ((parsed-revs (jj-status--parse-log-output log-no-desc))
             (last-described (jj-status--find-last-described-revision parsed-revs)))
        (expect last-described :to-be nil)))))

;; Test Suite: Bookmark Parsing
;; -----------------------------
;; Tests bookmark list parsing with fixtures

(describe "Integration: Bookmark parsing"
  (it "should parse bookmark fixture correctly"
    (let ((bookmark-fixture (jj-test-load-fixture "magit-status/sample-bookmarks.txt")))
      (let ((parsed-bookmarks (jj-status--parse-bookmark-output bookmark-fixture)))
        (expect (length parsed-bookmarks) :to-equal 3)
        (let ((first-bookmark (car parsed-bookmarks)))
          (expect (plist-get first-bookmark :name) :to-equal "main")
          (expect (plist-get first-bookmark :change-id) :to-equal "yqosqzytrlsw1234"))))))

;; Test Suite: End-to-End Parsing Pipeline
;; ----------------------------------------
;; Tests complete parsing of all three command outputs

(describe "Integration: Complete parsing pipeline"
  (it "should parse all three outputs without errors"
    (let ((log-fixture (jj-test-load-fixture "magit-status/sample-log-with-graph.txt"))
          (status-fixture (jj-test-load-fixture "magit-status/sample-status-with-files.txt"))
          (bookmark-fixture (jj-test-load-fixture "magit-status/sample-bookmarks.txt")))
      ;; Parse all three
      (let ((revisions (jj-status--parse-log-output log-fixture))
            (files (jj-status--parse-status-output status-fixture))
            (bookmarks (jj-status--parse-bookmark-output bookmark-fixture)))
        ;; Verify all parsed successfully
        (expect revisions :to-be-truthy)
        (expect (length revisions) :to-be-greater-than 0)
        (expect files :to-be-truthy)
        (expect (length files) :to-be-greater-than 0)
        (expect bookmarks :to-be-truthy)
        (expect (length bookmarks) :to-be-greater-than 0)))))

;; Test Suite: Rendering Integration
;; ----------------------------------
;; Tests rendering with realistic fixture data

(describe "Integration: Complete buffer rendering"
  (it "should render buffer with all three sections"
    (let ((log-fixture (jj-test-load-fixture "magit-status/sample-log-with-graph.txt"))
          (status-fixture (jj-test-load-fixture "magit-status/sample-status-with-files.txt"))
          (bookmark-fixture (jj-test-load-fixture "magit-status/sample-bookmarks.txt")))
      (with-temp-buffer
        (jj-test-with-project-folder "/tmp/test-project/"
          (let ((revisions (jj-status--parse-log-output log-fixture))
                (files (jj-status--parse-status-output status-fixture))
                (bookmarks (jj-status--parse-bookmark-output bookmark-fixture)))
            (jj-status--render-buffer revisions files bookmarks)

            ;; Verify all sections present
            (expect (buffer-string) :to-match "jj: test-project")
            (expect (buffer-string) :to-match "Working Copy Changes")
            (expect (buffer-string) :to-match "Revisions")
            (expect (buffer-string) :to-match "Bookmarks")

            ;; Verify content from fixtures
            (expect (buffer-string) :to-match "src/new-feature.el")
            (expect (buffer-string) :to-match "qpvuntsm")
            (expect (buffer-string) :to-match "main")))))))

;;; test-jj-status-integration.el ends here
