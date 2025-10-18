;;; test-jj-status-parsing.el --- Tests for jj status parsing -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for Task Group 2: Output Parsing & Data Structures
;;
;; This file contains focused tests for the parsing layer of the magit-like
;; status buffer feature. Tests verify that jj command outputs are correctly
;; parsed into structured data formats.
;;
;; Test Coverage:
;;   - jj-status--parse-log-output: Parse log output with graph
;;   - jj-status--parse-status-output: Parse status output
;;   - jj-status--parse-bookmark-output: Parse bookmark list
;;   - jj-status--determine-unique-prefix: Query unique prefix
;;
;; Running Tests:
;;   eask test buttercup tests/test-jj-status-parsing.el

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

;; Test Suite: jj-status--parse-log-output
;; ----------------------------------------
;; Tests parsing of jj log output with graph characters

(describe "jj-status--parse-log-output"
  (let ((test-cases
         '((:description "should parse log output with graph and extract change IDs"
            :output "@  qpvuntsmqxuquz57 | Working copy | main\n│\n◉  yqosqzytrlsw1234 | Add new feature | \n│\n~  (immutable)\n"
            :expected ((:graph-line "@  "
                        :change-id "qpvuntsmqxuquz57"
                        :description "Working copy"
                        :bookmarks ("main"))
                       (:graph-line "◉  "
                        :change-id "yqosqzytrlsw1234"
                        :description "Add new feature"
                        :bookmarks nil)))
           (:description "should handle no description set placeholder"
            :output "@  qpvuntsmqxuquz57 | (no description set) | \n│\n"
            :expected ((:graph-line "@  "
                        :change-id "qpvuntsmqxuquz57"
                        :description "(no description set)"
                        :bookmarks nil)))
           (:description "should parse multiple bookmarks for a revision"
            :output "◉  yqosqzytrlsw1234 | Fix parser | main dev feature\n│\n"
            :expected ((:graph-line "◉  "
                        :change-id "yqosqzytrlsw1234"
                        :description "Fix parser"
                        :bookmarks ("main" "dev" "feature"))))
           (:description "should handle empty output gracefully"
            :output ""
            :expected nil))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let ((output (plist-get test-case :output))
              (expected (plist-get test-case :expected)))
          (expect (jj-status--parse-log-output output) :to-equal expected))))))

;; Test Suite: jj-status--parse-status-output
;; -------------------------------------------
;; Tests parsing of jj status output for file changes

(describe "jj-status--parse-status-output"
  (let ((test-cases
         '((:description "should parse file paths and status indicators"
            :output "Working copy changes:\nA src/new-file.el\nM src/existing.el\nR src/old-file.el\n"
            :expected ((:path "src/new-file.el" :status "A")
                       (:path "src/existing.el" :status "M")
                       (:path "src/old-file.el" :status "R")))
           (:description "should handle empty working copy"
            :output "Working copy changes:\n"
            :expected nil)
           (:description "should handle untracked files"
            :output "Working copy changes:\n? untracked.txt\n"
            :expected ((:path "untracked.txt" :status "?"))))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let ((output (plist-get test-case :output))
              (expected (plist-get test-case :expected)))
          (expect (jj-status--parse-status-output output) :to-equal expected))))))

;; Test Suite: jj-status--parse-bookmark-output
;; ---------------------------------------------
;; Tests parsing of jj bookmark list output

(describe "jj-status--parse-bookmark-output"
  (let ((test-cases
         '((:description "should parse bookmark names and change IDs from default format"
            :output "main: qpvuntsmqxuquz57 abc123 some description\ndev: yqosqzytrlsw1234 def456 another desc\n"
            :expected ((:name "main" :change-id "qpvuntsmqxuquz57")
                       (:name "dev" :change-id "yqosqzytrlsw1234")))
           (:description "should handle empty bookmark list"
            :output ""
            :expected nil)
           (:description "should handle single bookmark"
            :output "feature: mzvwutvlkqwt5678 ghi789 feature description\n"
            :expected ((:name "feature" :change-id "mzvwutvlkqwt5678"))))))
    (dolist (test-case test-cases)
      (it (plist-get test-case :description)
        (let ((output (plist-get test-case :output))
              (expected (plist-get test-case :expected)))
          (expect (jj-status--parse-bookmark-output output) :to-equal expected))))))

;; Test Suite: jj-status--determine-unique-prefix
;; -----------------------------------------------
;; Tests unique prefix determination for change IDs

(describe "jj-status--determine-unique-prefix"
  (it "should query jj for unique prefix and return plist"
    (let* ((change-id "qpvuntsmqxuquz57")
           (cmd-list (list "log" "-r" change-id "-T" "shortest(change_id)" "--no-graph"))
           (expected-args (append '("--no-pager" "--color" "never")
                                  (flatten-tree cmd-list))))
      (jj-test-with-mocked-command
        (list (list "jj" expected-args
                    :exit-code 0
                    :stdout "qpvuntsm"
                    :stderr ""))
        (jj-test-with-project-folder "/tmp/test"
          (let ((result (jj-status--determine-unique-prefix change-id)))
            (expect (plist-get result :unique-prefix) :to-equal "qpvuntsm")
            (expect (plist-get result :full-id) :to-equal change-id))))))

  (it "should fallback to first 8 chars if query fails"
    (let* ((change-id "qpvuntsmqxuquz57")
           (cmd-list (list "log" "-r" change-id "-T" "shortest(change_id)" "--no-graph"))
           (expected-args (append '("--no-pager" "--color" "never")
                                  (flatten-tree cmd-list))))
      (jj-test-with-mocked-command
        (list (list "jj" expected-args
                    :exit-code 1
                    :stdout ""
                    :stderr "Error"))
        (jj-test-with-project-folder "/tmp/test"
          (let ((result (jj-status--determine-unique-prefix change-id)))
            (expect (plist-get result :unique-prefix) :to-equal "qpvuntsm")
            (expect (plist-get result :full-id) :to-equal change-id)))))))

;;; test-jj-status-parsing.el ends here
