;;; run-coverage-proper.el --- Run tests with coverage -*- lexical-binding: t; -*-

;; IMPORTANT: Put current directory FIRST in load-path to ensure
;; we load the source jj.el, not the compiled version from .eask
(setq load-path (cons (expand-file-name ".") load-path))
(add-to-list 'load-path (expand-file-name "tests"))

;; Load test helper first
(load-file "tests/test-helper.el")

;; Load undercover and enable it
(require 'undercover)

;; IMPORTANT: Set force coverage BEFORE calling the undercover macro
(setq undercover-force-coverage t)

;; Create coverage directory if it doesn't exist
(unless (file-exists-p "coverage")
  (make-directory "coverage"))

;; Call undercover macro to instrument files
;; This MUST be called before loading jj.el
(undercover "jj.el"
            (:report-file "coverage/.resultset.json")
            (:report-format 'simplecov)
            (:send-report nil))

;; Load jj.el directly (not via require) to ensure undercover instruments it
(load-file "jj.el")
;; Provide the feature so test-jj.el's (require 'jj) is satisfied
(provide 'jj)

;; Now load tests
(load-file "tests/test-jj.el")

;; Run tests
(buttercup-run)

;; Coverage report will be saved automatically on exit

;;; run-coverage-proper.el ends here
