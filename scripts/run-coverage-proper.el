;;; run-coverage-proper.el --- Run tests with coverage -*- lexical-binding: t; -*-

;; IMPORTANT: Put current directory FIRST in load-path to ensure
;; we load the source jj.el, not the compiled version from .eask
(setq load-path (cons (expand-file-name ".") load-path))
(add-to-list 'load-path (expand-file-name "tests"))

;; DEBUG: Print environment info
(message "=== Coverage Debug Info ===")
(message "Current directory: %s" default-directory)
(message "Load path: %s" load-path)

;; Load test helper first (it will require buttercup internally)
(load-file "tests/test-helper.el")
(message "✓ Loaded test-helper.el")

;; Load undercover and enable it
(require 'undercover)
(message "✓ Loaded undercover")

;; IMPORTANT: Set force coverage BEFORE calling the undercover macro
(setq undercover-force-coverage t)
(message "✓ Set undercover-force-coverage to t")

;; Create coverage directory if it doesn't exist
(unless (file-exists-p "coverage")
  (make-directory "coverage")
  (message "✓ Created coverage directory"))

(when (file-exists-p "coverage")
  (message "✓ Coverage directory exists"))

;; Call undercover macro to instrument files
;; This MUST be called before loading jj.el
(message "Calling undercover macro to instrument jj.el...")
(undercover "jj.el"
            (:report-file "coverage/.resultset.json")
            (:report-format 'simplecov)
            (:send-report nil))
(message "✓ Called undercover macro")

;; Load jj.el directly (not via require) to ensure undercover instruments it
(message "Loading jj.el...")
(load-file "jj.el")
(message "✓ Loaded jj.el")
;; Provide the feature so test-jj.el's (require 'jj) is satisfied
(provide 'jj)

;; Now load tests
(message "Loading tests...")
(load-file "tests/test-jj.el")
(message "✓ Loaded tests")

;; Run tests
(message "Running tests...")
(buttercup-run)
(message "✓ Tests completed")

;; Coverage report will be saved automatically on exit
(message "Coverage report should be saved to coverage/.resultset.json")
(message "=== End Coverage Debug Info ===")

;;; run-coverage-proper.el ends here
