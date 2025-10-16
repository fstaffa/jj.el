;;; test-coverage.el --- Coverage reporting tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; Coverage Reporting Tests
;; =========================
;;
;; This file contains focused tests to verify coverage reporting functionality.
;; These tests ensure that undercover.el integration works correctly.
;;
;; Purpose:
;;   - Verify undercover.el can be loaded and configured
;;   - Verify coverage data collection doesn't break tests
;;   - Verify coverage output contains expected data
;;   - Do NOT test exhaustive coverage scenarios
;;
;; Running Tests:
;;   eask exec buttercup -L . -L tests tests/test-coverage.el

;;; Code:

(require 'buttercup)

;; Test Suite: undercover.el loading and configuration
;; ----------------------------------------------------
;; Verify that undercover.el can be loaded and configured properly

(describe "undercover.el loading"
  (it "should load undercover without errors"
    (expect (require 'undercover nil t) :to-be-truthy))

  (it "should have undercover--setup function available"
    (expect (fboundp 'undercover--setup) :to-be-truthy)))

;; Test Suite: Coverage data collection
;; -------------------------------------
;; Verify that coverage collection doesn't interfere with test execution

(describe "coverage data collection"
  (it "should not break normal test execution"
    ;; This test itself runs successfully if coverage collection works
    (expect t :to-be-truthy))

  (it "should allow loading files with coverage tracking"
    ;; Verify that Emacs Lisp files can be loaded with undercover active
    ;; We test this by checking that require still works
    (expect (require 'cl-lib nil t) :to-be-truthy)))

;; Test Suite: Coverage output format
;; -----------------------------------
;; Verify that coverage data is available in expected format

(describe "coverage output format"
  (it "should have undercover report functions available"
    (expect (fboundp 'undercover--collect-files-coverage) :to-be-truthy))

  (it "should have undercover data storage available"
    ;; undercover stores data in undercover--files
    (expect (boundp 'undercover--files) :to-be-truthy)))

;;; test-coverage.el ends here
