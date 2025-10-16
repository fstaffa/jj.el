;;; test-dependencies.el --- Verify CI/CD dependencies  -*- lexical-binding: t; -*-

;;; Commentary:

;; Dependency Verification Tests
;; ==============================
;;
;; This file contains focused tests to verify that required CI/CD dependencies
;; can be loaded and are available. These tests are minimal by design.
;;
;; Purpose:
;;   - Verify undercover.el can be loaded without errors
;;   - Verify package-lint is available via eask
;;   - Do NOT test exhaustive functionality of these tools
;;
;; Running Tests:
;;   eask exec buttercup -L . -L tests tests/test-dependencies.el

;;; Code:

(require 'buttercup)

;; Test Suite: undercover.el dependency
;; ------------------------------------
;; Verify that undercover.el can be loaded for coverage reporting

(describe "undercover.el dependency"
  (it "should load undercover without errors"
    (expect (require 'undercover nil t) :to-be-truthy)))

;; Test Suite: package-lint availability
;; --------------------------------------
;; Verify that package-lint is available through eask

(describe "package-lint availability"
  (it "should have package-lint available"
    (expect (locate-library "package-lint") :to-be-truthy)))

;; Test Suite: eask dependency resolution
;; ---------------------------------------
;; Verify that eask can resolve development dependencies

(describe "eask dependency resolution"
  (it "should have buttercup available for testing"
    (expect (featurep 'buttercup) :to-be-truthy)))

;;; test-dependencies.el ends here
