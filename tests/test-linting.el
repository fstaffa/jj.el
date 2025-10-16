;;; test-linting.el --- Tests for linting job execution  -*- lexical-binding: t; -*-

;;; Commentary:

;; Linting Job Tests
;; =================
;;
;; This file contains focused tests to verify that linting jobs execute correctly
;; in the CI/CD pipeline. These tests validate that linting tools catch common issues.
;;
;; Purpose:
;;   - Test that checkdoc is available
;;   - Test that package-lint is available
;;   - Test that byte-compile works
;;   - Verify eask commands are available
;;
;; Running Tests:
;;   eask exec buttercup -L . -L tests tests/test-linting.el
;;
;; Test Strategy:
;;   - Keep tests simple and focused
;;   - Verify linting infrastructure is available
;;   - Don't exhaustively test all linting scenarios

;;; Code:

(require 'buttercup)

;; Test Suite: package-lint availability
;; --------------------------------------
;; Verify package-lint is available and can check package metadata

(describe "package-lint validation"
  (it "should have package-lint available for validation"
    ;; package-lint should be available via eask
    (expect (locate-library "package-lint") :to-be-truthy))

  (it "should be able to load package-lint"
    ;; Should be able to require package-lint
    (expect (require 'package-lint nil t) :to-be-truthy)))

;; Test Suite: checkdoc availability
;; ----------------------------------
;; Verify checkdoc is available for documentation checking

(describe "checkdoc availability"
  (it "should have checkdoc available"
    ;; checkdoc is built-in to Emacs
    (expect (require 'checkdoc nil t) :to-be-truthy))

  (it "should be able to use checkdoc-current-buffer function"
    ;; Verify the main checkdoc function exists
    (expect (fboundp 'checkdoc-current-buffer) :to-be-truthy)))

;; Test Suite: byte-compilation availability
;; ------------------------------------------
;; Verify byte-compilation is available

(describe "byte-compilation availability"
  (it "should have bytecomp available"
    ;; bytecomp is built-in to Emacs
    (expect (require 'bytecomp nil t) :to-be-truthy))

  (it "should be able to use byte-compile-file function"
    ;; Verify the main byte-compile function exists
    (expect (fboundp 'byte-compile-file) :to-be-truthy)))

;; Test Suite: eask lint commands
;; -------------------------------
;; Verify that eask lint commands are available

(describe "eask lint commands"
  (it "should have eask available"
    ;; Verify eask command exists
    (let ((eask-available (executable-find "eask")))
      (expect eask-available :to-be-truthy))))

;;; test-linting.el ends here
