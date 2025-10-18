;;; jj-evil.el --- Evil mode integration for jj.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Filip Staffa
;;
;; Author: Filip Staffa
;; Maintainer: Filip Staffa
;; Created: October 18, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (evil "1.0.0") (jj "0.0.1"))
;; Keywords: vc jj jujutsu vcs evil vim
;; URL: https://github.com/fstaffa/jj.el
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package provides optional Evil mode integration for jj.el.
;; When loaded, it sets up Evil keybindings for jj-status-mode and
;; jj-describe-mode that mirror the standard keybindings, ensuring
;; consistent behavior whether or not Evil mode is active.
;;
;; Usage:
;;   (require 'jj-evil)
;;   (jj-evil-mode 1)
;;
;; Or with use-package:
;;   (use-package jj-evil
;;     :after (jj evil)
;;     :config
;;     (jj-evil-mode 1))
;;
;;; Code:

(require 'evil)
(require 'jj)

;;; Evil State Configuration

(defgroup jj-evil nil
  "Evil mode integration for jj.el."
  :group 'jj
  :prefix "jj-evil-")

;; Set jj-status-mode to use normal state by default
(evil-set-initial-state 'jj-status-mode 'normal)
(evil-set-initial-state 'jj-describe-mode 'insert)

;;; jj-status-mode Evil Keybindings

(evil-define-key 'normal jj-status-mode-map
  ;; Essential commands
  (kbd "q") #'jj-window-quit
  (kbd "g r") #'jj-status-refresh
  (kbd "?") #'jj-status-popup

  ;; Navigation
  (kbd "n") #'jj-status-next-item
  (kbd "p") #'jj-status-prev-item
  (kbd "j") #'jj-status-next-item    ; Vim-style
  (kbd "k") #'jj-status-prev-item    ; Vim-style
  (kbd "RET") #'jj-status-show-diff

  ;; File operations
  (kbd "s") #'jj-status-stage-file

  ;; Actions
  (kbd "l") #'jj-status-log-popup
  (kbd "d") #'jj-status-describe-popup
  (kbd "a") #'jj-status-abandon-popup
  (kbd "f") #'jj-fetch-popup
  (kbd "P") #'jj-push-popup)

;; Also support visual state selections (for future extensions)
(evil-define-key 'visual jj-status-mode-map
  (kbd "s") #'jj-status-stage-file)

;;; jj-describe-mode Evil Keybindings

(evil-define-key 'insert jj-describe-mode-map
  (kbd "C-c C-c") #'jj-describe-finish
  (kbd "C-c C-k") #'jj-describe-cancel)

(evil-define-key 'normal jj-describe-mode-map
  (kbd "C-c C-c") #'jj-describe-finish
  (kbd "C-c C-k") #'jj-describe-cancel
  ;; Also allow ZZ and ZQ for Vim users
  (kbd "Z Z") #'jj-describe-finish
  (kbd "Z Q") #'jj-describe-cancel)

;;; Minor Mode Definition

;;;###autoload
(define-minor-mode jj-evil-mode
  "Minor mode for Evil integration with jj.el.
When enabled, sets up Evil keybindings for jj modes that mirror
the standard keybindings, ensuring consistent behavior whether
or not Evil mode is active."
  :global t
  :group 'jj-evil
  :lighter " jj-evil"
  (if jj-evil-mode
      (message "jj-evil-mode enabled: Evil keybindings active for jj modes")
    (message "jj-evil-mode disabled")))

(provide 'jj-evil)

;;; jj-evil.el ends here
