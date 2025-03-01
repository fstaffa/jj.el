;;; jj.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Filip Staffa
;;
;; Author: Filip Staffa
;; Maintainer: Filip Staffa
;; Created: February 28, 2025
;; Modified: February 28, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:



(provide 'jj)


(defun jj--get-project-folder ()
  (locate-dominating-file default-directory ".jj" ))

(defun jj--get-project-name ()
  (-> (jj--get-project-folder)
      file-name-directory
      directory-file-name
      file-name-nondirectory))

(defun jj-window-quit ()
  "Quits current jj mode window."
  (interactive)
  (quit-window))

(define-key jj-status-mode-map (kbd "q") #'jj-window-quit)

(define-derived-mode jj-status-mode special-mode "jj-status"
  "Major mode for displaying Jujutsu status."
  :group 'jj
  :
  (setq buffer-read-only t))


(defun jj--run-command (command)
  "Run a jj COMMAND from the project root"
  (let* ((default-directory (jj--get-project-folder))
         (jj-cmds (list "jj" "--no-pager" "--color" "never" command))
         (cmd-string (s-join " " jj-cmds)))
    (shell-command-to-string cmd-string)))

(defun jj-status ()
  "Display the status of the current jj repository."
  (interactive)
  (let ((buffer (get-buffer-create (format "jj: %s" (jj--get-project-name)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (jj--run-command "st"))
        (jj-status-mode)))
    (switch-to-buffer buffer)))

(evil-define-key 'normal jj-status-mode-map (kbd "q") #'jj-window-quit)

(map! :leader :desc "jujutsu status" "j s" #'jj-status)
;;; jj.el ends here
