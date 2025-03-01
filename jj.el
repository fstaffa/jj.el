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
(require 'transient)


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


(defun jj-status-describe (args)
  "Run jj describe with ARGS."
  (interactive (list (transient-args 'jj-status-describe-popup)))
  (let* ((cmd (concat "describe " (string-join args " "))))
    (message cmd)
    (jj--run-command cmd)
    (jj-status)))

(transient-define-prefix jj-status-describe-popup ()
  "Popup for jujutsu describe comand."
  ["Options"
   ("-m" "Message" "-m=" :reader (lambda (&rest _args) (s-concat "\"" (read-string "-m ") "\"")))]
  ["Actions"
   ("d" "Describe" jj-status-describe)])

(transient-define-prefix jj-status-popup ()
  "Popup for jujutsu actions in the status buffer."
  ["Actions"
   ("d" "Describe change" jj-status-describe-popup)]
  )

(define-key jj-status-mode-map (kbd "q") #'jj-window-quit)
(define-key jj-status-mode-map (kbd "?") #'jj-status-popup)
(evil-define-key 'normal jj-status-mode-map (kbd "q") #'jj-window-quit)
(evil-define-key 'normal jj-status-mode-map (kbd "?") #'jj-status-popup)
(map! :leader :desc "jujutsu status" "j s" #'jj-status)
;;; jj.el ends here
