;;; jj.el --- Integration with jujutsu version control system -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Filip Staffa
;;
;; Author: Filip Staffa
;; Maintainer: Filip Staffa
;; Created: February 28, 2025
;; Modified: February 28, 2025
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1") (s "1.13.0") (transient "0.8.0"))
;; Keywords: vc
;; URL: https://github.com/fstaffa/jj.el
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package provides Emacs integration for jujutsu (jj), a Git-compatible
;; version control system. It offers commands for common jj operations like
;; status, log, describe, new, fetch, and push through an intuitive interface.
;;; Code:

;; Declare optional functions from external packages to silence byte-compiler warnings
(declare-function evil-define-key "evil-core")

(require 's)
(require 'transient)
(provide 'jj)


(defun jj--get-project-folder ()
  (locate-dominating-file default-directory ".jj" ))

(defun jj--get-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory
     (jj--get-project-folder)))))

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
    (message cmd-string)
    (shell-command-to-string cmd-string)))

(defun jj-status ()
  "Display the status of the current jj repository."
  (interactive)
  (let ((buffer (get-buffer-create (format "jj: %s" (jj--get-project-name)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (jj--run-command "status"))
        (jj-status-mode)))
    (switch-to-buffer buffer)))


(defun jj-status-describe (args)
  "Run jj describe with ARGS."
  (interactive (list (transient-args 'jj-status-describe-popup)))
  (let* ((cmd (concat "describe " (string-join args " "))))
    (jj--run-command cmd)
    (jj-status)))

(transient-define-prefix jj-status-describe-popup ()
  "Popup for jujutsu describe comand."
  ["Options"
   ("-m" "Message" "-m=" :reader (lambda (&rest _args) (s-concat "\"" (read-string "-m ") "\"")))]
  ["Actions"
   ("d" "Describe" jj-status-describe)])

(defun jj--log-count-revs (revset)
  "Count number of revisions in log."
  (let ((log (jj--run-command (format "log -T '\"a\"' --revisions \"%s\" --no-graph" revset))))
    (length log)))

(defun jj--status-abandon-revset-from-trunk ()
  "Prompt for branch name and create revset from trunk."
  (interactive)
  (let* ((jj-current-revset
          (format "trunk()..%s"
                  (jj--bookmarks-select)))
         (revision-count (jj--log-count-revs jj-current-revset)))
    (if (y-or-n-p (format "Abandon %d revisions? " revision-count))
        (jj-status-abandon (append (list (format "\"%s\""  jj-current-revset)))))))

(defun jj--bookmarks-get ()
  (let ((bookmarks (jj--run-command "bookmark list -T 'name ++ \"\n\"'")))
    (s-split "\n" bookmarks 't)))

(defun jj--bookmarks-select ()
  "Select bookmark from jj."
  (let ((bookmark (completing-read "Select branch: " (jj--bookmarks-get))))
    bookmark))

(defun jj-status-abandon (args)
  "Run jj abandon with ARGS."
  (interactive (list (transient-args 'jj-status-abandon-popup)))
  (let* ((cmd (concat "abandon " (string-join args " "))))
    (jj--run-command cmd)
    (jj-status)))

(transient-define-prefix jj-status-abandon-popup ()
  "Popup for jujutsu abandon comand."
  ["Actions"
   ("b" "Abandon branch from trunk" jj--status-abandon-revset-from-trunk)])

(defun jj--log (args)
  "Run jj log with ARGS."
  (interactive (list (transient-args 'jj-status-log-popup)))
  (let* ((cmd (concat "log " (string-join args " "))))
    (jj--log-show cmd)))

(defun jj--log-show (cmd)
  "Show log for given CMD."
  (interactive)
  (let ((buffer (get-buffer-create (format "jj log: %s" (jj--get-project-name)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (jj--run-command cmd))
        (jj-status-mode)))
    (switch-to-buffer buffer)))

(defun jj--revset-read (&rest _args)
  "Read revset from user."
  (s-concat "\"" (read-string "-r ") "\""))

(transient-define-prefix jj-status-log-popup ()
  "Popup for jujutsu log command"
  :value '("-n=256")
  ["Options"
   ("-r" "Revisions" "--revisions=" :reader jj--revset-read)
   ("-n" "Number of revisions to show" "-n=" :reader (lambda (&rest _args) (s-concat "\"" (read-string "-n ") "\"")))
   ("-s" "Summary, for each path show only whethr it was added, modified or deleted" "--summary" )
   ("-p" "Show patch" "--patch" )
   ("-R" "Show revisions in the opposite order (older first)" "--reversed" )
   ("-G" "Do not display the graph" "--no-graph" )
   ("-S" "Show histogram of changes" "--stat" )
   ("-w" "Ignore whitespace when comparing lines" "--ignore-all-space" )
   ("-W" "Ignore changes in amount of whitespace when comparing lines" "--ignore-space-change" )
   ]
  ["Actions"
   ("l" "Log" jj--log)])

(defun jj--read-revision ()
  "Read revision from user."
  (read-string "Revision: "))

(transient-define-suffix jj--new-parent-revisions-add ()
  "Add REV to the list of parent revisions."
  (interactive)
  (let ((rev (jj--read-revision))
        (scope (transient-scope)))
    (if (and (stringp rev) (not (string-empty-p rev)))
        (push rev scope)
      (user-error (format "Invalid revision %s" rev)))
    (transient-setup transient-current-command nil nil :scope scope)
    (message "Added %s" rev)
    (message "Parents: %s" scope)))

(transient-define-suffix jj--new-parent-revisions-clear ()
  "Clear list of parent revisions."
  (interactive)
  (transient-setup transient-current-command nil nil :scope nil)
  (transient--redisplay))

(defun jj--new-parent-revisions-display ()
  "Display list of parent revisions."
  (if (transient-scope)
      (format "Parents: %s" (s-join ", " (transient-scope)))
    "Using default parent (@)"))

(defun jj--new (args)
  "Run jj new with ARGS."
  (interactive (list (transient-args 'jj-status-new-popup)))
  (let* ((cmd (string-join (append '("new") (transient-scope) args) " ")))
    (jj--run-command cmd)
    (jj-status)))

(transient-define-prefix jj-status-new-popup ()
  "Popup for jujutsu new command."
  ["Parent revisions"
   (:info #'jj--new-parent-revisions-display)
   ("a" "Add parent revision" jj--new-parent-revisions-add :transient t)
   ("c" "Clear parent revisions" jj--new-parent-revisions-clear :transient t)
   ]

  ["Options"
   ("-m" "Message" "-m=" :reader (lambda (&rest _args) (s-concat "\"" (read-string "-m ") "\"")))
   ("-E" "Do not edit the newly created change" "--no-edit")
   ("-b" "Insert the new change before the given commit" "--insert-before" :reader jj--revset-read)
   ("-a" "Insert the new change after the given commit" "--insert-after" :reader jj--revset-read)
   ]
  ["Actions"
   ("n" "New" jj--new)]
  (interactive)
  (transient-setup 'jj-status-new-popup nil nil :scope nil))

(defun jj--fetch (args)
  "Run jj fetch with ARGS."
  (interactive (list (transient-args 'jj-fetch-popup)))
  (let* ((cmd (string-join (append '("git fetch") args) " ")))
    (jj--run-command cmd)
    (message "Successfully fetched")
    (jj-status)))

(transient-define-prefix jj-fetch-popup ()
  "Popup for jujutsu git fetch command"
  :value '("--branch=glob:*")
  ["Options"
   ("-b" "Fetch only some branches" "--branch" :reader (lambda (&rest _args) (s-concat  "\"" (read-string "--branch ") "\"")))
   ("-r" "The remote to fetch from" "--remote" (lambda (&rest _args) (s-concat  "\"" (read-string "--branch ") "\""))) ;;can be repeated
   ("-a" "Fetch from all remotes" "--all-remotes")
   ]
  ["Actions"
   ("f" "Fetch" jj--fetch)])

(defun jj--push (args)
  "Run jj push with ARGS."
  (interactive (list (transient-args 'jj-push-popup)))
  (let* ((cmd (string-join (append '("git push") args) " ")))
    (jj--run-command cmd)
    (jj-status)))

(transient-define-prefix jj-push-popup ()
  "Popup for jujutsu git push command"
  ["Options"
   ("-b" "Push only this bookmark or bookmarks matchin a pattern" "--bookmark=" :reader (lambda (&rest _args) (s-concat  "\"" (read-string "--branch ") "\""))) ;;can be repeated
   ("-c" "Push this commit by creating a bookmark based on its change ID" "--change=" :reader (lambda (&rest _args) (s-concat  "\"" (read-string "--change ") "\""))) ;;can be repeated
   ("-r" "The remote to push to" "--remote=" (lambda (&rest _args) (s-concat  "\"" (read-string "--branch ") "\"")))
   ("-a" "Push all bookmark (including new and deleted)" "--all")
   ("-t" "Push all tracked bookmarks" "--tracked")
   ("-N" "Push all new bookmarks" "--allow-new")
   ("-d" "Push all deleted bookmarks" "--deleted")
   ("-e" "Allow pushing commits with empty descriptions" "--allow-empty-description")
   ("-P" "Allow pushing commits that are private" "--allow-private")
   ]
  ["Push"
   ("p" "Push" jj--push)])

(transient-define-prefix jj-status-popup ()
  "Popup for jujutsu actions in the status buffer."
  ["Actions"
   ("d" "Describe change" jj-status-describe-popup)
   ("a" "Abandon change" jj-status-abandon-popup)
   ("l" "Log" jj-status-log-popup)
   ("n" "new" jj-status-new-popup)
   ("f" "fetch" jj-fetch-popup)
   ("p" "push" jj-push-popup)]
  ["Essential commands"
   ("q" "Quit" jj-window-quit)])

(define-key jj-status-mode-map (kbd "q") #'jj-window-quit)
(define-key jj-status-mode-map (kbd "l") #'jj-status-log-popup)
(define-key jj-status-mode-map (kbd "?") #'jj-status-popup)

;; Optional integration with evil mode
(when (and (boundp 'evil-mode) (fboundp 'evil-define-key))
  (evil-define-key 'normal jj-status-mode-map (kbd "q") #'jj-window-quit)
  (evil-define-key 'normal jj-status-mode-map (kbd "l") #'jj-status-log-popup)
  (evil-define-key 'normal jj-status-mode-map (kbd "?") #'jj-status-popup))

;; Optional integration with Doom Emacs
;; Note: Doom users should add this to their config.el instead:
;; (map! :leader :desc "jujutsu status" "j s" #'jj-status)
;;; jj.el ends here
