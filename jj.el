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
;; Keywords: vc jj jujutsu vcs
;; URL: https://github.com/fstaffa/jj.el
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; This package provides Emacs integration for jujutsu (jj), a Git-compatible
;; version control system. It offers commands for common jj operations like
;; status, log, describe, new, fetch, and push through an intuitive interface.
;;; Code:


(require 's)
(require 'transient)
(provide 'jj)

;;; Configuration Variables

(defcustom jj-debug-mode nil
  "Enable debug logging for jj command execution.
When non-nil, log detailed information about command execution
to the *Messages* buffer, including commands run, exit codes,
and stderr output."
  :type 'boolean
  :group 'jj)

(defcustom jj-error-buffer-name "*jj-errors*"
  "Name of the buffer used to store detailed error context.
This buffer accumulates error information including timestamps,
commands, exit codes, and stderr/stdout output for debugging
purposes."
  :type 'string
  :group 'jj)

;;; Project Folder Management

(defun jj--get-project-folder ()
  (locate-dominating-file default-directory ".jj" ))

(defun jj--get-project-name ()
  (file-name-nondirectory
   (directory-file-name
    (file-name-directory
     (jj--get-project-folder)))))

(defun jj-window-quit ()
  "Quit the current jj buffer window.
Bound to q key in jj-status-mode."
  (interactive)
  (quit-window))

;;; Major Mode Definition

(defvar-local jj-status--parsed-data nil
  "Buffer-local variable storing parsed jj data structures.
Plist containing :revisions, :files, and :bookmarks from last fetch.")

(define-derived-mode jj-status-mode special-mode "jj-status"
  "Major mode for displaying Jujutsu status."
  :group 'jj
  :
  (setq buffer-read-only t)
  ;; Disable font-lock to prevent it from interfering with text properties
  (setq font-lock-defaults nil)
  (font-lock-mode -1))

;;; Custom Faces for Status Buffer
;; Task Group 3: Buffer Rendering System

(defface jj-status-section-heading
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face for section headers in jj status buffer."
  :group 'jj)

(defface jj-status-change-id-unique
  '((t :inherit font-lock-constant-face :weight bold))
  "Face for unique prefix portion of change IDs in jj status buffer."
  :group 'jj)

(defface jj-status-change-id-suffix
  '((t :inherit shadow))
  "Face for non-unique suffix portion of change IDs in jj status buffer.
Uses shadow face for dimmed/grey appearance."
  :group 'jj)

(defface jj-status-graph
  '((t :inherit font-lock-comment-face))
  "Face for ASCII graph characters in jj status buffer."
  :group 'jj)

;;; Debug Logging

(defun jj--debug-log (format-string &rest args)
  "Log a debug message to *Messages* buffer when `jj-debug-mode' is enabled.
FORMAT-STRING and ARGS are passed to `format' to construct the message.
Messages are prefixed with \"[jj-debug] \" for easy filtering."
  (when jj-debug-mode
    (apply #'message (concat "[jj-debug] " format-string) args)))

;;; Command Execution Infrastructure

(defun jj--run-command (args)
  "Run a jj command with ARGS from the project root.
ARGS can be a list of arguments or a nested list structure.
Nested lists are flattened using `flatten-tree'.

Returns a list: (success-flag stdout stderr exit-code)
where success-flag is t if exit-code is 0, nil otherwise.

Example:
  (jj--run-command '(\"status\"))
  (jj--run-command (list \"log\" \"-r\" my-revset))"
  (let* ((default-directory (jj--get-project-folder))
         (stdout-buffer (generate-new-buffer " *jj-stdout*"))
         (stderr-buffer (generate-new-buffer " *jj-stderr*"))
         (exit-code nil)
         (stdout "")
         (stderr "")
         ;; Flatten nested lists and prepend global args
         (cmd-args (append '("--no-pager" "--color" "never")
                           (flatten-tree args))))
    (jj--debug-log "Command: jj %s" (string-join cmd-args " "))
    (unwind-protect
        (progn
          ;; Call call-process with explicit argument list to avoid apply issues
          (setq exit-code
                (let ((destination (cons stdout-buffer stderr-buffer)))
                  (apply #'call-process
                         (append (list "jj" nil destination nil)
                                 cmd-args))))
          (setq stdout (with-current-buffer stdout-buffer (buffer-string)))
          (setq stderr (with-current-buffer stderr-buffer (buffer-string)))
          (jj--debug-log "Exit code: %d (%s)" exit-code (if (zerop exit-code) "success" "failure"))
          (when (not (string-empty-p stderr))
            (jj--debug-log "Stderr: %s" stderr))
          (list (zerop exit-code) stdout stderr exit-code))
      (kill-buffer stdout-buffer)
      (kill-buffer stderr-buffer))))

;;; Error Handling Functions

(defmacro jj--with-command (command &rest body)
  "Execute jj COMMAND and handle result with BODY on success.
Automatically validates repository, runs command, destructures result,
and handles errors. BODY is executed with stdout, stderr, and exit-code
bound when command succeeds."
  (declare (indent 1))
  `(progn
     (jj--validate-repository)
     (let* ((result (jj--run-command ,command))
            (success (car result))
            (stdout (cadr result))
            (stderr (caddr result))
            (exit-code (cadddr result)))
       (if success
           (progn ,@body)
         (jj--handle-command-error ,command exit-code stderr stdout)))))

(defun jj--validate-repository ()
  "Validate that the current directory is within a jj repository.
Returns the project folder path if valid.
Signals `user-error' if not in a jj repository."
  (let ((project-folder (jj--get-project-folder)))
    (unless project-folder
      (jj--debug-log "Repository validation failed: not in a jj repository")
      (user-error "Not in a jj repository"))
    project-folder))

(defun jj--write-error-buffer (command exit-code stderr stdout)
  "Write detailed error context to the error buffer.
COMMAND is the jj command that failed (can be string or list).
EXIT-CODE is the exit code returned by the command.
STDERR is the stderr output from the command.
STDOUT is the stdout output from the command."
  (let ((error-buffer (get-buffer-create jj-error-buffer-name))
        (command-str (if (listp command)
                         (string-join (flatten-tree command) " ")
                       command)))
    (with-current-buffer error-buffer
      (goto-char (point-max))
      (insert (format "===============================================================================\n"))
      (insert (format "Error at: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format "===============================================================================\n\n"))
      (insert (format "Command: jj %s\n" command-str))
      (insert (format "Exit Code: %d\n\n" exit-code))
      (insert (format "--- Stderr ---\n%s\n" (if (string-empty-p stderr) "<empty>" stderr)))
      (insert (format "--- Stdout ---\n%s\n" (if (string-empty-p stdout) "<empty>" stdout)))
      (insert (format "===============================================================================\n\n")))))

(defun jj--handle-command-error (command exit-code stderr stdout)
  "Handle command errors by categorizing and signaling appropriate error type.
COMMAND is the jj command that failed (can be string or list).
EXIT-CODE is the exit code returned by the command.
STDERR is the stderr output from the command.
STDOUT is the stdout output from the command.

Error categorization:
- User errors (exit codes 1-2 or \"invalid\" in stderr): signals `user-error'
- Command failures (exit codes 1-255): signals `error'
- System errors (binary not found): signals `error'"
  ;; Log error handling
  (let ((command-str (if (listp command)
                         (string-join (flatten-tree command) " ")
                       command)))
    (jj--debug-log "Handling command error: command=%s exit-code=%d" command-str exit-code)

    ;; Write error context to buffer before signaling
    (jj--write-error-buffer command exit-code stderr stdout)

    ;; Categorize and signal appropriate error
    (cond
     ;; User errors: exit codes 1-2 or "invalid" in stderr
     ((or (and (>= exit-code 1) (<= exit-code 2))
          (string-match-p "invalid" stderr))
      (jj--debug-log "Error type: user-error")
      (user-error "jj command failed: %s (exit code %d)" command-str exit-code))

     ;; Command failures: all other non-zero exit codes
     (t
      (jj--debug-log "Error type: command-failure")
      (error "jj command failed: %s (exit code %d)" command-str exit-code)))))

;;; Status Buffer Command Execution Functions

(defun jj-status--fetch-revision-list ()
  "Fetch revision list from jj log with graph output.
Executes: jj log --revisions \"immutable_heads()..@\"
with custom template for change_id, description, and bookmarks.
Returns raw command output string with graph (graph is shown by default).
The graph symbols appear on the left of each line."
  (jj--with-command '("log" "--revisions" "immutable_heads()..@" "-T" "change_id ++ ' | ' ++ description.first_line() ++ ' | ' ++ bookmarks")
    stdout))

(defun jj-status--fetch-working-copy-status ()
  "Fetch working copy status from jj status command.
Executes: jj status
Returns raw command output string."
  (jj--with-command '("status")
    stdout))

(defun jj-status--fetch-bookmark-list ()
  "Fetch bookmark list from jj bookmark command.
Executes: jj bookmark list (uses default output format).
Returns raw command output string.
Default format: name: change_id commit_id description"
  (jj--with-command '("bookmark" "list")
    stdout))


;;; Status Buffer Parsing Functions
;; Task Group 2: Output Parsing & Data Structures

(defun jj-status--parse-log-output (output)
  "Parse jj log OUTPUT with graph into list of revision plists.

INPUT: Raw jj log output string with graph characters and custom template.
The expected format from jj is:
  GRAPH  CHANGE_ID | DESCRIPTION | BOOKMARKS
  GRAPH  (connector lines like │ or ~)

Each revision is on a single line with pipe-separated fields.
Graph connector lines are skipped.

RETURNS: List of revision plists with structure:
  (:graph-line GRAPH :change-id ID :description DESC :bookmarks LIST)

Example:
  (jj-status--parse-log-output \"@  qpvuntsm | Working copy | main\\n│\\n\")
  => ((:graph-line \"@  \" :change-id \"qpvuntsm\"
       :description \"Working copy\" :bookmarks (\"main\")))"
  (when (and output (not (string-empty-p output)))
    (let ((lines (split-string output "\n" t))
          (revisions '()))
      ;; Parse each line
      (dolist (line lines)
        ;; Match lines with change IDs (contain alphanumeric after graph symbols)
        ;; Format: GRAPH  CHANGEID | DESCRIPTION | BOOKMARKS
        (when (string-match "\\`\\([^a-z]*\\)\\([a-z0-9]+\\) |\\([^|]*\\)|\\(.*\\)\\'" line)
          (let* ((graph (match-string 1 line))
                 (change-id (match-string 2 line))
                 (description-raw (match-string 3 line))
                 (bookmarks-raw (match-string 4 line))
                 (description (if description-raw (string-trim description-raw) ""))
                 (bookmarks-str (if bookmarks-raw (string-trim bookmarks-raw) ""))
                 (bookmarks (if (string-empty-p bookmarks-str)
                                nil
                              (split-string bookmarks-str " " t))))
            (push (list :graph-line graph
                        :change-id change-id
                        :description description
                        :bookmarks bookmarks)
                  revisions))))
      ;; Return revisions in correct order (they were pushed in reverse)
      (nreverse revisions))))

(defun jj-status--parse-status-output (output)
  "Parse jj status OUTPUT into list of file plists.

INPUT: Raw jj status output string showing file changes.
Expected format:
  Working copy changes:
  A file1.txt
  M file2.txt
  ...

RETURNS: List of file plists with structure:
  (:path PATH :status STATUS)

Status indicators: A (added), M (modified), R (removed), ? (untracked)

Example:
  (jj-status--parse-status-output \"Working copy changes:\\nM  test.txt\\n\")
  => ((:path \"test.txt\" :status \"M\"))"
  (when (and output (not (string-empty-p output)))
    (let ((lines (split-string output "\n" t))
          (files '())
          (in-changes-section nil))
      (dolist (line lines)
        (cond
         ;; Mark when we enter the changes section
         ((string-match-p "Working copy changes:" line)
          (setq in-changes-section t))

         ;; Parse file status lines (STATUS PATH)
         ((and in-changes-section
               (string-match "\\`\\([AMRI?]\\) \\(.+\\)\\'" line))
          (let ((status (match-string 1 line))
                (path (match-string 2 line)))
            (push (list :path path :status status) files)))))

      ;; Return files in correct order
      (nreverse files))))

(defun jj-status--parse-bookmark-output (output)
  "Parse jj bookmark list OUTPUT into list of bookmark plists.

INPUT: Raw jj bookmark list output with default format.
Expected format:
  bookmark1: change_id1 commit_id1 description1
  bookmark2: change_id2 commit_id2 description2
  ...

RETURNS: List of bookmark plists with structure:
  (:name NAME :change-id CHANGE-ID)

Example:
  (jj-status--parse-bookmark-output \"main: qpvuntsm abc123 description\\n\")
  => ((:name \"main\" :change-id \"qpvuntsm\"))"
  (when (and output (not (string-empty-p output)))
    (let ((lines (split-string output "\n" t))
          (bookmarks '()))
      (dolist (line lines)
        ;; Match: name: change_id commit_id description
        (when (string-match "\\`\\([^:]+\\): \\([a-z0-9]+\\) " line)
          (let ((name (match-string 1 line))
                (change-id (match-string 2 line)))
            (push (list :name name :change-id change-id) bookmarks))))

      ;; Return bookmarks in correct order
      (nreverse bookmarks))))

(defun jj-status--determine-unique-prefix (change-id)
  "Determine unique prefix for CHANGE-ID by querying jj.

INPUT: Full change ID string (e.g., \"qpvuntsmqxuquz57\")

RETURNS: Plist with structure:
  (:unique-prefix PREFIX :full-id CHANGE-ID)

The function queries jj using the template 'shortest(change_id)' to determine
the shortest unique prefix. Falls back to first 8 characters if query fails.

Example:
  (jj-status--determine-unique-prefix \"qpvuntsmqxuquz57\")
  => (:unique-prefix \"qpvuntsm\" :full-id \"qpvuntsmqxuquz57\")"
  (let* ((result (jj--run-command (list "log" "-r" change-id "-T" "shortest(change_id)" "--no-graph")))
         (success (car result))
         (stdout (cadr result)))
    (if success
        (list :unique-prefix (string-trim stdout)
              :full-id change-id)
      ;; Fallback to first 8 characters if query fails
      (list :unique-prefix (substring change-id 0 (min 8 (length change-id)))
            :full-id change-id))))

;;; Status Buffer Rendering Functions
;; Task Group 3: Buffer Rendering System

(defun jj-status--render-section-header (title)
  "Render a section header with TITLE in the current buffer.

Inserts the TITLE with bold face styling and adds a blank line after.
Uses text properties to apply the `jj-status-section-heading' face.

Example:
  (jj-status--render-section-header \"Working Copy Changes\")"
  (insert (propertize title 'face 'jj-status-section-heading))
  (insert "\n\n"))

(defun jj-status--format-change-id (change-id)
  "Format CHANGE-ID with bold unique prefix and grey suffix.

INPUT: Full change ID string (e.g., \"qpvuntsmqxuquz57\")

RETURNS: Propertized string with faces applied:
  - First 8 characters (or unique prefix): bold face
  - Remaining characters: dimmed/grey face

The function determines the unique prefix length by checking if the
change-id is longer than 8 characters. If so, it splits at character 8.

Example:
  (jj-status--format-change-id \"qpvuntsmqxuquz57\")
  => \"qpvuntsm\" (bold) + \"qxuquz57\" (grey)"
  (let* ((prefix-length (min 8 (length change-id)))
         (prefix (substring change-id 0 prefix-length))
         (suffix (if (> (length change-id) prefix-length)
                     (substring change-id prefix-length)
                   "")))
    (concat
     (propertize prefix 'face 'jj-status-change-id-unique)
     (if (not (string-empty-p suffix))
         (propertize suffix 'face 'jj-status-change-id-suffix)
       ""))))

(defun jj-status--render-working-copy (files)
  "Render Working Copy Changes section with FILES list.

INPUT: List of file plists with structure (:path PATH :status STATUS)

Inserts section header \"Working Copy Changes\" followed by file entries.
Each file is formatted as \"  [STATUS]  [path]\" with consistent indentation.
If FILES is empty, displays \"  (no changes)\" instead.

This function marks each file entry with the `jj-item' text property for
navigation support.

Example:
  (jj-status--render-working-copy
    '((:path \"file.txt\" :status \"M\")
      (:path \"new.el\" :status \"A\")))"
  (jj-status--render-section-header "Working Copy Changes")
  (if files
      (dolist (file files)
        (let ((status (plist-get file :status))
              (path (plist-get file :path))
              (start (point)))
          (insert (format "  %s  %s\n" status path))
          ;; Mark item bounds for navigation
          (jj-status--mark-item-bounds start (point) file)))
    (insert "  (no changes)\n"))
  (insert "\n"))

(defun jj-status--render-revisions (revisions)
  "Render Revisions section with REVISIONS list.

INPUT: List of revision plists with structure:
  (:graph-line GRAPH :change-id ID :description DESC :bookmarks LIST)

Inserts section header \"Revisions (immutable_heads()..@)\" followed by
revision entries. Each revision displays:
  - Graph line prefix (e.g., \"@  \", \"◉  \", \"│  \")
  - Formatted change ID with bold/grey styling
  - Description (or \"no description set\")
  - Bookmarks in [bracket] format if present

Maintains graph alignment using monospace spacing.

This function marks each revision entry with the `jj-item' text property for
navigation support.

Example:
  (jj-status--render-revisions
    '((:graph-line \"@  \" :change-id \"qpvuntsm\"
       :description \"Working copy\" :bookmarks (\"main\"))))"
  (jj-status--render-section-header "Revisions (immutable_heads()..@)")
  (if revisions
      (dolist (revision revisions)
        (let ((graph (plist-get revision :graph-line))
              (change-id (plist-get revision :change-id))
              (description (plist-get revision :description))
              (bookmarks (plist-get revision :bookmarks))
              (start (point)))
          ;; Insert graph prefix with face
          (insert (propertize graph 'face 'jj-status-graph))
          ;; Insert formatted change ID
          (insert (jj-status--format-change-id change-id))
          (insert "  ")
          ;; Insert description
          (insert description)
          ;; Insert bookmarks if present
          (when bookmarks
            (insert " ")
            (dolist (bookmark bookmarks)
              (insert (format "[%s]" bookmark))
              (insert " ")))
          (insert "\n")
          ;; Mark item bounds for navigation
          (jj-status--mark-item-bounds start (point) revision)))
    (insert "  (no revisions)\n"))
  (insert "\n"))

(defun jj-status--render-bookmarks (bookmarks)
  "Render Bookmarks section with BOOKMARKS list.

INPUT: List of bookmark plists with structure:
  (:name NAME :change-id CHANGE-ID)

Inserts section header \"Bookmarks\" followed by bookmark entries.
Each bookmark is formatted as \"  [name]  → [change-id]\" with arrow separator.
If BOOKMARKS is empty, displays \"  (no bookmarks)\" instead.

Example:
  (jj-status--render-bookmarks
    '((:name \"main\" :change-id \"qpvuntsm\")
      (:name \"dev\" :change-id \"yqosqzyt\")))"
  (jj-status--render-section-header "Bookmarks")
  (if bookmarks
      (dolist (bookmark bookmarks)
        (let ((name (plist-get bookmark :name))
              (change-id (plist-get bookmark :change-id)))
          (insert (format "  %s  → %s\n" name change-id))))
    (insert "  (no bookmarks)\n"))
  (insert "\n"))

(defun jj-status--render-buffer (revisions files bookmarks)
  "Render complete status buffer with REVISIONS, FILES, and BOOKMARKS.

Main rendering coordinator function. Takes parsed data structures and
renders them into the current buffer in the following order:
  1. Buffer title: \"jj: [project-name]\"
  2. Working Copy Changes section
  3. Revisions section
  4. Bookmarks section

Manages buffer read-only state: disables it during rendering, then
re-enables it after completion.

INPUT:
  REVISIONS - List of revision plists from jj-status--parse-log-output
  FILES - List of file plists from jj-status--parse-status-output
  BOOKMARKS - List of bookmark plists from jj-status--parse-bookmark-output

The function clears existing buffer content before rendering.

Example:
  (jj-status--render-buffer
    '((:graph-line \"@  \" :change-id \"qpvuntsm\"
       :description \"Working copy\" :bookmarks nil))
    '((:path \"file.txt\" :status \"M\"))
    '((:name \"main\" :change-id \"qpvuntsm\")))"
  (let ((inhibit-read-only t))
    (erase-buffer)
    ;; Insert buffer title
    (insert (propertize (format "jj: %s\n\n" (jj--get-project-name))
                        'face 'jj-status-section-heading))
    ;; Render sections in order
    (jj-status--render-working-copy files)
    (jj-status--render-revisions revisions)
    (jj-status--render-bookmarks bookmarks)))

;;; Status Buffer Navigation Functions
;; Task Group 4: Navigation System

(defun jj-status--mark-item-bounds (start end item-plist)
  "Mark text between START and END with jj-item text property.
ITEM-PLIST is the file or revision plist to associate with this region."
  (put-text-property start end 'jj-item item-plist))

(defun jj-status--item-at-point ()
  "Return plist identifying item under cursor.
Returns (:type 'file/:revision/nil :data plist)."
  (let ((item (get-text-property (point) 'jj-item)))
    (cond
     ((and item (plist-get item :path))
      (list :type 'file :data item))
     ((and item (plist-get item :change-id))
      (list :type 'revision :data item))
     (t
      (list :type nil :data nil)))))

(defun jj-status-next-item ()
  "Move point to next file or revision.
Bound to n key in jj-status-mode."
  (interactive)
  (let ((start-pos (point))
        (found nil))
    (forward-line 1)
    (while (and (not found) (not (eobp)))
      (if (get-text-property (point) 'jj-item)
          (setq found t)
        (forward-line 1)))
    (when (and (not found) (eobp))
      (goto-char (point-min))
      (while (and (not found) (< (point) start-pos))
        (if (get-text-property (point) 'jj-item)
            (setq found t)
          (forward-line 1))))
    (unless found
      (goto-char start-pos))))

(defun jj-status-prev-item ()
  "Move point to previous file or revision.
Bound to p key in jj-status-mode."
  (interactive)
  (let ((start-pos (point))
        (found nil))
    (forward-line -1)
    (while (and (not found) (not (bobp)))
      (if (get-text-property (point) 'jj-item)
          (setq found t)
        (forward-line -1)))
    (when (and (not found) (bobp))
      (goto-char (point-max))
      (while (and (not found) (> (point) start-pos))
        (if (get-text-property (point) 'jj-item)
            (setq found t)
          (forward-line -1))))
    (unless found
      (goto-char start-pos))))

(defun jj-status-show-diff ()
  "Show diff for file or revision at point.
Bound to RET key in jj-status-mode.
Currently displays a placeholder message."
  (interactive)
  (let* ((item-info (jj-status--item-at-point))
         (item-type (plist-get item-info :type)))
    (cond
     ((eq item-type 'file)
      (message "Diff viewing: coming in roadmap item #5"))
     ((eq item-type 'revision)
      (message "Diff viewing: coming in roadmap item #5"))
     (t
      (message "No item at point")))))

;;; Status Buffer File Staging Functions
;; Task Group 5: File Staging System

(defun jj-status--find-last-described-revision (revisions)
  "Find most recent revision where description is not empty.

INPUT: List of revision plists from jj-status--parse-log-output

RETURNS: Revision plist or nil if no described revision found.

Iterates from top of list (most recent), skips the working copy revision
(marked with @ in graph-line), and returns the first revision with a
non-empty description.

Example:
  (jj-status--find-last-described-revision
    '((:graph-line \"@  \" :change-id \"abc\" :description \"\")
      (:graph-line \"○  \" :change-id \"def\" :description \"Add feature\")))
  => (:graph-line \"○  \" :change-id \"def\" :description \"Add feature\")"
  (when revisions
    (cl-loop for revision in revisions
             for graph = (plist-get revision :graph-line)
             for description = (plist-get revision :description)
             ;; Skip working copy (@) and revisions with empty or placeholder descriptions
             when (and (not (string-match-p "@" graph))
                       (not (string-empty-p description))
                       (not (string-match-p "(no description set)" description)))
             return revision)))

(defun jj-status--validate-staging-target (change-id)
  "Check if CHANGE-ID is mutable (not immutable).

INPUT: Revision change ID string

RETURNS: t if mutable, nil if immutable

Queries jj for immutable revisions and checks if CHANGE-ID is in the list.
Uses `jj--with-command' for consistent error handling.

Example:
  (jj-status--validate-staging-target \"abc123\")
  => t  ; if abc123 is mutable"
  (let* ((result (jj--run-command '("log" "-r" "immutable_heads()" "-T" "change_id" "--no-graph")))
         (success (car result))
         (stdout (cadr result)))
    (if success
        (let ((immutable-ids (split-string (string-trim stdout) "\n" t)))
          (not (member change-id immutable-ids)))
      ;; If query fails, assume mutable (safer for user)
      t)))

(defun jj-status-refresh ()
  "Refresh the status buffer with current repository state.
Bound to g key in jj-status-mode.

Re-fetches repository data and re-renders the status buffer.
Preserves cursor position by remembering the item at point."
  (interactive)
  (jj--validate-repository)
  ;; Save cursor position by remembering the item at point
  (let ((saved-item (get-text-property (point) 'jj-item))
        (saved-line (line-number-at-pos)))
    ;; Fetch data from jj
    (let ((revision-output (jj-status--fetch-revision-list))
          (status-output (jj-status--fetch-working-copy-status))
          (bookmark-output (jj-status--fetch-bookmark-list)))
      ;; Parse outputs into data structures
      (let ((revisions (jj-status--parse-log-output revision-output))
            (files (jj-status--parse-status-output status-output))
            (bookmarks (jj-status--parse-bookmark-output bookmark-output)))
        ;; Re-render current buffer
        (jj-status--render-buffer revisions files bookmarks)
        ;; Update parsed data for navigation and staging
        (setq-local jj-status--parsed-data (list :revisions revisions
                                                  :files files
                                                  :bookmarks bookmarks))
        ;; Restore cursor position
        (when saved-item
          (goto-char (point-min))
          (let ((found nil))
            ;; Try to find the same item in the refreshed buffer
            (while (and (not found) (not (eobp)))
              (let ((current-item (get-text-property (point) 'jj-item)))
                (when (equal current-item saved-item)
                  ;; Move to beginning of line for consistency
                  (beginning-of-line)
                  (setq found t))
                (unless found
                  (forward-char))))
            ;; If item not found, go back to the original line (or close to it)
            (unless found
              (goto-char (point-min))
              (forward-line (1- saved-line)))))
        (message "Status refreshed")))))

(defun jj-status-stage-file ()
  "Stage file at point to last described revision.

Interactive command bound to s key in jj-status-mode.

Workflow:
  1. Get item at point using `jj-status--item-at-point'
  2. Error if not on a file
  3. Find last described revision from buffer-local data
  4. Error if no described revision exists
  5. Validate target is mutable
  6. Error if target is immutable
  7. Execute: jj squash --from @ --into [target-change-id] [filename]
  8. Display success message
  9. Trigger buffer refresh

Error messages:
  - \"Not on a file\" if cursor not on a file item
  - \"No described revision found\" if no described revision exists
  - \"Cannot stage to immutable revision\" if target is immutable"
  (interactive)
  (let* ((item-info (jj-status--item-at-point))
         (item-type (plist-get item-info :type))
         (item-data (plist-get item-info :data)))
    ;; Error if not on a file
    (unless (eq item-type 'file)
      (user-error "Not on a file"))

    ;; Get buffer-local revisions
    (let* ((parsed-data (buffer-local-value 'jj-status--parsed-data (current-buffer)))
           (revisions (plist-get parsed-data :revisions))
           (target-revision (jj-status--find-last-described-revision revisions)))
      ;; Error if no described revision
      (unless target-revision
        (user-error "No described revision found"))

      (let ((target-change-id (plist-get target-revision :change-id))
            (file-path (plist-get item-data :path)))
        ;; Validate target is mutable
        (unless (jj-status--validate-staging-target target-change-id)
          (user-error "Cannot stage to immutable revision"))

        ;; Execute squash command
        (jj--with-command (list "squash" "--from" "@" "--into" target-change-id file-path)
          (let ((prefix (substring target-change-id 0 (min 8 (length target-change-id)))))
            (message "Staged %s to %s" file-path prefix))
          (jj-status-refresh))))))

;;; User-Facing Commands

(defun jj-status ()
  "Display the status of the current jj repository with magit-like interface."
  (interactive)
  (jj--validate-repository)
  ;; Fetch data from jj
  (let ((revision-output (jj-status--fetch-revision-list))
        (status-output (jj-status--fetch-working-copy-status))
        (bookmark-output (jj-status--fetch-bookmark-list)))
    ;; Parse outputs into data structures
    (let ((revisions (jj-status--parse-log-output revision-output))
          (files (jj-status--parse-status-output status-output))
          (bookmarks (jj-status--parse-bookmark-output bookmark-output)))
      ;; Render buffer
      (let ((buffer (get-buffer-create (format "jj: %s" (jj--get-project-name)))))
        (with-current-buffer buffer
          ;; Enable mode FIRST if not already enabled
          (unless (eq major-mode 'jj-status-mode)
            (jj-status-mode))
          ;; Render buffer with text properties (inhibit-read-only protects against mode's read-only setting)
          (jj-status--render-buffer revisions files bookmarks)
          ;; Store parsed data AFTER mode is enabled
          (setq jj-status--parsed-data (list :revisions revisions
                                              :files files
                                              :bookmarks bookmarks)))
        (switch-to-buffer buffer)))))


(defun jj-status-describe (args)
  "Run jj describe with ARGS."
  (interactive (list (transient-args 'jj-status-describe-popup)))
  (jj--with-command (cons "describe" args)
    (jj-status)))

(transient-define-prefix jj-status-describe-popup ()
  "Popup for jujutsu describe comand."
  ["Options"
   ("-m" "Message" "-m" :reader (lambda (&rest _args) (read-string "Message: ")))]
  ["Actions"
   ("d" "Describe" jj-status-describe)])

(defun jj--log-count-revs (revset)
  "Count number of revisions in log for REVSET."
  (jj--with-command (list "log" "-T" "\"a\"" "--revisions" revset "--no-graph")
    (length stdout)))

(defun jj--status-abandon-revset-from-trunk ()
  "Prompt for branch name and create revset from trunk."
  (interactive)
  (jj--validate-repository)
  (let* ((jj-current-revset
          (format "trunk()..%s"
                  (jj--bookmarks-select)))
         (revision-count (jj--log-count-revs jj-current-revset)))
    (if (y-or-n-p (format "Abandon %d revisions? " revision-count))
        (jj-status-abandon (list jj-current-revset)))))

(defun jj--bookmarks-get ()
  "Get list of bookmarks from jj repository."
  (jj--with-command '("bookmark" "list" "-T" "name ++ \"\\n\"")
    (s-split "\n" stdout 't)))

(defun jj--bookmarks-select ()
  "Select bookmark from jj."
  (let ((bookmark (completing-read "Select branch: " (jj--bookmarks-get))))
    bookmark))

(defun jj-status-abandon (args)
  "Run jj abandon with ARGS."
  (interactive (list (transient-args 'jj-status-abandon-popup)))
  (jj--with-command (cons "abandon" args)
    (jj-status)))

(transient-define-prefix jj-status-abandon-popup ()
  "Popup for jujutsu abandon comand."
  ["Actions"
   ("b" "Abandon branch from trunk" jj--status-abandon-revset-from-trunk)])

(defun jj--log (args)
  "Run jj log with ARGS."
  (interactive (list (transient-args 'jj-status-log-popup)))
  (jj--log-show (cons "log" args)))

(defun jj--log-show (args)
  "Show log for given ARGS."
  (interactive)
  (jj--with-command args
    (let ((buffer (get-buffer-create (format "jj log: %s" (jj--get-project-name)))))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert stdout)
          (jj-status-mode)))
      (switch-to-buffer buffer))))

(defun jj--revset-read (&rest _args)
  "Read revset from user."
  (read-string "-r "))

(transient-define-prefix jj-status-log-popup ()
  "Popup for jujutsu log command"
  :value '("-n=256")
  ["Options"
   ("-r" "Revisions" "--revisions=" :reader jj--revset-read)
   ("-n" "Number of revisions to show" "-n=" :reader (lambda (&rest _args) (read-string "-n ")))
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
  (jj--with-command (append '("new") (transient-scope) args)
    (jj-status)))

(transient-define-prefix jj-status-new-popup ()
  "Popup for jujutsu new command."
  ["Parent revisions"
   (:info #'jj--new-parent-revisions-display)
   ("a" "Add parent revision" jj--new-parent-revisions-add :transient t)
   ("c" "Clear parent revisions" jj--new-parent-revisions-clear :transient t)
   ]

  ["Options"
   ("-m" "Message" "-m" :reader (lambda (&rest _args) (read-string "Message: ")))
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
  (jj--with-command (append '("git" "fetch") args)
    (message "Successfully fetched")
    (jj-status)))

(transient-define-prefix jj-fetch-popup ()
  "Popup for jujutsu git fetch command"
  :value '("--branch=glob:*")
  ["Options"
   ("-b" "Fetch only some branches" "--branch" :reader (lambda (&rest _args) (read-string "--branch ")))
   ("-r" "The remote to fetch from" "--remote" :reader (lambda (&rest _args) (read-string "--remote "))) ;;can be repeated
   ("-a" "Fetch from all remotes" "--all-remotes")
   ]
  ["Actions"
   ("f" "Fetch" jj--fetch)])

(defun jj--push (args)
  "Run jj push with ARGS."
  (interactive (list (transient-args 'jj-push-popup)))
  (jj--with-command (append '("git" "push") args)
    (jj-status)))

(transient-define-prefix jj-push-popup ()
  "Popup for jujutsu git push command"
  ["Options"
   ("-b" "Push only this bookmark or bookmarks matchin a pattern" "--bookmark=" :reader (lambda (&rest _args) (read-string "--bookmark "))) ;;can be repeated
   ("-c" "Push this commit by creating a bookmark based on its change ID" "--change=" :reader (lambda (&rest _args) (read-string "--change "))) ;;can be repeated
   ("-r" "The remote to push to" "--remote=" :reader (lambda (&rest _args) (read-string "--remote ")))
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
  "Popup for jujutsu actions in the status buffer.
Use n/p to navigate between items, s to stage files, g to refresh."
  ["Navigation"
   ("n" "Next item" jj-status-next-item)
   ("p" "Previous item" jj-status-prev-item)]
  ["File Operations"
   ("s" "Stage file" jj-status-stage-file)
   ("g" "Refresh" jj-status-refresh)]
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
(define-key jj-status-mode-map (kbd "n") #'jj-status-next-item)
(define-key jj-status-mode-map (kbd "p") #'jj-status-prev-item)
(define-key jj-status-mode-map (kbd "RET") #'jj-status-show-diff)
(define-key jj-status-mode-map (kbd "s") #'jj-status-stage-file)
(define-key jj-status-mode-map (kbd "g") #'jj-status-refresh)

;;; jj.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
