;;; agent-tui-dashboard.el --- Small dashboard for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; A small tabulated dashboard for live agent TUIs and their fan-out
;; worktrees.  It intentionally does not fetch Jira or pull-request metadata.
;;
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'agent-tui)
(require 'agent-tui-fanout)

(defgroup agent-tui-dashboard nil
  "Dashboard for `agent-tui' sessions."
  :group 'agent-tui)

(defcustom agent-tui-dashboard-search-directories
  (list user-emacs-directory)
  "Directories searched for `.agent-shell' markers and worktrees.

Each search directory and its immediate child directories are checked for an
`.agent-shell' marker and `.agent-shell/worktrees'."
  :type '(repeat directory)
  :group 'agent-tui-dashboard)

(defcustom agent-tui-dashboard-provider nil
  "Provider used when starting a TUI from the dashboard.

When nil, use the global `agent-tui-provider'."
  :type '(choice (const :tag "Global provider" nil) symbol)
  :group 'agent-tui-dashboard)

(defcustom agent-tui-dashboard-refresh-interval 2
  "Seconds between automatic dashboard refreshes.

Set to nil to disable automatic refreshing."
  :type '(choice (const :tag "Disable" nil) number)
  :group 'agent-tui-dashboard)

(defconst agent-tui-dashboard--buffer-name "*agent-tui dashboard*")

(defvar-local agent-tui-dashboard--rows nil)
(defvar-local agent-tui-dashboard--refresh-timer nil)

(defun agent-tui-dashboard--directory (directory)
  "Return DIRECTORY in canonical directory form."
  (file-name-as-directory (expand-file-name directory)))

(defun agent-tui-dashboard--parent (directory)
  "Return DIRECTORY's parent, or nil at the filesystem root."
  (let* ((directory (agent-tui-dashboard--directory directory))
         (parent (file-name-directory (directory-file-name directory))))
    (unless (or (null parent) (equal parent directory))
      (agent-tui-dashboard--directory parent))))

(defun agent-tui-dashboard--worktree-folder-p (directory)
  "Return non-nil when DIRECTORY uses the fan-out worktree layout."
  (agent-tui-fanout-worktree-folder-p directory))

(defun agent-tui-dashboard--folder-for-directory (directory)
  "Return the nearest fan-out folder containing DIRECTORY, or DIRECTORY."
  (let ((probe (agent-tui-dashboard--directory directory))
        found)
    (while (and probe (not found))
      (when (agent-tui-dashboard--worktree-folder-p probe)
        (setq found probe))
      (setq probe (agent-tui-dashboard--parent probe)))
    (or found (agent-tui-dashboard--directory directory))))

(defun agent-tui-dashboard--children (directory)
  "Return immediate child directories of DIRECTORY."
  (when (file-directory-p directory)
    (seq-filter #'file-directory-p
                (directory-files directory t
                                 directory-files-no-dot-files-regexp))))

(defun agent-tui-dashboard--configured-folders ()
  "Return configured fan-out and project folders.

Check each configured directory and one level of children, matching the
layout used by the agent-shell dashboard."
  (let (folders)
    (dolist (directory agent-tui-dashboard-search-directories)
      (let* ((root (agent-tui-dashboard--directory directory))
             (candidates (cons root (agent-tui-dashboard--children root))))
        (dolist (candidate candidates)
          (let ((base (agent-tui-fanout--base-directory candidate)))
            (when (file-directory-p base)
              (setq folders
                    (append (agent-tui-dashboard--children base) folders)))
            (when (file-directory-p (expand-file-name ".agent-shell" candidate))
              (push candidate folders))))))
    folders))

(defun agent-tui-dashboard--live-folders ()
  "Return folders inferred from active TUI buffers."
  (mapcar (lambda (buffer)
            (agent-tui-dashboard--folder-for-directory
             (agent-tui-cwd buffer)))
          (agent-tui-buffers)))

(defun agent-tui-dashboard--folders ()
  "Return de-duplicated existing dashboard folders."
  (let ((seen (make-hash-table :test #'equal)) folders)
    (dolist (folder (append (agent-tui-dashboard--live-folders)
                            (agent-tui-dashboard--configured-folders)))
      (when (file-directory-p folder)
        (let ((folder (agent-tui-dashboard--directory folder)))
          (unless (gethash folder seen)
            (puthash folder t seen)
            (push folder folders)))))
    (sort folders #'string-lessp)))

(defun agent-tui-dashboard--buffers (folder)
  "Return active TUI buffers assigned to FOLDER."
  (seq-filter
   (lambda (buffer)
     (and (file-in-directory-p (agent-tui-cwd buffer) folder)
          (file-equal-p
           (agent-tui-dashboard--folder-for-directory (agent-tui-cwd buffer))
           (agent-tui-dashboard--directory folder))))
   (agent-tui-buffers)))

(defun agent-tui-dashboard--status (buffers)
  "Return aggregate status for BUFFERS."
  (cond
   ((seq-some (lambda (buffer)
                (eq (agent-tui-status buffer) 'busy))
              buffers)
    'busy)
   ((seq-some (lambda (buffer)
                (eq (agent-tui-status buffer) 'unknown))
              buffers)
    'unknown)
   (buffers 'ready)
   (t 'none)))

(defun agent-tui-dashboard--git-output (directory &rest args)
  "Return trimmed Git output for DIRECTORY and ARGS, or nil."
  (with-temp-buffer
    (when (zerop (apply #'process-file "git" nil t nil
                        "-C" directory args))
      (string-trim (buffer-string)))))

(defun agent-tui-dashboard--branch (folder)
  "Return a branch name from FOLDER or one of its repositories."
  (seq-some
   (lambda (directory)
     (agent-tui-dashboard--git-output directory
                                      "symbolic-ref" "--quiet" "--short" "HEAD"))
   (cons folder
         (agent-tui-dashboard--children folder))))

(defun agent-tui-dashboard--buffer-title (buffer)
  "Return a clean display name for BUFFER."
  (string-trim (buffer-name buffer) "\\`[ *]+" "[ *]+\\'"))

(defun agent-tui-dashboard--title (folder buffers branch)
  "Return a display title for FOLDER, BUFFERS, and BRANCH."
  (or (when buffers
        (string-join (delete-dups (mapcar #'agent-tui-dashboard--buffer-title
                                          buffers))
                     " | "))
      branch
      (file-name-nondirectory (directory-file-name folder))))

(defun agent-tui-dashboard--icon (status)
  "Return a compact icon for STATUS."
  (pcase status
    ('busy (propertize "τ" 'face 'warning))
    ('ready (propertize "✓" 'face 'success))
    ('unknown (propertize "?" 'face 'warning))
    (_ (propertize "∅" 'face 'shadow))))

(defun agent-tui-dashboard--row (folder)
  "Build a row for FOLDER."
  (let* ((buffers (agent-tui-dashboard--buffers folder))
         (status (agent-tui-dashboard--status buffers))
         (branch (agent-tui-dashboard--branch folder)))
    (list :folder folder
          :buffers buffers
          :status status
          :branch branch
          :title (agent-tui-dashboard--title folder buffers branch))))

(defun agent-tui-dashboard--entry (row)
  "Return a tabulated-list entry for ROW."
  (let ((folder (plist-get row :folder)))
    (list folder
          (vector
           (agent-tui-dashboard--icon (plist-get row :status))
           (abbreviate-file-name (directory-file-name folder))
           (or (plist-get row :branch) "-")
           (plist-get row :title)))))

(defun agent-tui-dashboard--goto-id (id)
  "Return point to row ID when it exists."
  (when id
    (goto-char (point-min))
    (while (and (not (eobp))
                (not (equal (tabulated-list-get-id) id)))
      (forward-line 1))))

(defun agent-tui-dashboard--refresh ()
  "Refresh the dashboard contents."
  (let ((id (tabulated-list-get-id))
        (rows (mapcar #'agent-tui-dashboard--row
                      (agent-tui-dashboard--folders))))
    (setq agent-tui-dashboard--rows
          (mapcar (lambda (row)
                    (cons (plist-get row :folder) row))
                  rows)
          tabulated-list-entries (mapcar #'agent-tui-dashboard--entry rows))
    (tabulated-list-print t)
    (agent-tui-dashboard--goto-id id)))

(defun agent-tui-dashboard--row-at-point ()
  "Return the row at point."
  (let ((row (cdr (assoc (tabulated-list-get-id)
                         agent-tui-dashboard--rows))))
    (or row (user-error "No dashboard row at point"))))

(defun agent-tui-dashboard--choose-buffer (row)
  "Return a live buffer from ROW, prompting when necessary."
  (let ((buffers (seq-filter #'buffer-live-p (plist-get row :buffers))))
    (cond
     ((null buffers) nil)
     ((null (cdr buffers)) (car buffers))
     (t (get-buffer
         (completing-read "Agent TUI buffer: "
                          (mapcar #'buffer-name buffers) nil t))))))

;;;###autoload
(defun agent-tui-dashboard-visit ()
  "Visit the current TUI, starting one when necessary."
  (interactive)
  (let* ((row (agent-tui-dashboard--row-at-point))
         (folder (plist-get row :folder))
         (buffer (agent-tui-dashboard--choose-buffer row))
         (provider (or agent-tui-dashboard-provider agent-tui-provider)))
    (if buffer
        (pop-to-buffer buffer)
      (unless provider
        (user-error "No agent-tui provider selected"))
      (agent-tui-start-in-directory provider folder t nil))))

;;;###autoload
(defun agent-tui-dashboard-rename-buffer ()
  "Rename a live TUI buffer on the current row."
  (interactive)
  (let ((buffer (agent-tui-dashboard--choose-buffer
                 (agent-tui-dashboard--row-at-point))))
    (unless buffer
      (user-error "No live TUI on this row"))
    (with-current-buffer buffer
      (call-interactively #'rename-buffer))
    (agent-tui-dashboard--refresh)))

;;;###autoload
(defun agent-tui-dashboard-find-file ()
  "Find a file below the current row's folder."
  (interactive)
  (let ((default-directory
          (plist-get (agent-tui-dashboard--row-at-point) :folder)))
    (call-interactively #'find-file)))

;;;###autoload
(defun agent-tui-dashboard-delete-worktree ()
  "Delete the current fan-out worktree after confirmation."
  (interactive)
  (let* ((row (agent-tui-dashboard--row-at-point))
         (folder (plist-get row :folder)))
    (unless (agent-tui-dashboard--worktree-folder-p folder)
      (user-error "Refusing to delete non-fan-out folder: %s" folder))
    (when (yes-or-no-p (format "Delete %s? " (abbreviate-file-name folder)))
      (dolist (buffer (plist-get row :buffers))
        (when (buffer-live-p buffer)
          (kill-buffer buffer)))
      (agent-tui-fanout-delete-worktree folder)
      (agent-tui-dashboard--refresh))))

(defvar agent-tui-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'agent-tui-dashboard-visit)
    (define-key map (kbd "g") #'agent-tui-dashboard-refresh)
    (define-key map (kbd "R") #'agent-tui-dashboard-rename-buffer)
    (define-key map (kbd "f") #'agent-tui-dashboard-find-file)
    (define-key map (kbd "C-x f") #'agent-tui-dashboard-find-file)
    (define-key map (kbd "C-x C-f") #'agent-tui-dashboard-find-file)
    (define-key map (kbd "D") #'agent-tui-dashboard-delete-worktree)
    map)
  "Keymap for `agent-tui-dashboard-mode'.")

;;;###autoload
(defun agent-tui-dashboard-refresh ()
  "Refresh the current agent TUI dashboard."
  (interactive)
  (unless (derived-mode-p 'agent-tui-dashboard-mode)
    (user-error "Not in an agent-tui dashboard buffer"))
  (agent-tui-dashboard--refresh))

(defun agent-tui-dashboard--cancel-timer ()
  "Cancel the current dashboard refresh timer."
  (when (timerp agent-tui-dashboard--refresh-timer)
    (cancel-timer agent-tui-dashboard--refresh-timer))
  (setq agent-tui-dashboard--refresh-timer nil))

(define-derived-mode agent-tui-dashboard-mode tabulated-list-mode "Agent-TUI-Dashboard"
  "Major mode for the agent TUI dashboard."
  (setq tabulated-list-format
        [ ("A" 2 nil)
          ("Folder" 72 t)
          ("Branch" 24 t)
          ("Title" 0 t)])
  (setq tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (setq-local header-line-format
              "RET: visit  g: refresh  R: rename  f/C-x f/C-x C-f: find file  D: delete worktree")
  (setq-local revert-buffer-function
              (lambda (_ignore _noconfirm)
                (agent-tui-dashboard--refresh)))
  (add-hook 'kill-buffer-hook #'agent-tui-dashboard--cancel-timer nil t)
  (tabulated-list-init-header)
  (when agent-tui-dashboard-refresh-interval
    (setq agent-tui-dashboard--refresh-timer
          (run-at-time agent-tui-dashboard-refresh-interval
                       agent-tui-dashboard-refresh-interval
                       (lambda (buffer)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (when (derived-mode-p 'agent-tui-dashboard-mode)
                               (agent-tui-dashboard--refresh)))))
                       (current-buffer)))))

;;;###autoload
(defun agent-tui-dashboard ()
  "Open the agent TUI dashboard."
  (interactive)
  (let ((buffer (get-buffer-create agent-tui-dashboard--buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-tui-dashboard-mode)
        (agent-tui-dashboard-mode))
      (agent-tui-dashboard--refresh))
    (pop-to-buffer buffer)))

(provide 'agent-tui-dashboard)

;;; agent-tui-dashboard.el ends here
