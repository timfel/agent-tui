;;; agent-tui-fanout.el --- Fan out agent-tui sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; Create one agent TUI per task, normally in a Git worktree.  This contains
;; only worktree and launch orchestration; provider-specific session handling
;; stays in the provider package.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'agent-tui)

(defgroup agent-tui-fanout nil
  "Fan out `agent-tui' sessions."
  :group 'agent-tui)

(defcustom agent-tui-fanout-provider nil
  "Provider symbol used by fan-out sessions.

When nil, use the global `agent-tui-provider'."
  :type '(choice (const :tag "Global provider" nil) symbol)
  :group 'agent-tui-fanout)

(defcustom agent-tui-fanout-worktree-directory ".agent-tui/worktrees"
  "Directory, relative to a repository root, for fan-out worktrees."
  :type 'directory
  :group 'agent-tui-fanout)

(defcustom agent-tui-fanout-planning-request "Go into planning mode"
  "Text prefixed to each fan-out task.

Set this to an empty string to send only the task text."
  :type 'string
  :group 'agent-tui-fanout)

(defcustom agent-tui-fanout-startup-delay 3
  "Seconds to wait before sending a newly started TUI its task."
  :type 'number
  :group 'agent-tui-fanout)

(defcustom agent-tui-fanout-worktree-cleanup-age-days 14
  "Offer to delete fan-out folders older than this many days.

Set to nil to disable stale worktree cleanup."
  :type '(choice (const :tag "Disable cleanup" nil) integer)
  :group 'agent-tui-fanout)

(defcustom agent-tui-fanout-adjacent-repository-names nil
  "Sibling repository names to include in each fan-out worktree."
  :type '(repeat string)
  :group 'agent-tui-fanout)

(defcustom agent-tui-fanout-repositories-function
  #'agent-tui-fanout-default-repositories
  "Function called with a repository root and returning repository roots."
  :type 'function
  :group 'agent-tui-fanout)

(defvar-local agent-tui-fanout-worktree-parent nil
  "Parent folder for the current fan-out worktree set.")

(defun agent-tui-fanout--parent-directory (directory)
  "Return DIRECTORY's parent, or nil at the filesystem root."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (parent (file-name-directory (directory-file-name directory))))
    (unless (or (null parent) (equal parent directory))
      (file-name-as-directory parent))))

(defun agent-tui-fanout-default-repositories (repo-root)
  "Return REPO-ROOT and configured adjacent repositories."
  (let* ((repo-root (file-name-as-directory (expand-file-name repo-root)))
         (parent (agent-tui-fanout--parent-directory repo-root)))
    (cons repo-root
          (seq-filter
           (lambda (path)
             (and (file-directory-p path)
                  (not (file-equal-p repo-root path))))
           (mapcar (lambda (name) (expand-file-name name parent))
                   agent-tui-fanout-adjacent-repository-names)))))

(defun agent-tui-fanout--repo-root (directory)
  "Return the Git root for DIRECTORY, or nil."
  (let ((default-directory (file-name-as-directory
                            (expand-file-name directory))))
    (with-temp-buffer
      (when (zerop (process-file "git" nil t nil "rev-parse" "--show-toplevel"))
        (file-name-as-directory (string-trim (buffer-string)))))))

(defun agent-tui-fanout--base-ref (repo-root)
  "Return a usable base ref for REPO-ROOT."
  (let ((default-directory (file-name-as-directory repo-root)))
    (seq-some
     (lambda (ref)
       (when (zerop
              (if (equal ref "HEAD")
                  (process-file "git" nil nil nil "rev-parse" "--verify"
                                "--quiet" ref)
                (process-file "git" nil nil nil "show-ref" "--verify"
                              "--quiet" ref)))
         ref))
     '("refs/remotes/origin/master"
       "refs/remotes/origin/main"
       "refs/heads/master"
       "refs/heads/main"
       "HEAD"))))

(defun agent-tui-fanout--base-directory (repo-root)
  "Return the fan-out base directory for REPO-ROOT."
  (file-name-as-directory
   (expand-file-name agent-tui-fanout-worktree-directory repo-root)))

(defun agent-tui-fanout-worktree-folder-p (directory)
  "Return non-nil when DIRECTORY uses the configured fan-out layout."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (base (agent-tui-fanout--parent-directory directory))
         (relative (unless (file-name-absolute-p
                            agent-tui-fanout-worktree-directory)
                     agent-tui-fanout-worktree-directory))
         (root base))
    (cond
     ((file-name-absolute-p agent-tui-fanout-worktree-directory)
      (and base
           (file-equal-p base
                         (file-name-as-directory
                          (expand-file-name
                           agent-tui-fanout-worktree-directory)))))
     ((and base relative)
      (dotimes (_ (length (split-string relative "/" t)))
        (setq root (and root
                        (agent-tui-fanout--parent-directory root))))
      (and root
           (file-equal-p base (agent-tui-fanout--base-directory root)))))))

(defun agent-tui-fanout--buffer-in-directory-p (directory)
  "Return non-nil when an active TUI is rooted below DIRECTORY."
  (seq-some
   (lambda (buffer)
     (file-in-directory-p (agent-tui-cwd buffer)
                          (file-name-as-directory directory)))
   (agent-tui-buffers)))

(defun agent-tui-fanout--cleanup-stale-worktrees (base-directory)
  "Offer to remove stale child folders below BASE-DIRECTORY."
  (when (and agent-tui-fanout-worktree-cleanup-age-days
             (file-directory-p base-directory))
    (let ((cutoff (time-subtract
                   (current-time)
                   (days-to-time agent-tui-fanout-worktree-cleanup-age-days))))
      (dolist (directory
               (seq-filter #'file-directory-p
                           (directory-files
                            base-directory t directory-files-no-dot-files-regexp)))
        (when (and (time-less-p
                    (file-attribute-modification-time
                     (file-attributes directory))
                    cutoff)
                   (not (agent-tui-fanout--buffer-in-directory-p directory))
                   (yes-or-no-p
                    (format "Remove stale agent TUI worktree %s? " directory)))
          (delete-directory directory t nil))))))

(defun agent-tui-fanout--slug (title)
  "Return a filesystem-safe slug for TITLE."
  (let* ((title (if (string-match "\\`[A-Z][A-Z]+-[0-9]+\\b" title)
                    (match-string 0 title)
                  title))
         (slug (downcase title)))
    (setq slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
    (setq slug (replace-regexp-in-string "\\`-+\\|-+\\'" "" slug))
    (if (string-empty-p slug) "task" slug)))

(defun agent-tui-fanout--worktree-create
    (repo-root parent-folder branch base-ref)
  "Create or reuse a worktree for REPO-ROOT under PARENT-FOLDER."
  (let ((directory
         (expand-file-name
          (file-name-nondirectory
           (directory-file-name repo-root))
          parent-folder)))
    (make-directory parent-folder t)
    (let ((default-directory (file-name-as-directory repo-root)))
      (process-file "git" nil nil nil "worktree" "prune")
      (if (zerop (process-file "git" nil nil nil "worktree" "add" "-b"
                               branch directory base-ref))
          directory
        (when (and (file-directory-p directory)
                   (file-exists-p (expand-file-name ".git" directory))
                   (not (agent-tui-fanout--buffer-in-directory-p directory)))
          directory)))))

(defun agent-tui-fanout--worktrees-create-with-suffix
    (repo-roots base-directory slug suffix base-ref)
  "Create related worktrees for REPO-ROOTS using SLUG and SUFFIX."
  (let* ((name (if suffix (format "%s-%02d" slug suffix) slug))
         (parent-folder (expand-file-name name base-directory))
         (branch (format "agent-tui/%s" name))
         (created
          (seq-filter
           #'cdr
           (mapcar
            (lambda (repo-root)
              (cons repo-root
                    (agent-tui-fanout--worktree-create
                     repo-root parent-folder branch base-ref)))
            repo-roots))))
    (if (= (length created) (length repo-roots))
        (mapcar #'cdr created)
      (dolist (entry created)
        (let ((default-directory (car entry)))
          (process-file "git" nil nil nil "worktree" "remove" "--force"
                        (cdr entry))
          (process-file "git" nil nil nil "branch" "-d" branch)))
      (agent-tui-fanout--worktrees-create-with-suffix
       repo-roots base-directory slug (1+ (or suffix 0)) base-ref))))

(defun agent-tui-fanout--worktrees-create (repo-root title)
  "Create or reuse a fan-out worktree set for TITLE."
  (let* ((repo-root (file-name-as-directory (expand-file-name repo-root)))
         (repo-roots (funcall agent-tui-fanout-repositories-function repo-root))
         (base-directory (agent-tui-fanout--base-directory repo-root))
         (slug (agent-tui-fanout--slug title))
         (base-ref (agent-tui-fanout--base-ref repo-root)))
    (unless base-ref
      (user-error "Could not find a usable Git base ref in %s" repo-root))
    (car
     (agent-tui-fanout--worktrees-create-with-suffix
      repo-roots base-directory slug nil base-ref))))

(defun agent-tui-fanout--initial-request (task)
  "Return the planning-prefixed request for TASK, or nil."
  (let ((task (string-trim (or task "")))
        (planning (string-trim (or agent-tui-fanout-planning-request ""))))
    (cond
     ((string-empty-p task) nil)
     ((string-empty-p planning) task)
     (t (concat planning "\n" task)))))

(defun agent-tui-fanout--send-task (buffer task)
  "Send TASK to BUFFER if it is still a live TUI."
  (when (and (buffer-live-p buffer)
             (agent-tui--active-buffer-p buffer))
    (with-current-buffer buffer
      (agent-tui-enqueue-prompt task))))

(defun agent-tui-fanout--schedule-task (buffer task delay)
  "Schedule TASK for BUFFER after DELAY seconds."
  (when task
    (run-with-timer delay nil #'agent-tui-fanout--send-task buffer task)))

(defun agent-tui-fanout--git-directories (directory)
  "Return DIRECTORY and immediate Git children below it."
  (seq-filter
   (lambda (candidate)
     (file-exists-p (expand-file-name ".git" candidate)))
   (cons (file-name-as-directory directory)
         (seq-filter #'file-directory-p
                     (directory-files directory t
                                      directory-files-no-dot-files-regexp)))))

;;;###autoload
(defun agent-tui-fanout-delete-worktree (directory)
  "Remove Git worktree links below DIRECTORY and delete DIRECTORY."
  (dolist (git-directory (agent-tui-fanout--git-directories directory))
    (when (file-regular-p (expand-file-name ".git" git-directory))
      (process-file "git" nil nil nil "-C" git-directory
                    "worktree" "remove" "--force" git-directory)))
  (when (file-directory-p directory)
    (delete-directory directory t)))

;;;###autoload
(defun agent-tui-fanout-worktrees (task-specs &optional directory provider)
  "Start one TUI per entry in TASK-SPECS, normally in Git worktrees.

TASK-SPECS is an alist of (TITLE . TASK) pairs.  An absolute TITLE is treated
as an existing directory; other titles create or reuse worktrees below
DIRECTORY.  PROVIDER defaults to `agent-tui-fanout-provider' or the global
`agent-tui-provider'."
  (interactive
   (list (list (cons (read-string "Task title: ")
                     (read-string "Initial request: ")))
         (read-directory-name "Repository: " default-directory nil t)))
  (let* ((provider (or provider agent-tui-fanout-provider agent-tui-provider))
         (directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (titles (mapcar #'car task-specs))
         (needs-repository (not (seq-every-p #'file-name-absolute-p titles)))
         (repo-root (and needs-repository
                         (agent-tui-fanout--repo-root directory))))
    (unless provider
      (user-error "No agent-tui provider selected"))
    (when (and needs-repository (not repo-root))
      (user-error "Not inside a Git repository: %s" directory))
    (when (seq-some (lambda (title)
                      (or (not (stringp title)) (string-blank-p title)))
                    titles)
      (user-error "Empty fan-out title"))
    (when needs-repository
      (let ((base-directory (agent-tui-fanout--base-directory repo-root)))
        (make-directory base-directory t)
        (agent-tui-fanout--cleanup-stale-worktrees base-directory)))
    (cl-loop for spec in task-specs
             for index from 0
             for title-or-directory = (car spec)
             for task = (cdr spec)
             do
             (unless (or (null task) (stringp task))
               (user-error "Fan-out task must be a string: %S" task))
             (let* ((absolute (file-name-absolute-p title-or-directory))
                    (title (if absolute
                               (file-name-nondirectory
                                (directory-file-name title-or-directory))
                             title-or-directory))
                    (worktree (if absolute
                                  (file-name-as-directory
                                   (expand-file-name title-or-directory))
                                (agent-tui-fanout--worktrees-create
                                 repo-root title)))
                    (buffer (agent-tui-start-in-directory
                             provider worktree t nil)))
               (with-current-buffer buffer
                 (rename-buffer title t)
                 (setq-local agent-tui-fanout-worktree-parent
                             (unless absolute
                               (agent-tui-fanout--parent-directory worktree))))
               (agent-tui-fanout--schedule-task
                buffer
                (agent-tui-fanout--initial-request task)
                (+ agent-tui-fanout-startup-delay (* index 0.25)))))))

;;;###autoload
(defun agent-tui-fanout-cleanup-worktree ()
  "Delete the current fan-out worktree parent after confirmation."
  (interactive)
  (let ((parent agent-tui-fanout-worktree-parent))
    (unless (and parent (file-directory-p parent))
      (user-error "Current TUI is not a fan-out worktree"))
    (when (yes-or-no-p (format "Delete %s? " parent))
      (dolist (buffer (agent-tui-buffers))
        (when (file-in-directory-p (agent-tui-cwd buffer) parent)
          (kill-buffer buffer)))
      (agent-tui-fanout-delete-worktree parent))))

(provide 'agent-tui-fanout)

;;; agent-tui-fanout.el ends here
