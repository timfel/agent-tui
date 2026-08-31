;;; agent-tui-bwrap.el --- Bubblewrap support for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; Run an agent TUI through `systemd-run' and/or `bwrap'.  This is deliberately
;; independent of `agent-shell': the prefix is returned as argv and the core
;; renders it as shell text because agent-tui starts providers in a terminal.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'agent-tui)

(defgroup agent-tui-bwrap nil
  "Bubblewrap support for `agent-tui'."
  :group 'agent-tui)

(defcustom agent-tui-bwrap-enabled t
  "When non-nil, use `bwrap' when it is available."
  :type 'boolean
  :group 'agent-tui-bwrap)

(defcustom agent-tui-bwrap-cpu-limit 4
  "Maximum CPU quota, in CPUs, to request through `systemd-run'."
  :type 'integer
  :group 'agent-tui-bwrap)

(defcustom agent-tui-bwrap-memory-limit-gb 32
  "Maximum memory limit, in GiB, to request through `systemd-run'."
  :type 'integer
  :group 'agent-tui-bwrap)

(defcustom agent-tui-bwrap-memory-fraction 0.8
  "Fraction of host memory to request through `systemd-run'."
  :type 'number
  :group 'agent-tui-bwrap)

(defcustom agent-tui-bwrap-temp-prefix "/tmp/agent-tui-session"
  "Prefix for temporary directories exposed inside sandboxed sessions."
  :type 'string
  :group 'agent-tui-bwrap)

(defcustom agent-tui-bwrap-cleanup-temp-days 6
  "Delete sandbox temporary directories older than this many days.

Set to nil to disable cleanup."
  :type '(choice (const :tag "Disable cleanup" nil) integer)
  :group 'agent-tui-bwrap)

(defcustom agent-tui-bwrap-bind-paths
  '(("./" . w)
    ("~/.cache" . w)
    ("~/.cline" . w)
    ("~/.codex" . w)
    ("~/.config/goose" . w)
    ("~/.local/share/goose" . w)
    ("~/.local/state/goose" . w)
    ("~/.local/share/opencode" . w)
    ("~/.eclipse" . w)
    ("~/.gradle" . w)
    ("~/.hermes" . w)
    ("~/.m2" . w)
    ("~/.mx" . w)
    ("~/.npm" . w)
    ("~/.opencode" . w)
    ("~/.pi" . w)
    ("~/dev/.metadata" . w)
    ("~/dev/ci-overlays/.git" . w)
    ("~/dev/graal/.git" . w)
    ("~/dev/graal-enterprise/.git" . w)
    ("~/dev/graalpython/.git" . w)
    ("~/dev/eclipse" . w)
    ("~/dev/mx" . w)
    ("../graal" . w)
    ("../graalos" . w)
    ("../graalos-image-builder" . w)
    ("../graal-enterprise" . w)
    ("../ci-overlays" . w)

    ("~/.codex/config.toml" . w)
    ("~/.config/goose/config.yaml" . w)
    ("~/.config/goose/adversary.md" . w)
    ("~/.config/opencode/opencode.jsonc" . w)
    ("~/.cline/data/globalState.json" . w)
    ("~/.cline/data/settings/cline_mcp_settings.json" . w)

    ("~/.agents" . r)
    ("~/.bun" . r)
    ("~/.bundle" . r)
    ("~/.cargo" . r)
    ("~/.config" . r)
    ("~/.docker" . r)
    ("~/.emacs.d" . r)
    ("~/.gitconfig" . r)
    ("~/.gitignore" . r)
    ("~/.local" . r)
    ("~/.npmrc" . r)
    ("~/.nvm" . r)
    ("~/.ol" . r)
    ("~/.ssh" . r)
    ("~/.pyenv" . r)
    ("~/.rustup" . r)
    ("~/.sdkman" . r)
    ("~/dotfiles" . r)
    ("~/dev" . r)

    ("~/.config/mc" . nil)
    ("~/.config/onedrive" . nil)
    ("~/.config/pulse" . nil)
    ("~/.config/rclone" . nil))
  "Paths that `agent-shell' bwrap sessions should bind."
  :type '(repeat (cons (string :tag "Path")
                       (choice (const :tag "Read/write" w)
                               (const :tag "Read-only" r)
                               (const :tag "Hidden" nil))))
  :group 'agent-tui-bwrap)

(defcustom 
  '(("./" . rw)
    ("~/.pi" . rw)
    ("~/.cache" . rw)
    ("~/.local" . ro)
    ("~/.config" . ro)
    ("~/.gitconfig" . ro)
    ("~/.ssh" . ro))
  "Paths to expose inside a Bubblewrap session.

Each entry is a path relative to the launch directory or home directory, and
is paired with `rw' or `ro'.  The root filesystem is otherwise read-only."
  :type '(repeat (cons (string :tag "Path")
                       (choice (const :tag "Read/write" rw)
                               (const :tag "Read-only" ro))))
  :group 'agent-tui-bwrap)

(defvar agent-tui-bwrap--previous-command-prefix nil)

(defun agent-tui-bwrap--memory-limit ()
  "Return a systemd memory limit, or nil when unavailable."
  (when-let* ((total-kib (ignore-errors (car (memory-info))))
              (total-gib (/ total-kib 1024.0 1024.0))
              (limit (max 1
                          (min agent-tui-bwrap-memory-limit-gb
                               (floor (* agent-tui-bwrap-memory-fraction
                                         total-gib))))))
    (format "MemoryMax=%dG" limit)))

(defun agent-tui-bwrap--systemd-prefix ()
  "Return the optional `systemd-run' argv prefix."
  (when (executable-find "systemd-run" t)
    (let ((cpus (max 1 (min agent-tui-bwrap-cpu-limit
                             (/ (max 1 (num-processors)) 2)))))
      (append
       (list "systemd-run" "--user" "--scope" "-p"
             (format "CPUQuota=%d00%%" cpus))
       (when-let* ((memory (agent-tui-bwrap--memory-limit)))
         (list "-p" memory))
       (list "--")))))

(defun agent-tui-bwrap--cleanup-temp-dirs ()
  "Delete stale temporary directories created by this package."
  (when agent-tui-bwrap-cleanup-temp-days
    (let* ((directory (file-name-directory agent-tui-bwrap-temp-prefix))
           (prefix (file-name-nondirectory agent-tui-bwrap-temp-prefix))
           (cutoff (time-subtract
                    (current-time)
                    (days-to-time agent-tui-bwrap-cleanup-temp-days))))
      (when (file-directory-p directory)
        (dolist (path (directory-files directory t
                                       (concat "\\`" (regexp-quote prefix))))
          (when (and (file-directory-p path)
                     (time-less-p
                      (file-attribute-modification-time
                       (file-attributes path))
                      cutoff))
            (ignore-errors (delete-directory path t))))))))

;;;###autoload
(defun agent-tui-bwrap-command-prefix (directory)
  "Return an argv prefix for a TUI launched in DIRECTORY."
  (let* ((default-directory (file-name-as-directory directory))
         (prefix (agent-tui-bwrap--systemd-prefix)))
    (if (not (and agent-tui-bwrap-enabled (executable-find "bwrap" t)))
        prefix
      (agent-tui-bwrap--cleanup-temp-dirs)

      (let* ((tmpdir (make-temp-file
                      agent-tui-bwrap-temp-prefix t
                      (replace-regexp-in-string
                       "[^[:alnum:]]" ""
                       (or default-directory "agent-tui")))))
        (append
         prefix
         `("bwrap" "--die-with-parent" "--new-session"
           "--ro-bind" "/" "/"
           "--tmpfs" "/tmp"
           "--tmpfs" ,(getenv "HOME"))
         (thread-last
           (seq-map (lambda (e) (cons (expand-file-name (car e)) (cdr e))) agent-tui-bwrap-bind-paths)
           (seq-sort (lambda (e1 e2) (string-lessp (car e1) (car e2))))
           (seq-filter (lambda (e) (file-exists-p (car e))))
           (seq-mapcat (lambda (e)
                         (let ((p (car e))
                               (m (cdr e)))
                           (cond
                            ((eq m 'w) (list "--bind" (file-truename p) (file-truename p)
                                             "--bind" p p))
                            ((eq m 'r) (list "--ro-bind" (file-truename p) (file-truename p)
                                             "--ro-bind" p p))
                            (t (list "--tmpfs" p)))))))
         `("--proc" "/proc"
           "--dev" "/dev"
           "--chdir" ,default-directory
           "--setenv" "HTTP_PROXY" ,(or (getenv "HTTP_PROXY") "")
           "--setenv" "HTTPS_PROXY" ,(or (getenv "HTTPS_PROXY") "")
           "--setenv" "NO_PROXY" ,(or (getenv "NO_PROXY") "")
           "--setenv" "HOME" ,(getenv "HOME")
           "--setenv" "TMPDIR" ,tmpdir
           "--setenv" "XDG_CACHE_INNER" ,(expand-file-name ".agent-shell/xdgcache")
           "--setenv" "XDG_STATE_INNER" ,(expand-file-name ".agent-shell/xdgstate")
           "--setenv" "XDG_RUNTIME_INNER" ,(expand-file-name ".agent-shell/xdgruntime")
           "--"))))))

;;;###autoload
(define-minor-mode agent-tui-bwrap-mode
  "Use `agent-tui-bwrap-command-prefix' for TUI commands."
  :global t
  :lighter " AT-Bwrap"
  :group 'agent-tui-bwrap
  (if agent-tui-bwrap-mode
      ;; Do not overwrite the saved prefix when enabling an already enabled
      ;; mode.  `define-minor-mode' may be called with 1 more than once, and
      ;; otherwise disabling would restore the bwrap prefix itself.
      (unless (eq agent-tui-command-prefix
                  #'agent-tui-bwrap-command-prefix)
        (setq agent-tui-bwrap--previous-command-prefix
              agent-tui-command-prefix)
        (setq agent-tui-command-prefix
              #'agent-tui-bwrap-command-prefix))
    (when (eq agent-tui-command-prefix
              #'agent-tui-bwrap-command-prefix)
      (setq agent-tui-command-prefix
            agent-tui-bwrap--previous-command-prefix))))

(provide 'agent-tui-bwrap)

;;; agent-tui-bwrap.el ends here
