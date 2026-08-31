;;; agent-tui-context.el --- Emacs context for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; Collect a small amount of useful Emacs context and append it to an
;; explicitly submitted TUI prompt.  Context is not added to ordinary prompts
;; automatically, and no terminal Markdown/link rendering is attempted.
;;
;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'eieio)
(require 'project)
(require 'server)
(require 'seq)
(require 'subr-x)
(require 'xref)
(require 'agent-tui)

(defgroup agent-tui-context nil
  "Add explicit Emacs context to `agent-tui' prompts."
  :group 'agent-tui)

(defcustom agent-tui-context-buffer-limit 5
  "Maximum number of recent buffers to include."
  :type 'integer
  :group 'agent-tui-context)

(defcustom agent-tui-context-lines-around-point 10
  "Number of lines of context to include around point."
  :type 'integer
  :group 'agent-tui-context)

(defcustom agent-tui-context-shell-output-lines 20
  "Number of trailing shell lines to include."
  :type 'integer
  :group 'agent-tui-context)

(defcustom agent-tui-context-special-buffer-regexps
  '("\\`\\*Backtrace\\*\\'"
    "\\`\\*Jira.*"
    "\\`\\*ci-dashboard\\*\\'")
  "Buffer names that are eligible regardless of project."
  :type '(repeat regexp)
  :group 'agent-tui-context)

(defcustom agent-tui-context-include-emacsclient-instructions t
  "When non-nil, explain how the agent can query Emacs with `emacsclient'."
  :type 'boolean
  :group 'agent-tui-context)

(defcustom agent-tui-context-sources
  '(agent-tui-context-region-source
    agent-tui-context-vc-source
    agent-tui-context-magit-source
    agent-tui-context-emacs-source)
  "Functions used by `agent-tui-enqueue-context-prompt'."
  :type '(repeat function)
  :group 'agent-tui-context)

(defcustom agent-tui-context-major-mode-languages
  '((fundamental-mode . "text")
    (special-mode . "text")
    (vterm-mode . "text")
    (ghostel-mode . "text")
    (term-mode . "text")
    (eshell-mode . "text")
    (comint-mode . "text")
    (shell-mode . "text"))
  "Language names used for context fences."
  :type '(alist :key-type symbol :value-type string)
  :group 'agent-tui-context)

(defvar agent-tui-context--target-buffer nil)
(defvar agent-tui-context--origin-buffer nil)

(defun agent-tui-context--origin-buffer ()
  "Return the buffer from which context collection began."
  (or agent-tui-context--origin-buffer (current-buffer)))

(defun agent-tui-context--target-buffer ()
  "Return the TUI receiving the current contextual prompt."
  (or agent-tui-context--target-buffer
      (let ((origin (current-buffer)))
        (or (and (agent-tui--active-buffer-p origin) origin)
            (with-current-buffer origin
              (let* ((context (agent-tui--project-context))
                     (buffers (agent-tui--project-buffers
                               (car context) (cdr context))))
                (or (car buffers)
                    (car (agent-tui-buffers)))))
            (user-error "No active agent-tui buffer")))))

(defun agent-tui-context--project-root (buffer)
  "Return BUFFER's project root, or its working directory."
  (with-current-buffer buffer
    (let ((directory (agent-tui-cwd buffer)))
      (or (when-let* ((project (project-current nil directory)))
            (file-name-as-directory (expand-file-name (project-root project))))
          directory))))

(defun agent-tui-context--eligible-buffer-p (buffer project-root)
  "Return non-nil when BUFFER is useful context for PROJECT-ROOT."
  (let ((name (buffer-name buffer)))
    (and name
         (not (string-prefix-p " " name))
         (not (agent-tui--active-buffer-p buffer))
         (not (with-current-buffer buffer
                (derived-mode-p 'agent-tui-dashboard-mode)))
         (or (seq-some (lambda (regexp)
                         (string-match-p regexp name))
                       agent-tui-context-special-buffer-regexps)
             (when-let* ((buffer-root (agent-tui-context--buffer-directory buffer)))
               (string-prefix-p
                (file-name-as-directory project-root)
                buffer-root))))))

(defun agent-tui-context--buffer-directory (buffer)
  "Return BUFFER's directory when it has one."
  (with-current-buffer buffer
    (when (and default-directory (file-directory-p default-directory))
      (file-name-as-directory (expand-file-name default-directory)))))

(defun agent-tui-context--tail-lines (start end count)
  "Return up to COUNT trailing lines between START and END."
  (save-excursion
    (goto-char end)
    (forward-line (- count))
    (buffer-substring-no-properties (max start (point)) end)))

(defun agent-tui-context--lines-around-point ()
  "Return a snippet around point in the current buffer."
  (save-excursion
    (let ((origin (point)))
      (goto-char origin)
      (forward-line (- agent-tui-context-lines-around-point))
      (let ((start (point)))
        (goto-char origin)
        (forward-line agent-tui-context-lines-around-point)
        (buffer-substring-no-properties start (point))))))

(defun agent-tui-context--snippet (buffer)
  "Return a compact snippet from BUFFER."
  (with-current-buffer buffer
    (if (derived-mode-p 'vterm-mode 'term-mode 'eshell-mode
                        'comint-mode 'shell-mode)
        (agent-tui-context--tail-lines
         (point-min) (point-max) agent-tui-context-shell-output-lines)
      (agent-tui-context--lines-around-point))))

(defun agent-tui-context--language (buffer)
  "Return a fence language for BUFFER."
  (with-current-buffer buffer
    (or (alist-get major-mode agent-tui-context-major-mode-languages)
        (let ((name (string-remove-suffix "-mode" (symbol-name major-mode))))
          (when (string-match-p "\\`[[:alnum:]+#-]+\\'" name)
            name)))))

(defun agent-tui-context--fenced (text &optional language)
  "Put TEXT in a Markdown fence with optional LANGUAGE."
  (let ((run 0) (pos 0))
    (while (string-match "`+" text pos)
      (setq run (max run (- (match-end 0) (match-beginning 0)))
            pos (match-end 0)))
    (let ((fence (make-string (max 3 (1+ run)) ?`)))
      (format "%s%s\n%s\n%s" fence (or language "") text fence))))

(defun agent-tui-context--emacsclient-command ()
  "Return an `emacsclient' command for the current server, or nil."
  (when (and agent-tui-context-include-emacsclient-instructions
             (boundp 'server-process)
             (process-live-p server-process))
    (if (or server-use-tcp
            (memq system-type '(windows-nt ms-dos cygwin)))
        (format "emacsclient --server-file=%s"
                (shell-quote-argument
                 (expand-file-name server-name server-auth-dir)))
      (format "emacsclient --socket-name=%s"
              (shell-quote-argument
               (expand-file-name server-name server-socket-dir))))))

(defun agent-tui-context--xref-summary (history)
  "Return a short readable summary of XREF HISTORY."
  (let (items)
    (dolist (entry (seq-take history 5))
      (let ((marker (cond ((markerp entry) entry)
                          ((and (consp entry) (markerp (car entry))) (car entry))
                          ((and (consp entry) (markerp (cdr entry))) (cdr entry)))))
        (when (and marker (marker-buffer marker))
          (with-current-buffer (marker-buffer marker)
            (save-excursion
              (goto-char marker)
              (push (format "%s (%s)"
                            (or (thing-at-point 'symbol t)
                                (format "line %d" (line-number-at-pos)))
                            (buffer-name))
                    items))))))
    (when items
      (string-join (nreverse items) " -> "))))

;;;###autoload
(defun agent-tui-context-emacs-source ()
  "Return recent Emacs buffer and navigation context."
  (let* ((target (agent-tui-context--target-buffer))
         (origin (agent-tui-context--origin-buffer))
         (root (agent-tui-context--project-root target))
         (buffers (delete-dups (cons origin (buffer-list))))
         (parts nil)
         (count 0))
    (dolist (buffer buffers)
      (when (and (< count agent-tui-context-buffer-limit)
                 (agent-tui-context--eligible-buffer-p buffer root))
        (let ((snippet (string-trim
                        (agent-tui-context--snippet buffer))))
          (unless (string-empty-p snippet)
            (push (format "### Buffer: %s\n%s"
                          (buffer-name buffer)
                          (agent-tui-context--fenced
                           snippet (agent-tui-context--language buffer)))
                  parts)
            (setq count (1+ count))))))
    (when-let* ((xref-summary
                (and (boundp 'xref--history)
                     (agent-tui-context--xref-summary xref--history))))
      (push (format "### Recent Navigation\n%s" xref-summary) parts))
    (when parts
      (concat
       (when-let* ((command (agent-tui-context--emacsclient-command)))
         (format "Agents may inspect live Emacs state with `%s'.\n\n" command))
       (string-join (nreverse parts) "\n\n")))))

;;;###autoload
(defun agent-tui-context-region-source ()
  "Return the active region from the originating buffer, or nil."
  (let ((buffer (agent-tui-context--origin-buffer)))
    (with-current-buffer buffer
      (when (use-region-p)
        (agent-tui-context--fenced
         (buffer-substring-no-properties (region-beginning) (region-end))
         (agent-tui-context--language buffer))))))

(defun agent-tui-context--diff-patch ()
  "Return the current diff hunk or file, or nil."
  (when (derived-mode-p 'diff-mode)
    (condition-case nil
        (let ((hunk (ignore-errors (diff-bounds-of-hunk)))
              (file (diff-bounds-of-file)))
          (when file
            (let ((bounds (or (and hunk
                                   (list (car hunk) (cadr hunk)))
                              (list (car file) (cadr file)))))
              (buffer-substring-no-properties (car bounds) (cadr bounds)))))
      (error nil))))

;;;###autoload
(defun agent-tui-context-vc-source ()
  "Return the current built-in `diff-mode' patch, or nil."
  (let ((buffer (agent-tui-context--origin-buffer)))
    (with-current-buffer buffer
      (when-let* ((patch (agent-tui-context--diff-patch)))
        (concat "[VC DIFF]\n"
                (agent-tui-context--fenced patch "diff"))))))

(defun agent-tui-context--magit-slot (section slot)
  "Return SLOT from Magit SECTION."
  (eieio-oref section slot))

(defun agent-tui-context--magit-patch (section)
  "Return a compact patch for Magit SECTION."
  (cond
   ((and (fboundp 'magit-hunk-section-p)
         (magit-hunk-section-p section))
    (buffer-substring-no-properties
     (agent-tui-context--magit-slot section 'start)
     (agent-tui-context--magit-slot section 'end)))
   ((and (fboundp 'magit-file-section-p)
         (magit-file-section-p section))
    (concat
     (if (fboundp 'magit-diff-file-header)
         (magit-diff-file-header section)
       "")
     (mapconcat
      #'identity
      (delq nil
            (mapcar
             (lambda (child)
               (when (and (fboundp 'magit-hunk-section-p)
                          (magit-hunk-section-p child))
                 (buffer-substring-no-properties
                  (agent-tui-context--magit-slot child 'start)
                  (agent-tui-context--magit-slot child 'end))))
             (agent-tui-context--magit-slot section 'children)))
      "")))))

;;;###autoload
(defun agent-tui-context-magit-source ()
  "Return the current Magit hunk or file patch, or nil."
  (let ((buffer (agent-tui-context--origin-buffer)))
    (with-current-buffer buffer
      (when (and (require 'magit-mode nil t)
                 (require 'magit-section nil t)
                 (require 'magit-diff nil t)
                 (fboundp 'magit-current-section))
        (when-let* ((section (magit-current-section))
                    (patch (string-trim-right
                            (or (agent-tui-context--magit-patch section) ""))))
          (unless (string-empty-p patch)
            (concat "[MAGIT DIFF]\n"
                    (agent-tui-context--fenced patch "diff"))))))))

(defun agent-tui-context--build (prompt target origin)
  "Build PROMPT with context for TARGET and ORIGIN."
  (let (parts)
    (let ((agent-tui-context--target-buffer target)
          (agent-tui-context--origin-buffer origin))
      (dolist (source agent-tui-context-sources)
        (when (functionp source)
          (condition-case nil
              (when-let* ((part (funcall source)))
                (push part parts))
            (error nil)))))
    (if (null parts)
        prompt
      (concat prompt
              "\n\n[USER ENVIRONMENT CONTEXT - EMACS STATE]\n"
              (string-join (nreverse parts) "\n\n")
              "\n[END CONTEXT]"))))

;;;###autoload
(defun agent-tui-context-prompt (prompt)
  "Return PROMPT with explicitly requested Emacs context appended."
  (unless (stringp prompt)
    (user-error "Prompt must be a string"))
  (let* ((origin (current-buffer))
         (target (agent-tui-context--target-buffer)))
    (agent-tui-context--build prompt target origin)))

;;;###autoload
(defun agent-tui-enqueue-context-prompt (prompt)
  "Append Emacs context to PROMPT and send it to the relevant TUI.

When called outside a TUI, use the most recent TUI in the current project."
  (interactive "sPrompt: ")
  (let* ((origin (current-buffer))
         (target (agent-tui-context--target-buffer))
         (prompt (agent-tui-context--build prompt target origin)))
    (with-current-buffer target
      (agent-tui-enqueue-prompt prompt))))

(provide 'agent-tui-context)

;;; agent-tui-context.el ends here
