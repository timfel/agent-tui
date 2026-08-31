;;; agent-tui.el --- Run coding-agent TUIs in Emacs terminals -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Fel
;; Author: Tim Fel
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;;
;; `agent-tui' is a small provider-neutral layer for running terminal UIs
;; such as pi in an Emacs terminal emulator.  Providers are identified by a
;; symbol and implement the four `agent-tui--*' generic methods below.  The
;; public functions have the provider-neutral API:
;;
;;   (agent-tui-start PREFIX-KEY SESSIONID)
;;   (agent-tui-started BUFFER)
;;   (agent-tui-busy? LAST-BUFFER-CONTENTS)
;;   (agent-tui-get-sessionid BUFFER)
;;
;; A provider's start command should bind `agent-tui-provider' dynamically and
;; call `agent-tui-start'.  The provider symbol is then stored buffer-locally,
;; so timers and bookmarks continue to use the right provider after the start
;; command has returned.  See `agent-tui-pi.el' for a complete example.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function project-current "project" (&optional maybe-prompt directory))
(declare-function project-root "project" (project))
(declare-function project-buffers "project" (project))
(declare-function term-char-mode "term" ())
(declare-function ghostel "ghostel" (&optional new-buffer))
(declare-function ghostel-send-string "ghostel" (string))
(declare-function ghostel-send-key "ghostel" (key-name &optional mods))
(declare-function vterm "vterm" (&optional arg))
(declare-function eat "eat" (&optional buffer))

(defgroup agent-tui nil
  "Run agent TUIs in Emacs terminal emulators."
  :group 'tools
  :prefix "agent-tui-")

(defcustom agent-tui-terminal 'ghostel
  "Terminal emulator to use for agent TUIs.

The supported values are `ghostel', `vterm', `eat', and `term'."
  :type '(choice (const :tag "Ghostel" ghostel)
                 (const :tag "Vterm" vterm)
                 (const :tag "Eat" eat)
                 (const :tag "Built-in term" term))
  :group 'agent-tui)

(defcustom agent-tui-command-prefix nil
  "Command prefix used when starting an agent TUI.

This is either shell text, an argv list, or a function.  A function is
called with the current directory as its only argument and must return shell
text or an argv list.  The resulting command is placed before the provider's
TUI command."
  :type '(choice (const :tag "No prefix" nil)
                 (string :tag "Shell text")
                 (repeat :tag "Argument vector" string)
                 (function :tag "Function"))
  :group 'agent-tui)

(defcustom agent-tui-busy-check-period 4
  "Number of seconds between checks of an agent TUI's busy state."
  :type 'number
  :group 'agent-tui)

(defvar agent-tui-provider nil
  "Provider used by `agent-tui-start'.

Provider packages normally bind this dynamically in their public start
command.  Setting it globally is also useful when there is only one provider
in a configuration.")

(defvar agent-tui-started-hook nil
  "Hook run in a buffer after generic agent TUI setup has completed.")

(defvar agent-tui-idle-hook nil
  "Hook run once when the current agent TUI becomes idle.

The hook is made buffer-local in agent TUI buffers.  It is not run again until
`agent-tui-busy?' has returned non-nil and subsequently returns nil.")

;; These variables are deliberately implementation details.  In particular,
;; the provider is stored in each buffer rather than relying on the dynamically
;; bound global `agent-tui-provider' after the start command returns.
(defvar-local agent-tui--provider nil)
(defvar-local agent-tui--terminal nil)
(defvar-local agent-tui--busy-timer nil)
(defvar-local agent-tui--idle-notified nil)
(defvar-local agent-tui--prompt-queue nil)

(defconst agent-tui--buffer-name "*agent-tui*")
(defconst agent-tui--last-buffer-lines 12)
(defconst agent-tui--last-buffer-bytes 8192)

;;; Provider protocol

(cl-defgeneric agent-tui--start (provider &optional prefix-key sessionid)
  "Start PROVIDER in a terminal and return its buffer.

PREFIX-KEY is the prefix argument supplied to the public start command.
SESSIONID, when non-empty, identifies a session to resume."
  (ignore prefix-key sessionid)
  (error "Provider %S does not implement agent-tui start" provider))

(cl-defgeneric agent-tui--started (provider buffer)
  "Perform provider-specific setup after PROVIDER started in BUFFER."
  (ignore provider buffer)
  nil)

(cl-defgeneric agent-tui--busy-p (provider last-buffer-contents)
  "Return non-nil when PROVIDER is busy.

LAST-BUFFER-CONTENTS contains the last few rendered lines from the terminal
buffer."
  (ignore provider last-buffer-contents)
  nil)

(cl-defgeneric agent-tui--get-sessionid (provider buffer)
  "Return PROVIDER's resumable session id for BUFFER, or an empty string."
  (ignore provider buffer)
  "")

(defun agent-tui--provider-for-buffer (&optional buffer)
  "Return the provider associated with BUFFER, or the global provider."
  (if buffer
      (with-current-buffer buffer
        (or agent-tui--provider agent-tui-provider))
    (or agent-tui--provider agent-tui-provider)))

(defun agent-tui--prefix-level (prefix-key)
  "Return the numeric level of PREFIX-KEY.

The value returned by the `P' interactive code is nil, a number, or a list
such as `(4)' or `(16)'."
  (cond ((null prefix-key) 0)
        ((eq prefix-key t) 1)
        ;; Any non-nil prefix is a request for a new buffer, including a
        ;; negative or zero numeric prefix.  The magnitude is only used to
        ;; recognize the double-prefix selection operation.
        (t (abs (prefix-numeric-value prefix-key)))))

(defun agent-tui--directory (directory)
  "Return DIRECTORY in the canonical directory-name form."
  (file-name-as-directory (expand-file-name directory)))

(defun agent-tui--project-context (&optional directory)
  "Return `(PROJECT . ROOT)' for DIRECTORY using `project.el'."
  (require 'project)
  (let* ((directory (agent-tui--directory (or directory default-directory)))
         (project (project-current nil directory))
         (root (if project
                   (project-root project)
                 directory)))
    (cons project (agent-tui--directory root))))

(defun agent-tui--active-buffer-p (buffer)
  "Return non-nil when BUFFER is an active agent-tui buffer."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and agent-tui--provider
              (timerp agent-tui--busy-timer)
              ;; A provider may use a terminal without exposing a process, so
              ;; absence of a process is not by itself evidence of inactivity.
              (let ((process (get-buffer-process buffer)))
                (or (null process) (process-live-p process)))))))

;;;###autoload
(defun agent-tui-buffers ()
  "Return active agent-tui buffers in most-recently-used order."
  (seq-filter #'agent-tui--active-buffer-p (buffer-list)))

;;;###autoload
(defun agent-tui-cwd (&optional buffer)
  "Return BUFFER's working directory.

This is the directory inherited by the terminal process.  BUFFER defaults to
`current-buffer'."
  (with-current-buffer (or buffer (current-buffer))
    (file-name-as-directory (expand-file-name default-directory))))

;;;###autoload
(defun agent-tui-status (&optional buffer)
  "Return BUFFER's current status.

The result is `busy', `ready', `unknown', or `dead'.  BUFFER defaults to
`current-buffer'."
  (setq buffer (or buffer (current-buffer)))
  (if (not (agent-tui--active-buffer-p buffer))
      'dead
    (with-current-buffer buffer
      (condition-case nil
          (if (agent-tui-busy? (agent-tui--last-buffer-contents))
              'busy
            'ready)
        (error 'unknown)))))

(defun agent-tui--project-buffers (project root)
  "Return active agent-tui buffers in PROJECT rooted at ROOT, in MRU order."
  (let ((project-buffers (and project (project-buffers project))))
    (cl-loop for buffer in (buffer-list)
             when (and (agent-tui--active-buffer-p buffer)
                       (if project
                           (memq buffer project-buffers)
                         (with-current-buffer buffer
                           (equal (agent-tui--directory default-directory)
                                  root))))
             collect buffer)))

(defun agent-tui--select-buffer (buffers)
  "Select and return one buffer from BUFFERS."
  (let* ((names (mapcar #'buffer-name buffers))
         (default (car names))
         (name (completing-read "Agent TUI buffer: " names nil t nil nil default)))
    (get-buffer name)))

(defun agent-tui--select-existing-buffer (buffer)
  "Select BUFFER in the current window and return it."
  (pop-to-buffer-same-window buffer)
  buffer)

(defun agent-tui--session-buffer (buffers sessionid)
  "Return the buffer in BUFFERS associated with SESSIONID, if any."
  (cl-loop for buffer in buffers
           when (condition-case nil
                    (equal (agent-tui-get-sessionid buffer) sessionid)
                  (error nil))
           return buffer))

;;;###autoload
(defun agent-tui-start (&optional prefix-key sessionid)
  "Start or select the selected agent TUI.

The selected provider is `agent-tui-provider'.  Provider start commands should
bind that variable and call this function.

With no PREFIX-KEY, select the most recently used active agent-tui buffer in
the current `project.el' project.  If there is no such buffer, start one with
its `default-directory' set to the project root.  A single prefix always
starts a new buffer.  A double (or larger) prefix prompts for one of the
active buffers instead.  If SESSIONID is provided, an existing buffer for
that session is reused when possible; otherwise a new buffer resumes it.
Return the selected or started terminal buffer."
  (interactive "P")
  (let* ((provider (or agent-tui-provider
                       (user-error "No agent-tui provider selected")))
         (prefix-level (agent-tui--prefix-level prefix-key))
         (context (agent-tui--project-context))
         (project (car context))
         (root (cdr context))
         (buffers (agent-tui--project-buffers project root))
         (sessionid (and sessionid
                         (not (string-empty-p sessionid))
                         sessionid))
         buffer)
    (cond
     ;; A session ID identifies the session to resume.  Prefer an already
     ;; visible matching buffer over starting another process.
     ((and sessionid (null prefix-key)
           (setq buffer (agent-tui--session-buffer buffers sessionid)))
      (agent-tui--select-existing-buffer buffer))
     ;; A double prefix is explicitly the buffer-selection operation.  There
     ;; is no sensible selection to offer when the candidate set is empty.
     ((and (>= prefix-level 16) (null sessionid))
      (unless buffers
        (user-error "No active agent-tui buffers in project %s" root))
      (agent-tui--select-existing-buffer (agent-tui--select-buffer buffers)))
     ;; A single prefix, or a session ID without a matching buffer, starts a
     ;; new process in the project root.
     ((or prefix-key sessionid)
      (let ((default-directory root))
        ;; A session ID must not accidentally be sent to an unrelated
        ;; terminal that a terminal emulator might reuse for a nil argument.
        (setq buffer (agent-tui--start provider
                                       (or prefix-key t)
                                       sessionid))))
     ;; No prefix: reuse the most recent active process, or start at ROOT.
     ((setq buffer (car buffers))
      (agent-tui--select-existing-buffer buffer))
     (t
      (let ((default-directory root))
        ;; There was no existing agent-tui buffer, so force a fresh terminal
        ;; even though the user did not type a prefix.
        (setq buffer (agent-tui--start provider t nil)))))
    (if (memq buffer buffers)
        buffer
      (unless (buffer-live-p buffer)
        (error "Provider %S did not return a live terminal buffer" provider))
      (with-current-buffer buffer
        (setq-local agent-tui--provider provider))
      (agent-tui-started buffer)
      ;; Terminal packages are asked only to create a new buffer.  Select it
      ;; here, alongside the existing-buffer selection paths above.
      (agent-tui--select-existing-buffer buffer))))

;;;###autoload
(defun agent-tui-start-in-directory
    (provider directory &optional prefix-key sessionid no-focus)
  "Start PROVIDER in DIRECTORY and return its terminal buffer.

When NO-FOCUS is non-nil, start a fresh buffer without selecting it.  This is
a convenience for integrations that need to launch a provider without relying
on the dynamically bound global `agent-tui-provider'."
  (let* ((directory (agent-tui--directory directory))
         (default-directory directory)
         (agent-tui-provider provider)
         (buffer
          (if no-focus
              (let ((buffer (agent-tui--start provider
                                               (or prefix-key t)
                                               sessionid)))
                (unless (buffer-live-p buffer)
                  (error "Provider %S did not return a live terminal buffer"
                         provider))
                (with-current-buffer buffer
                  (setq-local agent-tui--provider provider))
                (agent-tui-started buffer)
                buffer)
            (agent-tui-start prefix-key sessionid))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq default-directory directory)))
    buffer))

(defun agent-tui--restart (resume)
  "Restart the current agent TUI, optionally RESUME its session."
  (let ((buffer (current-buffer)))
    (unless (agent-tui--active-buffer-p buffer)
      (user-error "The current buffer is not an active agent-tui buffer"))
    (let* ((provider (agent-tui--provider-for-buffer buffer))
           (directory (agent-tui-cwd buffer))
           (sessionid (and resume (agent-tui-get-sessionid buffer))))
      (when (and resume
                 (or (null sessionid) (string-empty-p sessionid)))
        (user-error "The current agent-tui buffer has no resumable session"))
      (kill-buffer buffer)
      (agent-tui-start-in-directory provider directory t sessionid))))

;;;###autoload
(defun agent-tui-restart ()
  "Close the current agent TUI and start a fresh session with its provider."
  (interactive)
  (agent-tui--restart nil))

;;;###autoload
(defun agent-tui-reload ()
  "Close the current agent TUI and resume its session with its provider."
  (interactive)
  (agent-tui--restart t))

;;;###autoload
(defun agent-tui-started (buffer)
  "Run post-start setup for the agent TUI in BUFFER.

This is also useful to providers that need to perform setup after creating a
terminal themselves."
  (interactive (list (current-buffer)))
  (unless (buffer-live-p buffer)
    (error "Not a live buffer: %S" buffer))
  (let ((provider (agent-tui--provider-for-buffer buffer)))
    (unless provider
      (error "No agent-tui provider associated with %s" (buffer-name buffer)))
    (setq buffer (agent-tui--ensure-buffer-name buffer))
    (with-current-buffer buffer
      (setq-local agent-tui--provider provider))
    ;; Install the generic monitor before calling provider setup.  Providers
    ;; may implement `agent-tui--started' without having to remember to call
    ;; the next method merely to get the default timer.
    (agent-tui--initialize-buffer buffer)
    (agent-tui--started provider buffer)
    (with-current-buffer buffer
      (run-hooks 'agent-tui-started-hook))
    buffer))

;;;###autoload
(defun agent-tui-busy? (last-buffer-contents)
  "Return non-nil if the current agent TUI is still busy.

LAST-BUFFER-CONTENTS is normally supplied by the internal timer."
  (let ((provider (agent-tui--provider-for-buffer)))
    (unless provider
      (error "No agent-tui provider associated with %s" (buffer-name)))
    (agent-tui--busy-p provider last-buffer-contents)))

;;;###autoload
(defun agent-tui-get-sessionid (buffer)
  "Return the resumable session id for the agent TUI in BUFFER.

Providers that do not support sessions return the empty string."
  (unless (buffer-live-p buffer)
    (error "Not a live buffer: %S" buffer))
  (let ((provider (agent-tui--provider-for-buffer buffer)))
    (if provider
        (agent-tui--get-sessionid provider buffer)
      "")))

;;; Terminal support

(defun agent-tui--ensure-buffer-name (buffer)
  "Ensure BUFFER has an agent-tui-specific name."
  (with-current-buffer buffer
    (unless (or (string= (buffer-name) agent-tui--buffer-name)
                (string-match-p
                 (concat "\\`" (regexp-quote agent-tui--buffer-name)
                         "<[0-9]+>\\'")
                 (buffer-name)))
      (rename-buffer (generate-new-buffer-name agent-tui--buffer-name))))
  buffer)

(defun agent-tui--start-eat (&optional _prefix-key)
  "Start a new Eat buffer.

The optional argument is retained for the terminal helper's common calling
convention; buffer reuse is handled by `agent-tui-start', not by Eat."
  (require 'eat)
  ;; Eat accepts a buffer or buffer name as its optional argument.  Supplying
  ;; a generated name makes this unconditionally create a new terminal.
  (funcall #'eat (generate-new-buffer-name "*eat*")))

(defun agent-tui--start-term (_prefix-key)
  "Start a new built-in term buffer."
  (require 'term)
  (let ((buffer (get-buffer-create (generate-new-buffer-name "*term*"))))
    (with-current-buffer buffer
      (declare-function term-mode "term.el")
      (term-mode)
      (declare-function term-exec "term.el")
      (term-exec (current-buffer) "term" (or (getenv "SHELL") shell-file-name) nil nil)
      (term-char-mode))
    buffer))

(defun agent-tui--terminal-process-sentinel (process event)
  "Run the original sentinel and kill the TUI buffer when PROCESS exits."
  (let ((buffer (process-buffer process))
        (sentinel (process-get process 'agent-tui-original-sentinel)))
    (unwind-protect
        (when (and sentinel
                   (not (eq sentinel #'agent-tui--terminal-process-sentinel)))
          (funcall sentinel process event))
      ;; The command sent to the terminal ends its shell after the agent exits.
      ;; Kill the buffer here as well because `term' and some third-party
      ;; terminal emulators leave dead terminal buffers behind.
      (when (and (buffer-live-p buffer)
                 (not (process-live-p process)))
        (kill-buffer buffer)))))

(defun agent-tui--install-terminal-process-sentinel (buffer)
  "Arrange for BUFFER to be killed when its terminal process exits."
  (with-current-buffer buffer
    (when-let* ((process (get-buffer-process buffer)))
      (unless (eq (process-sentinel process)
                  #'agent-tui--terminal-process-sentinel)
        (process-put process 'agent-tui-original-sentinel
                     (process-sentinel process))
        (set-process-sentinel
         process #'agent-tui--terminal-process-sentinel)))))

(defun agent-tui--start-terminal (command &optional prefix-key)
  "Start a new terminal and execute COMMAND in it.

PREFIX-KEY is accepted for the common provider calling convention, but is not
used to choose a terminal.  Terminal-buffer selection is handled by
`agent-tui-start'.  Return the new terminal buffer.
This helper is intended for provider implementations."
  (ignore prefix-key)
  (let* ((terminal agent-tui-terminal)
         (buffer
          (pcase terminal
            ('ghostel
             (require 'ghostel)
             ;; Ghostel's optional argument is its new-buffer flag.
             (funcall #'ghostel t))
            ('vterm
             (require 'vterm)
             ;; A non-nil argument makes a new vterm; do not pass the user's
             ;; numeric prefix because vterm assigns it a buffer number.
             (funcall #'vterm t))
            ('eat
             (agent-tui--start-eat))
            ('term
             (agent-tui--start-term prefix-key))
            (_ (error "Unknown agent-tui terminal: %S" terminal)))))
    (unless (buffer-live-p buffer)
      ;; A few terminal commands switch to their buffer but return nil.
      (setq buffer (current-buffer)))
    (unless (buffer-live-p buffer)
      (error "Could not create a %s terminal buffer" terminal))
    (setq buffer (agent-tui--ensure-buffer-name buffer))
    (with-current-buffer buffer
      (setq-local agent-tui--terminal terminal)
      (agent-tui--install-terminal-process-sentinel buffer)
      ;; The terminal starts a shell before the provider command is sent.  End
      ;; that shell after COMMAND returns so quitting the provider also ends
      ;; the terminal process.
      (agent-tui--send-input (concat command "; exit")))
    buffer))

(defun agent-tui--process-send-string (string)
  "Send STRING to the current buffer's terminal process."
  (let ((process (get-buffer-process (current-buffer))))
    (unless (process-live-p process)
      (error "No live terminal process in %s" (buffer-name)))
    (process-send-string process string)))

(defun agent-tui--send-string (string)
  "Send STRING to the current buffer's terminal emulator."
  (pcase agent-tui--terminal
    ('ghostel
     (if (fboundp 'ghostel-send-string)
         ;; Ghostel's low-level sender expects UTF-8 bytes.
         (ghostel-send-string (encode-coding-string string 'utf-8))
       (agent-tui--process-send-string string)))
    ('vterm
     (if (fboundp 'vterm-send-string)
         (vterm-send-string string)
       (agent-tui--process-send-string string)))
    ('eat
     (if (and (fboundp 'eat-term-send-string)
              (boundp 'eat-terminal)
              eat-terminal)
         (eat-term-send-string eat-terminal string)
       (agent-tui--process-send-string string)))
    (_ (agent-tui--process-send-string string))))

(defun agent-tui--send-return ()
  "Send a Return key to the current buffer's terminal emulator."
  (pcase agent-tui--terminal
    ('ghostel
     (if (fboundp 'ghostel-send-key)
         (ghostel-send-key "return")
       (agent-tui--process-send-string "\r")))
    ('vterm
     (if (fboundp 'vterm-send-return)
         (vterm-send-return)
       (agent-tui--process-send-string "\r")))
    ('eat
     (if (and (fboundp 'eat-self-input)
              (boundp 'eat-terminal)
              eat-terminal)
         ;; This is Eat's public way to send a terminal key event.
         (eat-self-input 1 'return)
       (agent-tui--process-send-string "\r")))
    (_ (agent-tui--process-send-string "\r"))))

(defun agent-tui--send-input (string)
  "Send STRING followed by Return to the current terminal."
  (agent-tui--send-string string)
  (agent-tui--send-return))

(defun agent-tui--command-prefix (directory)
  "Return the configured command prefix for DIRECTORY as shell text."
  (let ((prefix (cond ((null agent-tui-command-prefix) nil)
                      ((functionp agent-tui-command-prefix)
                       (funcall agent-tui-command-prefix directory))
                      ((or (stringp agent-tui-command-prefix)
                           (listp agent-tui-command-prefix))
                       agent-tui-command-prefix)
                      (t (user-error "Invalid agent-tui-command-prefix: %S"
                                     agent-tui-command-prefix)))))
    (cond
     ((null prefix) "")
     ((stringp prefix)
      (setq prefix (string-trim-right prefix))
      (if (string-empty-p prefix) "" (concat prefix " ")))
     ((listp prefix)
      (unless (cl-every #'stringp prefix)
        (user-error "agent-tui command prefix argv must contain strings: %S"
                    prefix))
      (if prefix
          (concat (mapconcat #'shell-quote-argument prefix " ") " ")
        ""))
     (t
      (user-error "agent-tui command prefix must return shell text or an argv list, got %S"
                  prefix)))))

;;; Busy/idle monitoring

(defun agent-tui--strip-terminal-control-sequences (string)
  "Remove common terminal control sequences from STRING."
  (setq string (replace-regexp-in-string "\033\\][^\007]*\007" "" string))
  (setq string
        (replace-regexp-in-string "\033\\[[0-?]*[ -/]*[@-~]" "" string))
  (setq string (replace-regexp-in-string "\033[()][0-2]" "" string))
  (replace-regexp-in-string "\r" "" string))

(defun agent-tui--last-buffer-contents ()
  "Return the last few rendered lines of the current terminal buffer."
  (save-restriction
    (widen)
    (let* ((end (point-max))
           (start (max (point-min) (- end agent-tui--last-buffer-bytes)))
           (contents (buffer-substring-no-properties start end)))
      (setq contents (agent-tui--strip-terminal-control-sequences contents))
      (mapconcat #'identity
                 (last (split-string contents "\n")
                       agent-tui--last-buffer-lines)
                 "\n"))))

(defun agent-tui--cancel-timer ()
  "Cancel the current buffer's agent-tui timer."
  (when (timerp agent-tui--busy-timer)
    (cancel-timer agent-tui--busy-timer))
  (setq agent-tui--busy-timer nil))

(defun agent-tui--initialize-buffer (buffer)
  "Install generic agent-tui state and monitoring in BUFFER."
  (with-current-buffer buffer
    (make-local-variable 'agent-tui-idle-hook)
    (make-local-variable 'agent-tui--prompt-queue)
    (agent-tui--cancel-timer)
    (setq agent-tui--idle-notified nil)
    (add-hook 'kill-buffer-hook #'agent-tui--cancel-timer nil t)
    (unless (and (numberp agent-tui-busy-check-period)
                 (> agent-tui-busy-check-period 0))
      (user-error "agent-tui-busy-check-period must be greater than zero"))
    (setq agent-tui--busy-timer
          (run-at-time agent-tui-busy-check-period
                       agent-tui-busy-check-period
                       #'agent-tui--busy-timer-function
                       (current-buffer)))))

(defun agent-tui--busy-timer-function (&optional buffer)
  "Check BUFFER and run its idle hook on a busy-to-idle transition.

`run-at-time' passes only the arguments supplied after the function, not the
timer object itself."
  (setq buffer (or buffer (current-buffer)))
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (condition-case err
          (let ((busy (agent-tui-busy?
                       (agent-tui--last-buffer-contents))))
            (if busy
                ;; A later nil result is now allowed to notify the hook.
                (setq agent-tui--idle-notified nil)
              (unless agent-tui--idle-notified
                ;; Set this before running user hooks so a hook that causes
                ;; redisplay or queues work cannot cause duplicate callbacks.
                (setq agent-tui--idle-notified t)
                (run-hooks 'agent-tui-idle-hook)
                (agent-tui--flush-prompt-queue))))
        (error
         ;; A provider error should not make the repeating timer disappear.
         (message "agent-tui busy check failed in %s: %s"
                  (buffer-name) (error-message-string err)))))))

;;; Prompt queue

(defun agent-tui--flush-prompt-queue ()
  "Send all prompts queued for the current buffer."
  (catch 'agent-tui-stop-flushing
    (while agent-tui--prompt-queue
      (let ((prompt (pop agent-tui--prompt-queue)))
        (condition-case err
            (agent-tui--send-input prompt)
          (error
           ;; Keep the failed prompt and all prompts after it for the next idle
           ;; transition.  Do not turn a transient terminal error into lost work.
           (push prompt agent-tui--prompt-queue)
           (message "agent-tui could not send queued prompt: %s"
                    (error-message-string err))
           (throw 'agent-tui-stop-flushing nil)))))))

;;;###autoload
(defun agent-tui-enqueue-prompt (string)
  "Send STRING immediately if idle, or queue it while the agent TUI is busy.

When the buffer is busy, STRING is sent to the terminal after it becomes idle.
Multiple queued prompts are sent in queue order."
  (interactive "sPrompt: ")
  (unless (stringp string)
    (user-error "Prompt must be a string"))
  (unless (and (boundp 'agent-tui--busy-timer)
               (timerp agent-tui--busy-timer))
    (user-error "The current buffer is not an active agent-tui buffer"))
  (setq agent-tui--prompt-queue
        (append agent-tui--prompt-queue (list string)))
  (unless (agent-tui-busy? (agent-tui--last-buffer-contents))
    (agent-tui--flush-prompt-queue))
  string)

(provide 'agent-tui)

;;; agent-tui.el ends here
