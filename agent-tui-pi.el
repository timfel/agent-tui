;;; agent-tui-pi.el --- Pi provider for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Fel
;; Author: Tim Fel
;; Package-Requires: ((emacs "27.1") (agent-tui "0.1.0"))

;;; Commentary:
;;
;; Start Pi with `agent-tui-pi-start'.  Pi's `--session' option accepts both
;; session paths and partial session IDs, so the value returned by
;; `agent-tui-get-sessionid' can be passed back to `agent-tui-pi-start'.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'agent-tui)

(defgroup agent-tui-pi nil
  "Pi integration for agent-tui."
  :group 'agent-tui)

(defcustom agent-tui-pi-command "pi"
  "Command used to start Pi's interactive TUI.

This may include command-line options, but should not include a shell command
prefix; use `agent-tui-command-prefix' for that."
  :type 'string
  :group 'agent-tui-pi)

(defcustom agent-tui-pi-session-query-timeout 1
  "Maximum number of seconds to wait for Pi's `/session' response."
  :type 'number
  :group 'agent-tui-pi)

(defvar-local agent-tui-pi--session-id nil
  "Session ID known for the Pi process in the current buffer.")

(defvar-local agent-tui-pi--session-query-in-progress nil
  "Non-nil while the current buffer is being queried for its Pi session ID.")

(defun agent-tui-pi--new-session-id ()
  "Return a locally generated UUID suitable for a new Pi session.

Passing an explicit ID to Pi gives `agent-tui-get-sessionid' a useful fallback
before Pi is ready to answer a `/session' query."
  (let ((hex (secure-hash 'sha256
                          (format "%s:%s:%s"
                                  (float-time)
                                  (random most-positive-fixnum)
                                  (emacs-pid)))))
    ;; Use UUID v4-shaped text.  Pi treats the value as a project session ID;
    ;; the shape also makes it convenient to use with other Pi versions.
    (format "%s-%s-4%s-8%s-%s"
            (substring hex 0 8)
            (substring hex 8 12)
            (substring hex 13 16)
            (substring hex 17 20)
            (substring hex 20 32))))

(defun agent-tui-pi--command (sessionid directory &optional generated-session-id)
  "Build a Pi command for SESSIONID in DIRECTORY.

GENERATED-SESSION-ID is used for a fresh session."
  (let ((prefix (agent-tui--command-prefix directory))
        (generated-session-id (or generated-session-id
                                  agent-tui-pi--session-id
                                  (agent-tui-pi--new-session-id))))
    (concat prefix
            agent-tui-pi-command
            (if (and sessionid (not (string-empty-p sessionid)))
                (concat " --session " (shell-quote-argument sessionid))
              ;; Supplying an ID for fresh sessions lets us report it without
              ;; depending on timing or terminal rendering.
              (concat " --session-id "
                      (shell-quote-argument generated-session-id))))))

(defun agent-tui-pi--process-session-id (buffer)
  "Return a session ID found in BUFFER's Pi process command line."
  (with-current-buffer buffer
    (let ((command (and (process-live-p (get-buffer-process buffer))
                        (process-command (get-buffer-process buffer)))))
      (when (listp command)
        (cl-loop for option in '("--session" "--session-id")
                 for tail = (member option command)
                 when (and tail (cadr tail))
                 return (cadr tail))))))

(defun agent-tui-pi--buffer-session-id (buffer &optional start)
  "Return the latest Pi session ID rendered in BUFFER, or nil.

When START is non-nil, only text inserted at or after START is searched."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let* ((contents (agent-tui--strip-terminal-control-sequences
                        (buffer-substring-no-properties
                         (or start (point-min)) (point-max))))
             (contents (replace-regexp-in-string "\\r" "" contents))
             (regexp
              "^[[:space:]]*ID:[[:space:]]*\\([[:xdigit:]]\\{8\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{4\\}-[[:xdigit:]]\\{12\\}\\)")
             (position 0)
             session-id)
        (while (string-match regexp contents position)
          (setq session-id (match-string 1 contents)
                position (match-end 0)))
        session-id))))

(defun agent-tui-pi--query-session-id (buffer)
  "Ask Pi for its current session ID in BUFFER.

Pi commands such as `/new' and `/fork' can replace the session without
changing the process command line, so the startup ID is only a fallback."
  (with-current-buffer buffer
    (unless agent-tui-pi--session-query-in-progress
      (let ((process (get-buffer-process buffer))
            (start (point-max))
            (deadline (+ (float-time)
                         (max 0 agent-tui-pi-session-query-timeout)))
            session-id)
        (when (process-live-p process)
          (setq agent-tui-pi--session-query-in-progress t)
          (unwind-protect
              (progn
                (agent-tui--send-input "/session")
                ;; Wait until the terminal has processed the command.  Search
                ;; only output added by this request so an earlier `/session'
                ;; response cannot be mistaken for the current one.
                (while (and (process-live-p process)
                            (<= (float-time) deadline)
                            (null (setq session-id
                                        (agent-tui-pi--buffer-session-id
                                         buffer start))))
                  (accept-process-output process 0.05))
                (or session-id
                    (agent-tui-pi--buffer-session-id buffer)))
            (setq agent-tui-pi--session-query-in-progress nil)))))))

(cl-defmethod agent-tui--start ((provider (eql 'pi))
                                &optional prefix-key sessionid)
  "Start Pi in the configured agent-tui terminal."
  (ignore provider)
  (let* ((directory default-directory)
         (sessionid (and sessionid
                         (not (string-empty-p sessionid))
                         sessionid))
         (pi-session-id (or sessionid (agent-tui-pi--new-session-id)))
         (buffer (agent-tui--start-terminal
                  (agent-tui-pi--command sessionid directory pi-session-id)
                  prefix-key)))
    ;; The terminal helper creates a different current buffer, so the session
    ;; ID must be copied explicitly into the returned terminal buffer.
    (with-current-buffer buffer
      (setq-local agent-tui-pi--session-id pi-session-id))
    buffer))

(cl-defmethod agent-tui--started ((provider (eql 'pi)) buffer)
  "Perform post-start setup for Pi in BUFFER."
  (ignore provider)
  ;; Pi resumes using a command-line argument, so unlike some TUIs it does
  ;; not need a synthetic `/resume' key sequence after startup.
  (with-current-buffer buffer
    (unless agent-tui-pi--session-id
      (setq-local agent-tui-pi--session-id
                  (or (agent-tui-pi--process-session-id buffer) ""))))
  (cl-call-next-method))

(cl-defmethod agent-tui--busy-p ((provider (eql 'pi)) last-buffer-contents)
  "Return non-nil when Pi's rendered output says it is working."
  (ignore provider)
  ;; Pi displays one of these transient status rows immediately above the
  ;; editor while an agent turn is running.  Restricting the match to a line
  ;; beginning avoids matching ordinary prose in an assistant response.
  (let ((case-fold-search t))
    (string-match-p
     "^[[:space:]]*[^[:alnum:][:space:]]?[[:space:]]*\\(?:working\\|thinking\\|compacting\\|retrying\\|executing\\)\\(?:[.]\\{3\\}\\|…\\)?[[:space:]]*$"
     last-buffer-contents)))

(cl-defmethod agent-tui--get-sessionid ((provider (eql 'pi)) buffer)
  "Return Pi's current session ID for BUFFER."
  (ignore provider)
  (with-current-buffer buffer
    (let ((session-id
           (condition-case nil
               (agent-tui-pi--query-session-id buffer)
             (error nil))))
      (when (and session-id (not (string-empty-p session-id)))
        (setq-local agent-tui-pi--session-id session-id))
      (or agent-tui-pi--session-id
          (agent-tui-pi--process-session-id buffer)
          ""))))

;;;###autoload
(defun agent-tui-pi-start (&optional prefix-key sessionid)
  "Start Pi's terminal UI.

With a prefix argument, request a new terminal session.  If SESSIONID is
non-empty, resume that Pi session."
  (interactive "P")
  (let ((agent-tui-provider 'pi))
    (agent-tui-start prefix-key sessionid)))

(provide 'agent-tui-pi)

;;; agent-tui-pi.el ends here
