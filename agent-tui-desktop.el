;;; agent-tui-desktop.el --- Desktop restore support for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; Persist active agent TUI sessions through Emacs Desktop.  Terminal buffers
;; use several possible major modes, so the restore handler is installed for
;; the supported terminal modes and delegates ordinary terminal buffers to
;; the handler that would otherwise handle their mode.
;;
;;; Code:

(require 'desktop)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'agent-tui)

(defgroup agent-tui-desktop nil
  "Desktop restore support for `agent-tui'."
  :group 'agent-tui)

(defvar agent-tui-desktop-mode)
(defvar desktop-buffer-mode-handlers)
(defvar desktop-buffer-major-mode)
(defvar desktop-dirname)
(defvar-local desktop-save-buffer)

(defconst agent-tui-desktop--terminal-modes
  '(ghostel-mode vterm-mode eat-mode term-mode))

(defvar agent-tui-desktop--fallback-handlers nil
  "Handlers displaced while the agent-tui Desktop handler is installed.")

(defun agent-tui-desktop--install-handler (mode)
  "Install the agent-tui handler for MODE ahead of other handlers.

Several terminal packages install their own Desktop handler when their mode
library is loaded.  Desktop uses the first matching alist entry, so retain
that handler as a fallback for ordinary terminal buffers while ensuring
agent-tui entries are handled by this package."
  (let ((existing (cdr (assq mode desktop-buffer-mode-handlers))))
    (when (and existing
               (not (eq existing #'agent-tui-desktop--restore-buffer)))
      (let ((fallback (assq mode agent-tui-desktop--fallback-handlers)))
        (if fallback
            (setcdr fallback existing)
          (push (cons mode existing) agent-tui-desktop--fallback-handlers)))))
  (setq desktop-buffer-mode-handlers
        (cons (cons mode #'agent-tui-desktop--restore-buffer)
              (seq-remove (lambda (entry) (eq (car entry) mode))
                          desktop-buffer-mode-handlers))))

(defun agent-tui-desktop--after-load (_file)
  "Keep agent-tui Desktop handlers first after a library is loaded."
  (when agent-tui-desktop-mode
    (dolist (mode agent-tui-desktop--terminal-modes)
      (agent-tui-desktop--install-handler mode))))

(defun agent-tui-desktop--setup-buffer ()
  "Configure the current TUI buffer for Desktop saving."
  (when agent-tui-desktop-mode
    (setq-local desktop-save-buffer #'agent-tui-desktop--save-buffer)))

(defun agent-tui-desktop--clear-buffer ()
  "Remove this package's Desktop save function from the current buffer."
  (when (eq desktop-save-buffer #'agent-tui-desktop--save-buffer)
    (kill-local-variable 'desktop-save-buffer)))

(defun agent-tui-desktop--setup-existing-buffers ()
  "Configure existing TUI buffers for Desktop saving."
  (dolist (buffer (agent-tui-buffers))
    (with-current-buffer buffer
      (agent-tui-desktop--setup-buffer))))

(defun agent-tui-desktop--clear-existing-buffers ()
  "Remove this package's Desktop save function from existing TUIs."
  (dolist (buffer (agent-tui-buffers))
    (with-current-buffer buffer
      (agent-tui-desktop--clear-buffer))))

(defun agent-tui-desktop--save-buffer (desktop-directory)
  "Return Desktop restore data for the current TUI.
DESKTOP-DIRECTORY is where Desktop writes its state."
  (when (and agent-tui-desktop-mode
             (agent-tui--active-buffer-p (current-buffer)))
    (let ((session-id (agent-tui-get-sessionid (current-buffer)))
          (provider (agent-tui-provider-for-buffer)))
      (when (and provider (not (string-empty-p session-id)))
        `((:agent-tui . t)
          (:directory . ,(desktop-file-name
                          (agent-tui-cwd)
                          desktop-directory))
          (:session-id . ,session-id)
          (:provider . ,provider)
          (:terminal . ,agent-tui--terminal)
          (:buffer-name . ,(buffer-name)))))))

(defun agent-tui-desktop--existing-buffer (session-id provider)
  "Return a live TUI for SESSION-ID and PROVIDER, or nil."
  (seq-find
   (lambda (buffer)
     (and (equal provider
                 (buffer-local-value 'agent-tui--provider buffer))
          (condition-case nil
              (equal session-id (agent-tui-get-sessionid buffer))
            (error nil))))
   (agent-tui-buffers)))

(defun agent-tui-desktop--restore-message-buffer (buffer-name message)
  "Return a scratch BUFFER-NAME containing restore MESSAGE."
  (let ((buffer (generate-new-buffer
                 (or buffer-name "*agent-tui desktop restore*"))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (insert "Could not restore agent-tui buffer.\n\n" message "\n"))
      (special-mode)
      (setq-local desktop-save-buffer nil))
    buffer))

(defun agent-tui-desktop--restore-buffer
    (buffer-filename buffer-name buffer-misc)
  "Restore an agent-tui buffer from Desktop BUFFER-MISC.
Non-agent terminal buffers are delegated to their displaced handler or
Desktop's default handler."
  (if (not (eq (map-elt buffer-misc :agent-tui) t))
      (let ((handler (cdr (assq desktop-buffer-major-mode
                               agent-tui-desktop--fallback-handlers))))
        (if handler
            (funcall handler buffer-filename buffer-name buffer-misc)
          (desktop-restore-file-buffer buffer-filename buffer-name buffer-misc)))
    (let* ((session-id (map-elt buffer-misc :session-id))
           (provider-value (map-elt buffer-misc :provider))
           (provider (or provider-value agent-tui-provider))
           (saved-directory (map-elt buffer-misc :directory))
           (directory (and saved-directory
                           (file-name-as-directory
                            (expand-file-name saved-directory
                                              (or desktop-dirname
                                                  default-directory)))))
           (terminal (map-elt buffer-misc :terminal))
           (restore-buffer-name (or (map-elt buffer-misc :buffer-name)
                                    buffer-name)))
      (cond
       ((not agent-tui-desktop-mode)
        (agent-tui-desktop--restore-message-buffer
         restore-buffer-name "Agent TUI Desktop restore is disabled."))
       ((not session-id)
        (agent-tui-desktop--restore-message-buffer
         restore-buffer-name "Desktop entry has no TUI session ID."))
       ((not directory)
        (agent-tui-desktop--restore-message-buffer
         restore-buffer-name "Desktop entry has no TUI directory."))
       ((not (file-directory-p directory))
        (agent-tui-desktop--restore-message-buffer
         restore-buffer-name
         (format "TUI Desktop directory is not available: %s" directory)))
       ((not provider)
        (agent-tui-desktop--restore-message-buffer
         restore-buffer-name "Desktop entry has no TUI provider."))
       (t
        (let ((buffer (or (agent-tui-desktop--existing-buffer
                           session-id provider)
                          (let ((agent-tui-terminal
                                 (or terminal agent-tui-terminal)))
                            (agent-tui-start-in-directory
                             provider directory t session-id t)))))
          (when restore-buffer-name
            (with-current-buffer buffer
              (rename-buffer restore-buffer-name t)))
          buffer))))))

(defun agent-tui-desktop--enable ()
  "Install Desktop integration hooks and handlers."
  (add-hook 'agent-tui-started-hook #'agent-tui-desktop--setup-buffer)
  ;; A terminal package can add its handler later, while desktop.el is loading
  ;; the mode for the first restored buffer.  Reassert our handler afterward.
  (add-hook 'after-load-functions #'agent-tui-desktop--after-load)
  (dolist (mode agent-tui-desktop--terminal-modes)
    (agent-tui-desktop--install-handler mode))
  (agent-tui-desktop--setup-existing-buffers))

(defun agent-tui-desktop--disable ()
  "Remove Desktop integration hooks and handlers."
  (remove-hook 'agent-tui-started-hook #'agent-tui-desktop--setup-buffer)
  (remove-hook 'after-load-functions #'agent-tui-desktop--after-load)
  (dolist (mode agent-tui-desktop--terminal-modes)
    (setq desktop-buffer-mode-handlers
          (seq-remove
           (lambda (entry)
             (and (eq (car entry) mode)
                  (eq (cdr entry) #'agent-tui-desktop--restore-buffer)))
           desktop-buffer-mode-handlers))
    (when-let* ((fallback (assq mode agent-tui-desktop--fallback-handlers)))
      (push fallback desktop-buffer-mode-handlers)))
  (setq agent-tui-desktop--fallback-handlers nil)
  (agent-tui-desktop--clear-existing-buffers))

;;;###autoload
(define-minor-mode agent-tui-desktop-mode
  "Persist active agent TUI sessions with Emacs Desktop.

Enable this before Desktop restores buffers."
  :global t
  :group 'agent-tui-desktop
  (if agent-tui-desktop-mode
      (agent-tui-desktop--enable)
    (agent-tui-desktop--disable)))

;;;###autoload
(defun agent-tui-desktop-version ()
  "Show the agent-tui Desktop integration version."
  (interactive)
  (message "agent-tui-desktop v0.1.0"))

(provide 'agent-tui-desktop)

;;; agent-tui-desktop.el ends here
