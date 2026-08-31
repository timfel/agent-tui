;;; agent-tui-bookmark.el --- Bookmark support for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; Make active agent TUIs bookmarkable.  A bookmark switches to an existing
;; buffer or resumes the recorded provider session in its original directory.
;;
;;; Code:

(require 'bookmark)
(require 'seq)
(require 'subr-x)
(require 'agent-tui)

(declare-function ivy-configure "ivy" (&rest args))

(defun agent-tui-bookmark-make-record ()
  "Create a bookmark record for the current agent TUI session."
  (let* ((session-id (agent-tui-get-sessionid (current-buffer)))
         (session-id (unless (string-empty-p session-id) session-id))
         (provider (agent-tui--provider-for-buffer))
         (buffer-name (buffer-name))
         (directory (agent-tui-cwd)))
    `(,buffer-name
      (handler . agent-tui-bookmark-handler)
      (location . ,directory)
      (buffer-name . ,buffer-name)
      (project-path . ,directory)
      (session-id . ,session-id)
      (provider . ,provider))))

(defun agent-tui-bookmark--provider (value)
  "Return a provider symbol from bookmark VALUE, or the global provider."
  (or (cond ((symbolp value) value)
            ((stringp value) (intern-soft value)))
      agent-tui-provider
      (user-error "No agent-tui provider is recorded in this bookmark")))

(defun agent-tui-bookmark--start (directory provider session-id buffer-name)
  "Start or resume PROVIDER in DIRECTORY using SESSION-ID."
  (let ((buffer
         (agent-tui-start-in-directory
          provider directory t session-id)))
    (when (and (buffer-live-p buffer) buffer-name)
      (with-current-buffer buffer
        (rename-buffer buffer-name t)))
    buffer))

;;;###autoload
(defun agent-tui-bookmark-handler (bookmark)
  "Handle an agent-tui BOOKMARK."
  (let* ((buffer-name (bookmark-prop-get bookmark 'buffer-name))
         (session-id (bookmark-prop-get bookmark 'session-id))
         (directory (or (bookmark-prop-get bookmark 'project-path)
                        (bookmark-location bookmark)))
         (provider (agent-tui-bookmark--provider
                    (bookmark-prop-get bookmark 'provider)))
         (buffer (and buffer-name (get-buffer buffer-name))))
    (cond
     ((and buffer (buffer-live-p buffer))
      (if (y-or-n-p (format "Buffer `%s' exists.  Switch to it? " buffer-name))
          (pop-to-buffer-same-window buffer)
        (agent-tui-bookmark--start directory provider session-id buffer-name)))
     (session-id
      (agent-tui-bookmark--start directory provider session-id buffer-name))
     (t
      (agent-tui-bookmark--start directory provider nil buffer-name)))))

(put 'agent-tui-bookmark-handler 'bookmark-handler-type "agent-tui")

(defun agent-tui-bookmark--setup ()
  "Set up bookmarking in the current agent TUI buffer."
  (setq-local bookmark-make-record-function
              #'agent-tui-bookmark-make-record))

(defun agent-tui-bookmark--setup-existing-buffers ()
  "Set up bookmarking in existing agent TUI buffers."
  (dolist (buffer (agent-tui-buffers))
    (with-current-buffer buffer
      (agent-tui-bookmark--setup))))

(add-hook 'agent-tui-started-hook #'agent-tui-bookmark--setup)
(agent-tui-bookmark--setup-existing-buffers)

(defun agent-tui-bookmark--counsel-transformer (candidate)
  "Annotate a COUNSEL bookmark CANDIDATE with type and location."
  (let* ((record (bookmark-get-bookmark-record candidate))
         (type (or (and record
                        (bookmark-type-from-full-record (cons candidate record)))
                   ""))
         (location (bookmark-location candidate)))
    (format "%-40s %-14s %s"
            candidate
            (propertize (if (string-empty-p type) "file" type)
                        'face 'font-lock-type-face)
            (propertize (or location "")
                        'face 'font-lock-comment-face))))

(with-eval-after-load 'counsel
  (with-eval-after-load 'ivy
    (ivy-configure 'counsel-bookmark
      :display-transformer-fn #'agent-tui-bookmark--counsel-transformer)))

(provide 'agent-tui-bookmark)

;;; agent-tui-bookmark.el ends here
