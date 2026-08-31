;;; agent-tui-ol.el --- Org links for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; Store and follow links to agent-tui sessions.  The link records the
;; provider, working directory, and session ID; it does not depend on terminal
;; buffer names remaining unique.
;;
;;; Code:

(require 'ol)
(require 'seq)
(require 'subr-x)
(require 'agent-tui)

(defun agent-tui-ol--parse-path (path)
  "Parse PATH as DIRECTORY::SESSION-ID::PROVIDER."
  (when-let* ((pos1 (string-match "::" path))
              (pos2 (string-match "::" path (+ pos1 2))))
    (list :directory (substring path 0 pos1)
          :session-id (substring path (+ pos1 2) pos2)
          :provider (substring path (+ pos2 2)))))

(defun agent-tui-ol--provider (value)
  "Return a provider symbol from VALUE, or the global provider."
  (or (cond ((symbolp value) value)
            ((and (stringp value) (not (string-empty-p value)))
             (intern-soft value)))
      agent-tui-provider
      (user-error "No agent-tui provider is available for this link")))

(defun agent-tui-ol--find-buffer (session-id provider)
  "Find a live TUI for SESSION-ID and PROVIDER."
  (and (not (string-empty-p session-id))
       (seq-find
        (lambda (buffer)
          (and (equal session-id (agent-tui-get-sessionid buffer))
               (eq provider
                   (buffer-local-value 'agent-tui--provider buffer))))
        (agent-tui-buffers))))

;;;###autoload
(defun agent-tui-ol-store-link ()
  "Store an Org link to the current agent-tui buffer."
  (when (agent-tui--active-buffer-p (current-buffer))
    (let* ((directory (agent-tui-cwd))
           (session-id (agent-tui-get-sessionid (current-buffer)))
           (session-id (if (stringp session-id) session-id ""))
           (provider (agent-tui--provider-for-buffer))
           (link (format "agent-tui:%s::%s::%s"
                         directory session-id provider)))
      (org-link-store-props
       :type "agent-tui"
       :link link
       :description (buffer-name))
      link)))

;;;###autoload
(defun agent-tui-ol-follow (path _arg)
  "Follow an agent-tui Org link with PATH."
  (let ((parsed (agent-tui-ol--parse-path path)))
    (unless parsed
      (user-error "Invalid agent-tui link: %s" path))
    (let* ((directory (plist-get parsed :directory))
           (session-id (plist-get parsed :session-id))
           (provider (agent-tui-ol--provider (plist-get parsed :provider)))
           (buffer (agent-tui-ol--find-buffer session-id provider)))
      (cond
       (buffer
        (pop-to-buffer buffer))
       ((not (string-empty-p session-id))
        (agent-tui-start-in-directory provider directory t session-id))
       ((y-or-n-p "No session ID stored.  Start a new agent TUI? ")
        (agent-tui-start-in-directory provider directory t nil))
       (t
        (message "Org link not followed."))))))

(org-link-set-parameters "agent-tui"
                         :follow #'agent-tui-ol-follow
                         :store #'agent-tui-ol-store-link)

(provide 'agent-tui-ol)

;;; agent-tui-ol.el ends here
