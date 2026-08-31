;;; agent-tui-jira.el --- Jira fan-out helpers for agent-tui -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Tim Felgentreff

;;; Commentary:
;;
;; Launch focused agent TUI investigations from marked Jira issues.  This
;; intentionally does not add Jira link handling to terminal output.
;;
;;; Code:

(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'tablist nil t)
(require 'agent-tui-fanout)

(defvar jira-detail--current-key)
(defvar jira-issues-key-summary-map)

(defgroup agent-tui-jira nil
  "Jira helpers for `agent-tui'."
  :group 'agent-tui)

(defun agent-tui-jira--marked-issue-ids ()
  "Return explicitly marked Jira issue IDs in the current buffer."
  (if (derived-mode-p 'jira-detail-mode)
      (and jira-detail--current-key (list jira-detail--current-key))
    (let (issue-ids)
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((issue-id (tabulated-list-get-id))
                (mark-state (and (fboundp 'tablist-get-mark-state)
                                 (tablist-get-mark-state))))
            (when (and issue-id mark-state
                       (not (eq (car mark-state) ?\s)))
              (push issue-id issue-ids)))
          (forward-line 1)))
      (nreverse issue-ids))))

(defun agent-tui-jira--task (issue-id title)
  "Return a focused investigation task for ISSUE-ID and TITLE."
  (format
   (concat
    "Investigate Jira issue %s: %s\n\n"
    "Inspect the issue and this repository. Check whether it is already "
    "fixed, stale, or still actionable. If it is actionable, identify the "
    "root cause and propose a focused fix with validation. Do not change "
    "code yet.")
   issue-id title))

(defun agent-tui-jira--title (issue-id title)
  "Return a compact fan-out title for ISSUE-ID and TITLE."
  (let ((title (string-trim
                (replace-regexp-in-string "\\s-+" " " (or title "")))))
    (when (> (length title) 24)
      (setq title (concat (substring title 0 21) "...")))
    (if (string-empty-p title)
        issue-id
      (format "%s-%s" title issue-id))))

;;;###autoload
(defun agent-tui-jira-investigate-marked (&optional prompt?)
  "Start agent TUI investigations for marked Jira issues.

When PROMPT? is non-nil, ask for the task text for each issue."
  (interactive "P")
  (let ((issue-ids (agent-tui-jira--marked-issue-ids)))
    (if (null issue-ids)
        (message "No Jira issues are marked")
      (let ((directory
             (read-directory-name
              "Repository for agent TUI worktrees: "
              default-directory nil t)))
        (agent-tui-fanout-worktrees
         (mapcar
          (lambda (issue-id)
            (let* ((title (or (and (boundp 'jira-issues-key-summary-map)
                                   (gethash issue-id jira-issues-key-summary-map))
                              ""))
                   (task (agent-tui-jira--task issue-id title)))
              (cons (agent-tui-jira--title issue-id title)
                    (if prompt?
                        (read-string "Investigation task: " task)
                      task))))
          issue-ids)
         directory)))))

(provide 'agent-tui-jira)

;;; agent-tui-jira.el ends here
