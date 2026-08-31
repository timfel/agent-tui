;;; agent-tui-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t; -*-

;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory (or load-file-name buffer-file-name))
                             (car load-path))))

;;;###autoload
(autoload 'agent-tui-start "agent-tui" "Start the selected agent TUI." t)

;;;###autoload
(autoload 'agent-tui-restart "agent-tui"
  "Close the current agent TUI and start a fresh session with its provider." t)

;;;###autoload
(autoload 'agent-tui-reload "agent-tui"
  "Close the current agent TUI and resume its session with its provider." t)

;;;###autoload
(autoload 'agent-tui-started "agent-tui" "Run post-start setup for an agent TUI buffer." t)

;;;###autoload
(autoload 'agent-tui-start-in-directory "agent-tui"
  "Start a provider in a directory and return its terminal buffer." nil)

;;;###autoload
(autoload 'agent-tui-buffers "agent-tui"
  "Return active agent-tui buffers in most-recently-used order." nil)

;;;###autoload
(autoload 'agent-tui-cwd "agent-tui" "Return a TUI buffer's working directory." nil)

;;;###autoload
(autoload 'agent-tui-status "agent-tui" "Return a TUI buffer's current status." nil)

;;;###autoload
(autoload 'agent-tui-busy? "agent-tui" "Return non-nil if the current agent TUI is busy." nil)

;;;###autoload
(autoload 'agent-tui-get-sessionid "agent-tui" "Return an agent TUI session id." nil)

;;;###autoload
(autoload 'agent-tui-enqueue-prompt "agent-tui"
  "Send a prompt immediately if idle, or queue it while busy." t)

;;;###autoload
(autoload 'agent-tui-bwrap-command-prefix "agent-tui-bwrap"
  "Return an argv prefix for a TUI launched in a directory." nil)

;;;###autoload
(autoload 'agent-tui-bwrap-mode "agent-tui-bwrap"
  "Use the Bubblewrap command prefix for TUI commands." t)

;;;###autoload
(autoload 'agent-tui-fanout-worktrees "agent-tui-fanout"
  "Start one TUI per fan-out task." t)

;;;###autoload
(autoload 'agent-tui-fanout-delete-worktree "agent-tui-fanout"
  "Remove Git worktree links below a directory and delete it." nil)

;;;###autoload
(autoload 'agent-tui-fanout-worktree-folder-p "agent-tui-fanout"
  "Return non-nil when a directory uses the configured fan-out layout." nil)

;;;###autoload
(autoload 'agent-tui-fanout-cleanup-worktree "agent-tui-fanout"
  "Delete the current fan-out worktree parent after confirmation." t)

;;;###autoload
(autoload 'agent-tui-jira-investigate-marked "agent-tui-jira"
  "Start agent TUI investigations for marked Jira issues." t)

;;;###autoload
(autoload 'agent-tui-bookmark-handler "agent-tui-bookmark"
  "Handle an agent-tui bookmark." nil)

;;;###autoload
(autoload 'agent-tui-ol-follow "agent-tui-ol"
  "Follow an agent-tui Org link." nil)

;;;###autoload
(autoload 'agent-tui-context-prompt "agent-tui-context"
  "Return a prompt with explicitly requested Emacs context appended." nil)

;;;###autoload
(autoload 'agent-tui-enqueue-context-prompt "agent-tui-context"
  "Append Emacs context to a prompt and send it to a relevant TUI." t)

;;;###autoload
(autoload 'agent-tui-dashboard "agent-tui-dashboard"
  "Open the agent TUI dashboard." t)

;;;###autoload
(autoload 'agent-tui-dashboard-refresh "agent-tui-dashboard"
  "Refresh the current agent TUI dashboard." t)

;;;###autoload
(autoload 'agent-tui-dashboard-visit "agent-tui-dashboard"
  "Visit the current TUI, starting one when necessary." t)

;;;###autoload
(autoload 'agent-tui-dashboard-rename-buffer "agent-tui-dashboard"
  "Rename a live TUI buffer on the current row." t)

;;;###autoload
(autoload 'agent-tui-dashboard-find-file "agent-tui-dashboard"
  "Find a file below the current row's folder." t)

;;;###autoload
(autoload 'agent-tui-dashboard-delete-worktree "agent-tui-dashboard"
  "Delete the current fan-out worktree after confirmation." t)

;;;###autoload
(autoload 'agent-tui-desktop-mode "agent-tui-desktop"
  "Persist active agent TUI sessions with Emacs Desktop." t)

;;;###autoload
(autoload 'agent-tui-desktop-version "agent-tui-desktop"
  "Show the agent-tui Desktop integration version." t)

;;;###autoload
(autoload 'agent-tui-pi-start "agent-tui-pi" "Start Pi's terminal UI." t)

(provide 'agent-tui-autoloads)

;;; agent-tui-autoloads.el ends here
