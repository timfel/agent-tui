;;; agent-tui-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t; -*-

;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory (or load-file-name buffer-file-name))
                             (car load-path))))

;;;###autoload
(autoload 'agent-tui-start "agent-tui" "Start the selected agent TUI." t)

;;;###autoload
(autoload 'agent-tui-started "agent-tui" "Run post-start setup for an agent TUI buffer." t)

;;;###autoload
(autoload 'agent-tui-busy? "agent-tui" "Return non-nil if the current agent TUI is busy." nil)

;;;###autoload
(autoload 'agent-tui-get-sessionid "agent-tui" "Return an agent TUI session id." nil)

;;;###autoload
(autoload 'agent-tui-enqueue-prompt "agent-tui"
  "Queue a prompt for the next idle transition." t)

;;;###autoload
(autoload 'agent-tui-pi-start "agent-tui-pi" "Start Pi's terminal UI." t)

(provide 'agent-tui-autoloads)

;;; agent-tui-autoloads.el ends here
