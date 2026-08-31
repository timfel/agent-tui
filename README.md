# agent-tui

Run coding-agent TUIs in Emacs terminal buffers.  The core is provider-neutral;
`agent-tui-pi.el` provides Pi support.

## Basic setup

```elisp
(require 'agent-tui-pi)
(setq agent-tui-provider 'pi)
(global-set-key (kbd "C-x a p") #'agent-tui-pi-start)
```

Optional focused integrations are separately loadable:

```elisp
(require 'agent-tui-bwrap)
(agent-tui-bwrap-mode 1)

(require 'agent-tui-fanout)
(setq agent-tui-fanout-provider 'pi)

(require 'agent-tui-context)
(global-set-key (kbd "C-x a c") #'agent-tui-enqueue-context-prompt)

(require 'agent-tui-dashboard)
(global-set-key (kbd "C-x a a") #'agent-tui-dashboard)

(require 'agent-tui-bookmark)
(require 'agent-tui-ol)

(require 'agent-tui-desktop)
(agent-tui-desktop-mode 1)
```

Bookmarks and Org links record the provider, working directory, and session
ID.  Desktop integration restores active TUI sessions across Emacs restarts.

`agent-tui-fanout-worktrees` creates one TUI per task below
`.agent-tui/worktrees`.  `agent-tui-jira-investigate-marked` adds the small
Jira-to-fanout integration and requires the local Jira package:

```elisp
(require 'agent-tui-jira)
```


Context is opt-in: `agent-tui-enqueue-context-prompt` appends selected Emacs,
region, VC, and optional Magit context to one prompt.  The dashboard tracks
live TUIs and local worktrees without fetching remote Jira or pull-request
metadata.
