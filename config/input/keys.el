(use-package bind-key
  :bind
  ("C-x C-k" . kill-this-buffer)
  ("C-x C-0" . delete-window)
  ("C-x C-1" . delete-other-windows)
  ("C-x C-2" . split-window-right)
  ("C-x C-3" . split-window-below)
  ("C-x C-+" . balance-windows)
  :config
  (defalias 'yes-or-no-p 'y-or-n-p))
