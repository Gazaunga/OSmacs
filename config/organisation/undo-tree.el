(use-package undo-tree
  :bind
  ("C-x C-u" . undo-tree-undo)
  :config
  (mf/bind-key 'undo-tree-undo "o" "u" "u")
  (mf/bind-key 'undo-tree-visualize "o" "u" "v")
  (global-undo-tree-mode))
