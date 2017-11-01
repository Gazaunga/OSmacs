(use-package ace-window
  :config
  (setq aw-scope 'frame)
  (setq-default ace-dispatch-always t)
  (mf/bind-key 'ace-window "w" "a"))
