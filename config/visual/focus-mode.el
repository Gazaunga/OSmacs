(use-package focus
  :config
  (mf/bind-key 'focus-mode "v" "f")
  (add-hook 'after-init-hook (lambda () (focus-mode))))
