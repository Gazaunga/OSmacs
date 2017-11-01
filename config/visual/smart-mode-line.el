(use-package smart-mode-line
  :config
  (sml/setup)
  (add-hook 'after-init-hook (lambda ()  (sml/apply-theme 'respectful))))
