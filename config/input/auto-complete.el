(use-package auto-complete
  :config
  (ac-config-default)
  (setq ac-auto-start nil)
  (define-key ac-completing-map "\t" 'ac-complete)
  (define-key ac-completing-map "\r" nil)
  (setq-default ac-auto-show-menu nil))
