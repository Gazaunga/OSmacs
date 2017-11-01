(use-package nov
  :config
;;  (add-to-list 'auto-mode-alist '("\\.epub\\" . nov-mode))
  (setq nov-text-width most-positive-fixnum)
  (setq visual-fill-column-center-text t)
  (add-hook 'nov-mode-hook 'visual-line-mode)
  (add-hook 'nov-mode-hook 'visual-fill-column-mode)
  )
