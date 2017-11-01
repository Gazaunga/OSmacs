(use-package projectile
  :config
  (mf/bind-key 'projectile-switch-project "h" "p" "s")
  (mf/bind-key 'projectile-find-file "h" "p" "f")
  (projectile-mode)
  (use-package counsel-projectile
    :config
    (counsel-projectile-on)))
