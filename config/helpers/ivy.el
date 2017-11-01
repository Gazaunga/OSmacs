(use-package ivy
  :config
  (mf/bind-key 'ivy-switch-buffer "h" "i" "s")
  (use-package counsel)
  (use-package flx)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))
	  
