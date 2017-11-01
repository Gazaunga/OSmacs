(use-package circadian
  :config
  (use-package color-theme-sanityinc-tomorrow
    :config 
    (use-package anti-zenburn-theme
      :config
      (setq circadian-themes '((:sunrise . sanityinc-tomorrow-day)
			      (:sunset . anti-zenburn)))))
  (circadian-setup))
