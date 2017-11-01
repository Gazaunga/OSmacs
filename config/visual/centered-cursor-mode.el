(defvar centered-cursor-ignore-modes
  (list
   'Info-mode
   'term-mode
   'eshell-mode
   'shell-mode
   'erc-mode
   'exwm-mode
   )
  "These modes won't get centered cursor mode.")

(use-package centered-cursor-mode
  :config
  ;; disable in terminal modes http://stackoverflow.com/a/6849467/519736
  ;; also disable in Info mode, because it breaks going back with the backspace key
  (define-global-minor-mode my-global-centered-cursor-mode centered-cursor-mode
    (lambda ()
      (when (not (memq major-mode centered-cursor-ignore-modes))
	(centered-cursor-mode))))
  (my-global-centered-cursor-mode 1)
)
