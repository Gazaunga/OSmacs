(require 'button)

(defun dashboard-insert-custom (dashboard)
  (insert "Shortcuts\n")
  (insert "    ")
  (insert-button "ORG agenda"
		 'action (lambda (_) (org-agenda-list))
		 'follow-link t)
  (insert "\n    ")
  (insert-button "ORG todo"
		 'action (lambda (_) (org-todo-list))
		 'follow-link t)
  (insert "\n    ")
  (insert-button "Notmuch"
		 'action (lambda (_) (notmuch))
		 'follow-link t))


(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title
	(replace-regexp-in-string
	 "\n" ""
	 (format "Welcome to osmacs running on %s!"
		 (replace-regexp-in-string
		  ".*:\t*" ""
		  (shell-command-to-string "lsb_release -d"))
						     )))
  (setq dashboard-startup-banner (concat user-emacs-directory "/osmacs.png"))
  (setq dashboard-items '((projects . 5)))


  (add-to-list 'dashboard-item-generators  '(shortcuts . dashboard-insert-custom))
  (add-to-list 'dashboard-items '(shortcuts) t)
  )
