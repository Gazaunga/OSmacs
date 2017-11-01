(use-package org
  :config
  (mf/bind-key 'org-agenda "f" "o" "a")
  (mf/bind-key 'org-capture "f" "o" "a")
  (use-package projectile
    :config
    (mapcar (lambda (project) "Add all projectile folders to org agenda."
	  (when (file-exists-p project)
	   (add-to-list 'org-agenda-files (replace-regexp-in-string "/$" "" project))))
	    (projectile-relevant-known-projects)))
  
  (setq org-support-shift-select t)
  (setq org-export-backends
	(quote (ascii html icalendar latex md odt)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t))))
