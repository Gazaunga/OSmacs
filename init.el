;; Change the custom file, so "custom-set-variables" does not clutter up this file.
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Disable the start screen.
(setq-default inhibit-startup-screen t)
(setq-default inhibit-splash-screen t)

;; I don't wanna learn more about gnu.
(setq-default inhibit-startup-message t)

;; Show me what's going on.
(switch-to-buffer "*Messages*")

;; Use the straight package manager.
(let ((bootstrap-file (concat user-emacs-directory "straight/bootstrap.el"))
      (bootstrap-version 2))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Load my custom functions
(load-file (concat user-emacs-directory "functions.el"))

(defun load-directory (directory)
  "Load recursively all '.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

;; Make sure packages always get installed by default.
(setq use-package-always-ensure t)


;; If a ~/.secrets folder exists load all all .el files.
(when (file-exists-p "~/.secrets")
  (load-directory "~/.secrets"))

;; Load all folders in $EMACS/config
(mapcar (lambda (dir)
	  (unless (cl-search "." dir)
	    (load-directory (concat user-emacs-directory "config/" dir))))
	(directory-files (concat user-emacs-directory "config")))
