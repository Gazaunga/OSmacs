(use-package bind-key)

(defun mf/read-file (filename)
  "Return the contents of a file."
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents filename)
      (buffer-string))))

(defun mf/read-lines (filename)
  "Return a list of lines in a given file"
   (split-string (mf/read-file filename) "\n"))

(defun mf/enable-god-mode ()
  (god-mode))

(defun mf/switch-god-mode () "Enter gode mode and show which-key top level or exit god mode."
       (interactive)
       (if (bound-and-true-p god-mode)
	   (god-mode)
	 (mf/enable-god-mode)))


(defun mf/bind-key
    (command category package &optional function additional)
  "Bind a key to C-? and S-? …"
  (let ((keylist (list category package function additional)))
    (global-set-key (kbd (concat "s-SPC " (string-join keylist " "))) command)
    (global-set-key (kbd (concat "C-, " (string-join keylist " "))) command)))

(defun mf/easy-bind (command keychord)
  "Bind a keychord to the prefix map."
  (apply 'mf/bind-key command (mapcar (lambda (c) (char-to-string c)) keychord)))

(defun mf/monitor-from-line (line)
  "Look at a line and return the monitor if it is a matching xrandr line."
  (save-match-data
    (and (string-match " connected" line)
	 (string-match "^[a-zA-Z]+-[0-9]+" line)
	 (match-string 0 line ))))

(defun mf/get-monitor-list ()
  "Get a list of connected displays."
  (let ((xrandr (split-string (shell-command-to-string "xrandr") "\n" ))
	(result '()))
    (dotimes (counter (length xrandr) result)
      (let ((monitor (mf/monitor-from-line (nth counter xrandr))))
	(when monitor
	  (setq result (append result (list monitor))))))))

(defun mf/make-workspace-list (monitors workspaces)
  "Return a list for exwm to use for workspace assignment."
  (let ((result '()))
    (dotimes (count (* (length monitors) workspaces) result)
      (setq result (append result
	       (list (+ count 1)
		     (nth (if (evenp count) count (- count 1))
			  monitors)))))))

(defun app/command (command)
  (let ((splitted (split-string command " ")))
    (apply 'start-process (append
			   (list (nth 0 splitted) nil )
			   splitted))))

(defun app/physlock ()
	  "Lock the screen using physlock."
	  (interactive)
	  (app/command "physlock -s -u karl"))

(defun app/toggle-mute (&optional sink)
	  "Toggle the pulse audio mute status."
	  (interactive)
	  (app/command (concat "pactl set-sink-mute " (if sink sink "0") " toggle")))

(defun app/raise-volume (&optional sink)
	  "Raise the pulse audio volume"
	  (interactive)
	  (app/command (concat "pactl set-sink-volume " (if sink sink "0") " +5%")))

(defun app/lower-volume (&optional sink)
	  "Lower the pulse audio volume"
	  (interactive)
	  (app/command (concat "pactl set-sink-volume " (if sink sink "0") " -5%")))

(defun app/light-up ()
	  "Raise the brightness of the monitors."
	  (interactive)
	  (app/command "light -b -A 5"))

(defun app/light-down ()
	  "Lower the brigthness of the monitors."
	  (interactive)
	  (app/command "light -b -U 5"))

(defun app/run (command)
  "Run an application."
  (interactive (list (read-shell-command "$ ")))
  (start-process-shell-command command nil command))

(defun mf/get-auto-xrandr-line (display)
  "Returns the parameter for xrandr to automatically configure the display."
  (concat "--output " display " --auto"))

(defun mf/generate-xrandr-params ()
  "Generate the parameters for xrandr."
  (let ((xrandrfile "~/.xrandrrc"))
   (if (not (file-exists-p xrandrfile))
       (apply 'concat (mapcar 'mf/get-auto-xrandr-line (mf/get-monitor-list)))
     (with-temp-puffer
      (insert-file-contents xrandrfile)
      (buffer-to-string)))))

(defun app/configure-displays ()
  "Configure the attached displays"
  (interactive)
  (let ((xrandr-params (mf/generate-xrandr-params)))
    (start-process-shell-command "xrandr" nil (concat "xrandr " xrandr-params))))

(defun mf/rename-buffer ()
  (interactive)
  (exwm-workspace-rename-buffer
   (concat exwm-class-name " -> " exwm-title)))

(setq power-management-actions
      (list "poweroff"
	    "reboot"))

(defun power-management (action)
  (interactive (list (ivy-completing-read "systemctl " power-management-actions)))
  (shell-command (format "systemctl %s" action) nil nil))

(defun app/autostart (application)
  "Add an application to autostart."
  (add-hook 'exwm-init-hook
	    `(lambda ()
	       (start-process-shell-command "autostart-process" nil ,application))))

(defvar program-list '()
  "The programs which can be started with the execution command.")

(defun mf/list-desktop-files ()
  "Get a list of all desktop files on the system."
  (remove-if (lambda (element) (> 0 (length element)))
	     (split-string
	      (shell-command-to-string "find / -iname \"*.desktop\" 2>/dev/null"))))

(defun mf/needed-desktop-line (line)
  "Returns non-nil if the line is needed for processing the desktop file."
  (and (not (string-match "^Terminal=false" line))
       (not (or (string-match "^Name=" line)
		(string-match "^Exec=" line)))))


(defun mf/get-desktop-file-content (filename)
    "Return name and exec from a .desktop file. 
If those elements are not given nil will be returned.
Only gui applications will be listed."
  (let ((content (mf/read-lines filename)))
    (remove-if (lambda (element) (< (length element) 3))
	       (list (remove-if 'mf/needed-desktop-line content)))))

(defun mf/clean-desktop-exec-line (line)
  "Clean the Exec= line of a desktop file"
  (string-trim (replace-regexp-in-string
		"--" "" (replace-regexp-in-string
			 "%." "" (replace-regexp-in-string
				  "^Exec=" "" line)))))

(defun mf/get-desktop-file-name (object)
  "Return the name of a desktop file desktop."
  (replace-regexp-in-string "^Name=" "" (first object)))

(defun mf/get-desktop-file-exec (object)
  "Return the exec of a desktop file."
  (mf/clean-desktop-exec-line (second object)))

(defun mf/clean-desktop-file-content (filename)
  "Return the name and exec of a desktop file."
  (let ((content (mf/get-desktop-file-content filename)))
    (mapcar (lambda (data) (list (mf/get-desktop-file-name data)
				 (mf/get-desktop-file-exec data))) content)))

				
(defun mf/update-desktop-files ()
  "Update the desktop file database."
  (interactive)
  (setq program-list nil)
  (mapcar (lambda (element)
	    (if program-list
		(add-to-list 'program-list element)
	      (setq program-list (list element))))
	  (delete nil (mapcar 'mf/clean-desktop-file-content (mf/list-desktop-files)))))

(defun mf/get-desktop-application-names ()
  "Return all found application names"
  (mapcar (lambda (element) (first (first element))) program-list))

(defun mf/get-desktop-application-executable (applicationname)
  "Get the executable of an application based on its name."
  (second (first (first (remove-if (lambda (element)
			       (not (string= (first (first element)) applicationname)))
			     program-list)))))

(defun app/start (application)
  "Start the given application."
  (interactive (list (ivy-completing-read "$ " (mf/get-desktop-application-names))))
  (if application
      (start-process-shell-command application nil (mf/get-desktop-application-executable application))))

(defun mf/add-bookmark (name)
  "Add a named bookmark."
  (interactive (list (read-string "Bookmark Name: ")))
  (bookmark-set name t))
