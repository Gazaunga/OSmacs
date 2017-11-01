(defcustom exwm-autostart '()
  "A list of applications that will be started after exwm finished loading.")

(defcustom exwm-workspaces-per-monitor 2
  "The number of workspaces that will be added per monitor.")

(setq exwm-autostart (list
	       	      "run_keybase"
		      "setxkbmap -option ctrl:swap_lalt_lctl de bone && xmodmap ~/.Xmodmap"
		      ))

(defun my-exwm-autostart ()
  "Add applications that will be loaded after exwm init is done."
  (mapcar (lambda (program) (app/autostart program)) exwm-autostart)
  ;; This adds fswebcam to be used for the auto brightness
  (when (and (= 0 (shell-command "which fswebcam" nil nil))
	     (file-exists-p "/dev/video0"))
    (app/autostart "fswebcam -l 60 --png 9 --no-banner --greyscale --save ~/.light"))
  )

(defun my-exwm-keybindings ()
  "Add the key bindings for exwm."
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 'app/lower-volume)
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") 'ignore)
  (exwm-input-set-key (kbd "<XF86AudioMute>") 'app/toggle-mute)
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 'app/raise-volume)
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 'app/light-down)
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 'app/light-up)
  (exwm-input-set-key (kbd "s-,") 'ace-window)
  (exwm-input-set-key (kbd "s-SPC s-SPC") 'app/start)
  (exwm-input-set-key (kbd "s-l") 'app/physlock)
  (exwm-input-set-key (kbd "s-n") 'next-buffer)
  (exwm-input-set-key (kbd "s-r") 'previous-buffer)
  (exwm-input-set-key (kbd "s-.") 'exwm-workspace-switch-to-buffer)
  (exwm-input-set-key (kbd "s-SPC w e r") 'exwm-reset)
  (exwm-input-set-key (kbd "s-SPC w e R") 'exwm-restart))

(defun my-exwm-systray ()
  "Adds the systray support to exwm."
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)
  (setq exwm-systemtray-height 16) ;; Required because of a bug.
  )

(defun my-exwm-workspaces ()
  "Calculates the number of workspaes and assigns them to monitors."
  (interactive)

  (require 'exwm-randr)
  (setq exwm-randr-workspace-output-plist
	(mf/make-workspace-list (mf/get-monitor-list) exwm-workspaces-per-monitor))
  (add-hook 'exwm-randr-screen-change-hook 'app/configure-displays)
  

  (dotimes (value (* exwm-workspaces-per-monitor (length (mf/get-monitor-list))))
    (let ((i (+ 1 value)))
       (exwm-input-set-key
       (kbd (format "s-%d" i))
       `(lambda () (interactive)
	  (exwm-workspace-switch-create ,i)))))
  (exwm-randr-enable))

(defun my-exwm-buffers ()
  "Adds some hooks so buffer names are better."
  (add-hook 'exwm-update-class-hook 'mf/rename-buffer)
  (add-hook 'exwm-update-title-hook 'mf/rename-buffer))

(defun load-my-exwm ()
  "This loads the exwm config. It's just a wrapper around some smaller functions."
  (my-exwm-autostart)
  (my-exwm-keybindings)
  (my-exwm-systray)
  (my-exwm-buffers)
  (my-exwm-workspaces)
  (add-hook 'exwm-init-hook 'mf/update-desktop-files)
  )


(use-package xelb
  :config
  (use-package exwm
    :config
    (load-my-exwm)
    (exwm-enable)))
