(defvar use-auto-light nil
  "If this is non-nil background brightness will be adjusted automatically.")

(defvar auto-light-min 10
  "The percentage of background brightness which should be used at least.")

(require 'filenotify)

(defun light/get-light-histogram ()
  "Get the histogram of the .light image."
  (nth 1 (split-string
	  (shell-command-to-string "convert .light +dither -colors 1 -unique-colors txt:") "\n")))

(defun light/get-gray-value-from-histogram (histogram)
  "Get the gray value number from a histogram"
  (string-to-int (nth 0 (split-string (save-match-data (string-match "[0-9]+)$" histogram)
						       (match-string 0 histogram))))))

(defun light/scale-255-to-percent (number)
  "Returns the percentage value of a number on a scale to 255"
  (let ((value (ceiling (* 100 (/ (float number) 255)))))
    (min 100 (max auto-light-min value))))

(defun light/set-auto-light ()
  "Set the light of the monitor automatically based on a .light source image."
  (interactive)
  (let ((gray-value (light/get-gray-value-from-histogram (light/get-light-histogram))))
    (shell-command (format "light -S -p %d" (+ 10 ;; We want it a bit brighter than the read value.
					       (light/scale-255-to-percent gray-value))))))


(defun light/file-changed (event)
  (when use-auto-light (light/set-auto-light))
  nil)

(defun toggle-auto-brigthness ()
  "Toggles the usage of auto brightness."
  (interactive)
  (setq use-auto-light (not use-auto-light))
  (if use-auto-light
      (message "Enabled automatic brightness.")
       (message "Disabled automatic brightness.")))

(when (and (file-exists-p "/dev/video0")
	   (file-exists-p "~/.light"))
  (file-notify-add-watch "~/.light" '(change) 'light/file-changed))
