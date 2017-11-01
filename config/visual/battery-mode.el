;; Set the format to percentage and add a space in front of it.
(setq-default battery-mode-line-format " %p")

;; Battery will be hidden above this limit.
(setq-default battery-mode-line-limit 90)

;; https://superuser.com/questions/877677/programatically-determine-if-an-script-is-being-executed-on-laptop-or-desktop
;; /sys/class/dmi/id/chassis_type
;; 1 Other
;; 2 Unknown
;; 3 Desktop
;; 4 Low Profile Desktop
;; 5 Pizza Box
;; 6 Mini Tower
;; 7 Tower
;; 8 Portable
;; 9 Laptop
;; 10 Notebook
;; 11 Hand Held
;; 12 Docking Station
;; 13 All in One
;; 14 Sub Notebook
;; 15 Space-Saving
;; 16 Lunch Box
;; 17 Main System Chassis
;; 18 Expansion Chassis
;; 19 SubChassis
;; 20 Bus Expansion Chassis
;; 21 Peripheral Chassis
;; 22 Storage Chassis
;; 23 Rack Mount Chassis
;; 24 Sealed-Case PC

(defun get-device-type ()
  "Return the dim device type of the system."
  (with-temp-buffer
    (insert-file-contents "/sys/class/dmi/id/chassis_type")
    (buffer-string)))

(defun enable-battery-mode-if-needed ()
  "Checks wether the system needs the battery status displayed."
  (let ((device-type (string-to-int (get-device-type))))
    (when (member device-type '(5 8 9 10 11 14 16))
      (display-battery-mode))))

(add-hook 'after-init-hook 'enable-battery-mode-if-needed)
