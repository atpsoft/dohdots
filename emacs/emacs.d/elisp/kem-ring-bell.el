;; --------------------------------------------------------
;; nice little alternative visual bell; Miles Bader <miles /at/ gnu.org>

(defcustom echo-area-bell-string "*DING* " ;"♪"
	"Message displayed in mode-line by `echo-area-bell' function."
	:group 'user)
(defcustom echo-area-bell-delay 0.1
	"Number of seconds `echo-area-bell' displays its message."
	:group 'user)

;; internal variables
(defvar echo-area-bell-cached-string nil)
(defvar echo-area-bell-propertized-string nil)

(defun echo-area-bell ()
	"Briefly display a highlighted message in the echo-area.

    The string displayed is the value of `echo-area-bell-string',
    with a red background; the background highlighting extends to the
    right margin.  The string is displayed for `echo-area-bell-delay'
    seconds.

    This function is intended to be used as a value of `ring-bell-function'."

	(unless (equal echo-area-bell-string echo-area-bell-cached-string)
		(setq echo-area-bell-propertized-string
					(propertize
					 (concat
						(propertize
						 "x"
						 'display
						 `(space :align-to (- right ,(+ 2 (length echo-area-bell-string)))))
						echo-area-bell-string)
					 'face '(:background "black")))
		(setq echo-area-bell-cached-string echo-area-bell-string))
	(message echo-area-bell-propertized-string)
	(sit-for echo-area-bell-delay)
	(message ""))

(setq ring-bell-function 'echo-area-bell)
