(defvar autosave-position nil)
;;(setq autosave-position nil)
(defvar go-fullscreen 't)
(defun is-windowed-system()
  (interactive)
  window-system)

;; for some reason my setting doesn't match my getting, the y is always 3 less, so add it back in when saving the file
(defvar fudge-y-offset (if (>= emacs-major-version 24) 0 3))
(if (eq emacs-major-version 23)
		(setq fudge-y-offset 0))
(defvar kem-frame-position nil)
(defvar kem-frame-size-hash (make-hash-table :test 'equal))
(defvar kem-frame-local-filename "~/.emacs.local.frame")
(defvar kem-frame-saved-frame-size nil)

(defun get-kem-frame-size()
	(interactive)
	(gethash window-system-version kem-frame-size-hash))

(defun maximize-frame-startup(autosave-flag)
  (maximize-frame)
  (setq autosave-position autosave-flag)
  (setq kem-frame-saved-frame-size (get-kem-frame-size))
  )

(defun maximize-frame()
  (interactive)
  (if (or (and (mac-system-p) go-fullscreen (is-windowed-system)) (and kem-frame-position (get-kem-frame-size)))
		(let ((disp (getenv "DISPLAY"))
				(position)
				(size)
				(defaultpos '(1 . 1))
				(defaultsize '(234 . 66)))
		  (if disp
				(if (string-match ":0" disp)
					 (progn
						(setq defaultpos '(0 . 20))
						(setq defaultsize '(100 . 43))
						)
				  (if (string-match ":10" disp)
						(progn
						  (setq defaultpos '(100 . 90))
						  (setq defaultsize '(200 . 68))
						  ))))
		  (setq position (or kem-frame-position defaultpos))
		  (setq size (or (get-kem-frame-size) defaultsize))
		  (set-frame-position (selected-frame) (car position) (cdr position))
		  (set-frame-size (selected-frame) (car size) (cdr size))
		  )
	 )
  )

(add-hook 'window-size-change-functions 'kem-window-size-changed)

(defun save-frame-to-local (width height)
  (let* ((height (+ height fudge-y-offset))
			(new-frame-size (cons width height))
			(hash-entry-str ""))
	 (if (not (equal new-frame-size kem-frame-saved-frame-size))
		  (progn
				(message "new-frame-size %s" new-frame-size)
				(message "saved-frame-size %s" kem-frame-saved-frame-size)
				;; use (format "%s" window-system-version) so that nil gets converted to a string, so can be compared easily with the quoted version that is inserted into the file below
				(puthash (format "%s" window-system-version) (cons width height) kem-frame-size-hash)
				(maphash (lambda (key value)
									 (setq hash-entry-str (concat hash-entry-str
																								(format "(puthash \"%s\" '%s kem-frame-size-hash)\n" key value))))
								 kem-frame-size-hash)
				(message "writing hash: %s" kem-frame-size-hash)
			 (with-temp-buffer
				(insert
				 (format ";; don't modify this file manually, it is generated by kem-frame-util.el, used for storing default location of the emacs window\n")
				 hash-entry-str)
				(when (file-writable-p kem-frame-local-filename)
					(write-region (point-min) (point-max) kem-frame-local-filename)))
			 (setq kem-frame-saved-frame-size new-frame-size)))))

(defun kem-window-size-changed (frame)
  (let ((width (frame-width))
		  (height (frame-height)))
	 (if autosave-position
		  (save-frame-to-local width height))))
