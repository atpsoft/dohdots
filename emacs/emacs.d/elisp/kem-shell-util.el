(defun create-new-shell(shell-name)
  (let ((tmp-name (generate-new-buffer-name "shelltmp")))
	 (shell)
	 (rename-buffer tmp-name)
	 (shell)
	 (rename-buffer shell-name)
	 (switch-to-buffer tmp-name)
	 (rename-buffer "*shell*")))

(defvar default-shell-name nil)
(defvar open-shell-list nil)

(defun check-open-shell-list()
  (let (new-list)
	 (dolist (buffer open-shell-list new-list)
		(if (get-buffer-process buffer)
			 (setq new-list (cons buffer new-list))
		  ))
	 (setq open-shell-list new-list)
	 ))


(defun goto-shell(&optional shell-name)
  (interactive)
  (setq default-shell-name (if (or (equal (buffer-name) default-shell-name)
											  (not (get-buffer-process default-shell-name)))
										 "*shell*" default-shell-name))
  (check-open-shell-list)
  (let* (
			(shell-name (or shell-name
								 (let* ((prompt (concat "shell to goto" (if default-shell-name (concat " (default: " default-shell-name ")") "") ": "))
											 (read-value (completing-read prompt open-shell-list))
											 (read-value (if (equal read-value "") nil read-value)))
									read-value)
								 default-shell-name))
			(previous-buffer-name (buffer-name))
			)
	 (setq default-shell-name previous-buffer-name)
	 (if (not (get-buffer shell-name))
		  (create-new-shell shell-name))
	 (setq open-shell-list (append open-shell-list (list shell-name)))
	 (switch-to-buffer shell-name)
	 ))

;; make it so carriage return doesn't wipe out the text when displaying
;; e.g.  use irb and execute
;; puts "joe\r\n"
;; with this variable set to nil, you won't see the joe
(setq comint-inhibit-carriage-motion nil)

(defun shell-filter-ctrl-m (string)
  "Remove `^M' characters from comint output.

This function can be put on `comint-output-filter-functions'.
The argument STRING is ignored."
  (let ((pmark (process-mark (get-buffer-process (current-buffer)))))
    (save-excursion
      (goto-char (or (and (markerp comint-last-output-start)
			  (marker-position comint-last-output-start))
		     (point-min)))
      (while (re-search-forward "[\C-m]" pmark t)
        (replace-match "")))))

(add-hook 'comint-output-filter-functions 'shell-filter-ctrl-m)

(defun get-filename-and-lineno()
	(interactive)
	(save-excursion
		(end-of-line)
		(let ((eolpoint (point)))
			(beginning-of-line)
      (get-filename-and-lineno-from-string (buffer-substring-no-properties (point) eolpoint)))))

(defun get-filename-and-lineno-from-string(line)
  (interactive)
  (let* ((firstcolon (string-match ":" line))
         (filename (if firstcolon
                       (substring line 0 firstcolon)
                     line))
         (secondcolon (if firstcolon (string-match ":" line (+ firstcolon 1))
                        nil))
         (lineno (if firstcolon (substring line (+ firstcolon 1) secondcolon)
                 nil))
         )
    (cons filename (if lineno (string-to-number lineno)))))

  

(defvar grepped-file-keep-current-window 't)
(defun open-grepped-file-on-line()
  (interactive)
  (let* ((filename-lineno (get-filename-and-lineno))
				 (curfile (car filename-lineno))
				 (curlinenum (cdr filename-lineno))
				 (curdir default-directory))
    (message "opening grepped file default directory: %s" curdir)
		(other-window 1)
		(find-file (if (string-match "^\/" curfile) curfile (concat curdir curfile)))
		(if curlinenum
				(goto-line curlinenum)
			)
		(if grepped-file-keep-current-window
				(other-window 1)
			)
		)
	)


