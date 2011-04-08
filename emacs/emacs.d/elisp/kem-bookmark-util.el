(defface highlight-bookmarks-face
  '((t (:background "wheat")))
    "Face used to highlight current line."
	 :group 'highlight-bookmarks)

(defcustom highlight-bookmarks-high-faces '()
  "*Lines containing one of this faces are not highlighted."
  :type  'list
  :group 'highlight-bookmarks)


(defvar all-bookmarks '()
  "All current bookmarks -- stored as overlays because I'm not sure how else to make them move as the code before them changes.")

(defvar highlight-overlays '()
  "Overlays for highlighting.")

(defvar bookmark-regexp-history nil
  "History of regexps used for interactive fontification.")
(make-variable-buffer-local 'bookmark-regexp-history)
(put 'bookmark-regexp-history 'permanent-local t)

(defun get-highlight-overlay (beg end &optional buffer)
    ;; Dummy initialization
  (let ((overlay (make-overlay beg end buffer)))
	 (overlay-put overlay 'face 'highlight-bookmarks-face)
	 (push overlay highlight-overlays)
	 overlay))

(defun create-all-highlight-overlays ()
  (save-excursion
	 (mapcar* 'highlight-line-with-point (mapcar 'overlay-start all-bookmarks) (mapcar 'overlay-buffer all-bookmarks))
  ))

(defun delete-all-bookmarks ()
  (interactive)
  (mapcar 'delete-overlay  highlight-overlays)
  (mapcar 'delete-overlay all-bookmarks)
  (setq highlight-overlays nil)
  (setq all-bookmarks nil)
  )

(defvar kem-showing-bookmarks t)

;; Enable/Disable display of bookmarks
(defun kem-show-bookmarks (&optional on-off)
  "Switch display of bookmarks ON-OFF"
  (interactive)
  (setq kem-showing-bookmarks on-off)
  (cond
   (on-off
	 (message "displaying bookmarks")
	 (create-all-highlight-overlays))
   (t
	 (message "hiding bookmarks")
    (mapcar 'delete-overlay highlight-overlays))))

(defun toggle-bookmark-display ()
  (interactive)
  (save-excursion
  (kem-show-bookmarks (not kem-showing-bookmarks))))

(defun highlight-line-with-point (current-point &optional buffer)
  (save-excursion
	 (if buffer (switch-to-buffer buffer))
	 (goto-char current-point)
	 (let ((beg (progn (beginning-of-line) (point)))
			 (end (progn (forward-line 1)
							 (point))))
		(get-highlight-overlay beg end buffer)
		)))

(defun bookmark-point (point-to-bookmark)
  (save-excursion
	 (goto-char point-to-bookmark)
	 (let ((beg (progn (beginning-of-line) (point))))
		(push (make-overlay beg beg) all-bookmarks)
		(if kem-showing-bookmarks
			 (highlight-line-with-point beg)))))

(defun point-is-bookmarked (pt)
  (let ((bol (bol-point pt))
		  (result))
	 (mapcar (lambda (bookmark)
				  (if (eq (overlay-start bookmark) bol)
						(setq result (overlay-start bookmark))))
				  all-bookmarks)
	 result))

(defun bol-point (pt)
  (let ((current-point (point))
		  (beginning-of-line-point))
	 (goto-char pt)
	 (beginning-of-line)
	 (setq beginning-of-line-point (point))
	 (goto-char current-point)
	 beginning-of-line-point))

(defun delete-bookmark-at-point (pt)
  (let ((overlays all-bookmarks)
		  (bol (bol-point pt)))
	 (while overlays
		(let ((overlay (car overlays)))
		  (if (eq (overlay-start overlay) bol)
				(progn
				  (delete-overlay overlay)
				  (setq all-bookmarks (remove overlay all-bookmarks))
				  (setq overlays nil))))
		(setq overlays (cdr overlays)))))

(defun delete-bookmark (pt)
  (delete-bookmark-at-point pt)
  (let ((overlays (overlays-at pt))
		  (pt (bol-point pt)))
	 (message "in delete-bookmark, overlays = %S, pt = %S" overlays pt)
	 (mapcar (lambda (overlay)
				  (if (member overlay highlight-overlays)
						(progn
						  (setq highlight-overlays (remove overlay highlight-overlays))
						  (delete-overlay overlay))))
				overlays)))

(defun bookmark-current-line ()
  (interactive)
  (bookmark-point (point))
  )

(defun toggle-bookmark-current-line ()
  (interactive)
  (if (point-is-bookmarked (point))
		(delete-bookmark (point))
	 (bookmark-current-line)))

(defun bookmark-regexp (regexp)
  "Bookmark all lines containing a match of REGEXP.

Interactively, prompt for REGEXP.  Buffer-local history
list maintained for regexps.
\\<minibuffer-local-map>Use \\[next-history-element] and \\[previous-history-element] to retrieve next or previous history item.
\(See info node `Minibuffer History'.)"
  (interactive
   (list
	 (read-from-minibuffer "Regexp to bookmark line: "
								  (cons (or (car bookmark-regexp-history) "") 1 )
                           nil nil 'bookmark-regexp-history)))
;;  (kem-show-bookmarks t)
  (bookmark-set-pattern
	;; The \\(?:...\\) grouping construct ensures that a leading ^, +, * or ?
   ;; or a trailing $ in REGEXP will be interpreted correctly.
   (concat "^.*\\(?:" regexp "\\).*$")))

(defun bookmark-set-pattern (regexp)
  "Bookmark lines matching REGEXP."
  (save-excursion
	 (end-of-buffer)
	 (let ((search-end (point)))
		(beginning-of-buffer)
		(while (re-search-forward regexp search-end t)
		  (bookmark-point (match-beginning 0))
		  (goto-char (match-end 0))))))

(defun toggle-bookmarked-and-unbookmarked-lines ()
  (interactive)
  (save-excursion
	 (let ((old-bookmark-points (mapcar 'overlay-start all-bookmarks))
			 (end-point (progn (end-of-buffer) (point))))
		(delete-all-bookmarks)
		(beginning-of-buffer)
		(while (not (equal (point) end-point))
		  (if (not (member (point) old-bookmark-points))
				(bookmark-current-line))
		  (forward-line 1)))))

(defun bookmarks-by-point-order ()
  (sort (copy-sequence all-bookmarks) (lambda (bookmark1 bookmark2) (< (overlay-start bookmark1) (overlay-start bookmark2)))))

(defun iterate-bookmarked-lines (beg-end-function)
  (iterate-bookmarked-lines-directionally t beg-end-function))

(defun iterate-bookmarked-lines-reverse (beg-end-function)
  (iterate-bookmarked-lines-directionally nil beg-end-function))

(defun iterate-bookmarked-lines-directionally (forwardp beg-end-function)
  (save-excursion
	 (let ((beg)
			 (bookmarks (if forwardp (bookmarks-by-point-order)
							  (reverse (bookmarks-by-point-order)))))
		(mapcar (lambda (bookmark)
					 (goto-char (overlay-start bookmark))
					 (setq beg (point))
					 (forward-line 1)
					 (funcall beg-end-function beg (point)))
				  bookmarks))))

(defun copy-bookmarked-lines ()
  (interactive)
  (let ((yanked))
	 (iterate-bookmarked-lines-reverse
	  (lambda (beg end)
		 (push (buffer-substring beg end) yanked)))
	 (kill-new "")
	 (mapcar (lambda (yankstr) (kill-append yankstr nil)) yanked)))

(defun delete-bookmarked-lines ()
  (interactive)
  (iterate-bookmarked-lines-reverse
	(lambda (beg end)
	  (delete-region beg end)))
  (delete-all-bookmarks))

(defun cut-bookmarked-lines ()
  (interactive)
  (copy-bookmarked-lines)
  (delete-bookmarked-lines))

(defun bookmark-query-replace ()
  (interactive)
  (let ((repl_strs (query-replace-read-args "Query replace in bookmarks" t)))
	 (iterate-bookmarked-lines
	  (lambda (beg end)
		 (query-replace (car repl_strs) (cadr repl_strs) nil beg end)))))

(defun bookmark-replace-string ()
  (interactive)
  (let ((repl_strs (query-replace-read-args "Replace in bookmarks" t)))
	 (iterate-bookmarked-lines
	  (lambda (beg end)
		 (setq result (replace-string (car repl_strs) (cadr repl_strs) nil beg end))
		 ))))

(defun bookmark-regexp (regexp)
  "Bookmark all lines containing a match of REGEXP.

Interactively, prompt for REGEXP.  Buffer-local history
list maintained for regexps.
\\<minibuffer-local-map>Use \\[next-history-element] and \\[previous-history-element] to retrieve next or previous history item.
\(See info node `Minibuffer History'.)"
  (interactive
   (list
	 (read-from-minibuffer "Regexp to bookmark line: "
								  (cons (or (car bookmark-regexp-history) "") 1 )
                           nil nil 'bookmark-regexp-history)))
;;  (kem-show-bookmarks t)
  (bookmark-set-pattern
	;; The \\(?:...\\) grouping construct ensures that a leading ^, +, * or ?
   ;; or a trailing $ in REGEXP will be interpreted correctly.
   (concat "^.*\\(?:" regexp "\\).*$")))

(defun bookmark-set-pattern (regexp)
  "Bookmark lines matching REGEXP."
  (save-excursion
	 (end-of-buffer)
	 (let ((search-end (point)))
		(beginning-of-buffer)
		(while (re-search-forward regexp search-end t)
		  (bookmark-point (match-beginning 0))
		  (goto-char (match-end 0))))))

(defun toggle-bookmarked-and-unbookmarked-lines ()
  (interactive)
  (save-excursion
	 (let ((old-bookmarks all-bookmarks)
			 (end-point (progn (end-of-buffer) (point))))
		(delete-all-bookmarks)
		(beginning-of-buffer)
		(while (not (equal (point) end-point))
		  (if (not (member (point) old-bookmarks))
				(bookmark-current-line))
		  (forward-line 1)))))

