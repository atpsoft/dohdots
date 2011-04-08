
(defun after(string regexp)
  (if (string-match regexp string 0)
		(substring string (match-end 0))
	 ""))

(defun afterlast(string regexp)
  (let ((last 0))
	 (while (string-match regexp string last)
		(setq last (match-end 0)))
	 (if (> last 0)
		  (substring string last)
		"")))


;;(minibuffer-message (after "*file:/file:/smoe" "file:")) -- returns /smoe
;;(minibuffer-message (afterlast "*file:/file:/smoe" "file:")) -- returns /smoe
;;(minibuffer-message (afterlast "file:/smoe" "file:")) -- returns /smoe
		
