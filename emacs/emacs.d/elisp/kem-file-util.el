(defvar quickload-paths-alist '()
  "an alist of the format ((path . regexp)) that is used for quickloading -- regexp should be something like \".java$\"")
(defvar quickload-files nil
  "assoc list of files and their full paths -- generated using add-files-for-quickload")
(defun cons-name-and-path(filepath)
  (cons (afterlast filepath "/") filepath))

(defun add-files-for-quickload(path regexp)
  "called with a path and a regexp (usually something like \".java$\" ) -- called by add-files-for-quickload-from-alist"
  (setq quickload-files (append quickload-files
										  (mapcar 'cons-name-and-path (find-recursive-files path regexp)))))

(defun add-files-for-quickload-from-alist(alist)
  "called with an alist (likely quickload-paths-alist) to find files"
  (mapc (lambda (elem)
			 (add-files-for-quickload (car elem) (cdr elem))) alist))

(defun find-recursive-files (directory regexp)
       "List the files matching REGEXP in DIRECTORY and in its sub-directories."
       (interactive "DDirectory name: \nsRegexp: ")
       (let (file-list
             (current-directory-list
							(if (file-exists-p directory)
									(directory-files-and-attributes directory t)
								nil)))
         ;; while we are in the current directory
         (while current-directory-list
           (cond
            ;; check to see whether filename matches regexp
            ;; and if so, append its name to a list.
            ((and (string-match regexp (car (car current-directory-list)))
						(not (eq t (car (cdr (car current-directory-list))))))
					(setq file-list
							(cons (car (car current-directory-list)) file-list)))
            ;; check whether filename is that of a directory
            ((eq t (car (cdr (car current-directory-list))))
             ;; decide whether to skip or recurse
             (if
                 (equal "."
                        (substring (car (car current-directory-list)) -1))
                 ;; then do nothing since filename is that of
                 ;;   current directory or parent, "." or ".."
                 ()
               ;; else descend into the directory and repeat the process
               (setq file-list
                     (append
                      (find-recursive-files
                       (car (car current-directory-list))
							  regexp)
                      file-list)))))
           ;; move to the next filename in the list; this also
           ;; shortens the list so the while loop eventually comes to an end
           (setq current-directory-list (cdr current-directory-list)))
         ;; return the filenames
         file-list))


(defun quickload-filename-to-path(filename)
  (cdr (assoc filename quickload-files)))

(defun quickload-file()
  (interactive)
  (find-file
	(quickload-filename-to-path
	 (completing-read
	  "File to quickload: "
	  quickload-files
	  nil t ""))))


(defun reload-quickload-files ()
  (interactive)
  (setq quickload-files nil)
  (add-files-for-quickload-from-alist quickload-paths-alist)
  )
