(defun edit-migrations ()
  (interactive)
  (if (eq default-dohapp-home nil)
		(message "you need to set default-dohapp-home to your default DohApp::home directory")
	 (progn
		(let ((result) (upfile) (downfile))
		  (setq result (shell-command-to-string (concat "cd " default-dohapp-home "; export RUBYLIB=" default-rubylib "; migrate.rb -m")))
		  (string-match "/[^ ]+up\.sql" result)
		  (setq upfile (match-string 0 result))
		  (string-match "/[^ ]+down\.sql" result)
		  (setq downfile (match-string 0 result))
		  (find-file downfile)
		  (find-file upfile)
		  (message (concat "Opened: " upfile " and " downfile))
		  )
		)))


(defvar default-dohapp-home nil)
(defvar default-rubylib "~/dohruby/lib")
(defvar default-rubylib "/Users/kem/personal/rubylib:/Users/kem/dohruby/lib")

