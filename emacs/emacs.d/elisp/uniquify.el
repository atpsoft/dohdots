  (defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((lines) (end (copy-marker end)))
        (goto-char start)
        (while (and (< (point) (marker-position end))
                    (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (if (member line lines)
                (delete-region (point) (progn (forward-line 1) (point)))
              (push line lines)
              (forward-line 1)))))))

  (defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))
