;; start paren.el

(defun move-over-whitespace ()
  "moves point behind whitespace at point (or does nothing).
returns t if moved"
  (interactive)
  (if (and (save-excursion (re-search-forward search-whitespace-regexp nil t))
           (eq (match-beginning 0) (point)))
      (progn (goto-char (match-end 0))
             t)
    nil)
  )

(autoload 'jump-other-xml-tag "xmlxsl")

(defun goto-matching-paren ()
  "Go to the matching parenthesis."
  (interactive)
  (if (string= mode-name "HTML")
      (progn (jump-other-xml-tag)) ;; this defun is included in my XML-mode
      (cond ((eq (char-syntax (following-char)) ?\() (forward-list 1))
            ((eq (char-syntax (preceding-char)) ?\)) (backward-list 1))
            (t (message "No parenthesis matched.")))
    )
  )

(defun search-surrounding-sexp (pmin p pmax regopen regclose)
  "returns the boundaries of the innermost surrounding sexp between
pmin and pmax, containing p and matching the given regex's .
return nil if no such expression exists . "

  (if (or (< pmax pmin) (< p pmin) (< pmax p))
      (error "wrong parameters for search-surrounding-sexp")
    (let ((start nil)
          (end nil)
          (done nil)
          (found nil)
          result
          )
      (save-excursion
        (goto-char pmin)
        (while (not done)
          (setq result (condition-case err
                           (progn
                             (move-over-whitespace)
                             (forward-sexp)
                             (if (< p (point)) ;; we moved past p
                                 (let ((sub nil)
                                       )
                                   (setq done t)
                                   (setq end (point))
                                   (backward-sexp)
                                   (setq start (point))
;                                   (message "vor sub: (progn (goto-char %i) (set-mark-command nil) (goto-char %i))" start end)
                                   (setq sub (search-surrounding-sexp (+ start 1) p (- end 1) regopen regclose))
                                   (if sub
                                       (progn
;                                         (message "taking sub")
                                         (setq start (car sub)
                                               end (car (cdr sub))
                                               found t)
                                         (if (not (and (save-excursion (goto-char start) (looking-at regopen))
                                                       (save-excursion (goto-char (- end 1)) (looking-at regclose))))
                                             (progn ;;(message "sub returns illegal region")
                                                    (setq found nil))
                                           )
                                         )
;                                     (message "sub is nil")
                                     (setq found (and (save-excursion (goto-char start) (looking-at regopen))
                                                      (save-excursion (goto-char (- end 1)) (looking-at regclose))))
                                     )
;                                   (if found
;                                       (message "taking: (progn (goto-char %i) (set-mark-command nil) (goto-char %i))" start end)
;                                     (message "found is nil"))
                                   ))
                             nil)
                         (error (nth 1 err))
                         ))
          (if result (setq done t))))
      (if found (list start end) nil)
      )))

(defun exclude-bounds (bounds include)
  (if bounds
      (if include
          bounds
        (list (+ (car bounds) 1) (- (car (cdr bounds)) 1))
        )
    nil
  ))

;; string-boundaries:

(defun get-string-bounds (include)
  "returns the boundaries of the string point is in (or nil).
if include is t the surrounding quotation is included."
  (exclude-bounds (search-surrounding-sexp (point-min) (point) (point-max) "\"" "\"")
                  include))

(defun inside-string ()
  "returns t if point is inside string"
  (if (get-string-bounds t) t nil)
  )

(defun next-string ()
  (interactive)
  (let ((bounds (get-string-bounds t))
        done
        )
    (while (not done)
      (if bounds (goto-char (car (cdr bounds))))
      (setq done (or (not (re-search-forward "\""))
                     (inside-string)))
      (if (not done) (setq bounds (get-string-bounds t)))
      )
    )
  )

;; parenthesis-boundaries:

(defun get-parenthesis-bounds (include)
  "get region defined by surrounding parenthesis'
if 'include' is t the parenthesis' are included in the region . ([{ "
  (exclude-bounds (search-surrounding-sexp (point-min) (point) (point-max) "[[({<]" "[])}>]")
                  include))

(defun goto-closing-parenthesis ()
  "Move point to closing parenthesis of current level."
  (interactive)
  (let ((bounds (get-parenthesis-bounds nil)))
    (if bounds (goto-char (car (cdr bounds))) nil)))

(defun goto-opening-parenthesis ()
  "Move point to opening parenthesis of current level."
  (interactive)
  (let ((bounds (get-parenthesis-bounds nil)))
    (if bounds (goto-char (car bounds)) nil)))

(defun mark-bounds (bounds)
  "bounds is a list of 2 positions. the regions is marked"
  (goto-char (car bounds))
  (set-mark-command nil)
  (goto-char (car (cdr bounds)))
  )

(defun mark-parenthesis (include)
  "mark stuff in parenthesis
if INCLUDE is t the parenthesis' are marked too

Try using (mark-parenthesis t) multiple times.
Try using (mark-parenthesis nil) after (mark-parenthesis t)."
  (interactive)
  (let ((bounds (get-parenthesis-bounds include))
        )
    (if (not bounds)
        (message "Place cursor inside parenthesis.")
      (mark-bounds bounds)
      )))

(defun mark-parenthesis-included () (interactive) (mark-parenthesis t))
(defun mark-parenthesis-excluded () (interactive) (mark-parenthesis nil))

(defun surrounding-sexp-bounds (include)
  (exclude-bounds (search-surrounding-sexp (point-min) (point) (point-max)
                                                         "[[({<\"\']" "[])}>\"\']")
                                include)
  )

(defun mark-surrounding-sexp (include)
  "mark stuff in sexp ()
if INCLUDE is t the 'borders of sexp' are marked too

Try using (mark-surrounding-sexp t) multiple times                    .
Try using (mark-surrounding-sexp nil) after (mark-surrounding-sexp t) . "
  (interactive)
  (let ((bounds (surrounding-sexp-bounds include))
        )
    (if bounds
        ;;(save-excursion (message "Place cursor inside sexp."))
        (mark-bounds bounds)
      (backward-char 1)
      (mark-surrounding-sexp include)
      )))

(defun mark-surrounding-sexp-included () (interactive) (mark-surrounding-sexp t))
(defun mark-surrounding-sexp-excluded () (interactive) (mark-surrounding-sexp nil))

(defun recenter-best-sexp ()
  "recenters the biggest fitting sexp (or current line) in current window"
  (interactive)
  (let ((lines (window-height))
        (line1 (current-lineno))
        (line2 (current-lineno))
        (start-line (current-lineno))
        bounds
        (done nil)
        (last-size 0)
        )
    (save-excursion
      (while (not done)
        (setq bounds (surrounding-sexp-bounds t))
        (if (not bounds)
            (setq done t)
          (let (l1 l2 this-size)
            (goto-char (car bounds))
            (setq l1 (current-lineno))
            (goto-char (car (cdr bounds)))
            (setq l2 (current-lineno))

            (setq this-size (+ 1 (- l2 l1)));; size of current sexp
;            (message "l1=%s l2=%s" l1 l2)

            (if (> this-size lines);; does not fit on screen
                (let ((first-end (+ l1 (- lines 1))) ;; end of first screen of sexp
                      )
;                  (message "first-end=%s" first-end)
                  (if (and (<= last-size 1);; no sexp (only current-line)
                           (<= start-line first-end))
                      (setq line1 l1 line2 first-end done t);; start sexp on top of screen
                    (setq done t);; take last sexp
                    )
                  )
              (setq line1 l1 line2 l2 last-size this-size) ;; continue
              )
            )
          )
        )
      )

    (let* ((center (/ (+ line1 line2) 2))
           (center-pos (/ lines 2))
           (rpos (+ center-pos (- (current-lineno) center 1) 1))
           )

      (if (< rpos 0) (setq rpos 0))
;      (message "rpos=%s last-size=%s" rpos last-size)
      (recenter rpos)
      )
    )
  )

;; end paren.el

