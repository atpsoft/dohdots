; tired of accidentally starting a mail message
(global-unset-key "\C-x\m")

; replace ctrl-v with ctrl-dot for scroll-up and meta-v with ctrl-comma for scroll-down
(global-unset-key "\C-v")
(global-set-key (quote [67108910]) (quote scroll-up))
(global-unset-key "\M-v")
(global-set-key (quote [67108908]) (quote scroll-down))

; I like to have a single sequence binding for undo
(global-set-key "\C-z" 'undo)

(global-set-key "\M-w" 'kill-ring-save)

; I kept accidentally hit this bind somehow when I was going fast.  I very rarely need to do this, so I decided a 3 sequence binding was a good solution
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-y\C-q" 'save-buffers-kill-emacs-dont-ask-about-processes)

; not sure exactly what this does
(setq x-select-enable-clipboard t)

; the backup files drive me crazy
(custom-set-variables
 '(make-backup-files nil)
)

; these next few things related to windows and ctrl-m still aren't quite what I want, but getting closer
;(defun fix-windows-file()
;  (interactive)
;  (beginning-of-buffer)
;  (replace-ctrl-m)
;  (delete-trailing-save-buffer)
;)

(fset 'fix-windows-file
   [?\M-x ?b ?e ?g ?i ?n ?n ?i ?n ?g ?- ?o ?f ?- ?b ?u ?f ?f ?e ?r return ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?c ?t ?r ?l ?- ?m return ?\M-x ?d ?e ?l ?e ?t ?e ?- ?t ?r ?a ?i ?l ?i ?n ?g ?- ?s ?a ?v ?e ?- ?b ?u ?f ?f ?e ?r return])

(global-unset-key "\C-x\C-m")
;(global-set-key "\C-j\C-m" 'replace-ctrl-m)
(global-set-key "\C-j\C-m" 'fix-windows-file)

; old - doesn't appear to be needed or used anymore
; (setq my-frame-position '(0 . 0))
; (setq my-frame-size '(176 . 64))

; tinkering with colors
(custom-set-faces
'(font-lock-string-face ((((class color) (min-colors 88) (background light)) (:foreground "orange4"))))
'(font-lock-type-face ((((class color) (min-colors 88) (background light)) (:foreground "ForestGreen"))))
'(font-lock-variable-name-face ((((class color) (min-colors 88) (background light)) (:foreground "SeaGreen"))))
)


; scratch buffer -- nuke initial message
(setq initial-scratch-message nil)

; delete text instead of killing *scratch* buffer
(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)
