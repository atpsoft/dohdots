;--------------------
; Packages
;--------------------
(require 'package)

(setq package-archives '(
			("ELPA" . "http://tromey.com/elpa/")
      ("gnu" . "http://elpa.gnu.org/packages/")
      ("marmalade" . "http://marmalade-repo.org/packages/")
      ("MELPA" . "http://melpa.milkbox.net/packages/")))

(package-initialize)



;--------------------
; Keybindings & misc
;--------------------

; tired of accidentally starting a mail message
(global-unset-key "\C-x\m")

; replace ctrl-v with ctrl-dot for scroll-up and meta-v with ctrl-comma for scroll-down
;(global-unset-key "\C-v")
;(global-set-key (quote [67108910]) (quote scroll-up))
;(global-unset-key "\M-v")
;(global-set-key (quote [67108908]) (quote scroll-down))

; Mak: I like to have a single sequence binding for undo
(global-set-key "\C-z" 'undo)

(global-set-key "\M-w" 'kill-ring-save)

; Exit Emacs with Mak's three keystroke sequence.
(global-unset-key "\C-x\C-c")
(global-set-key "\C-x\C-y\C-q" 'save-buffers-kill-emacs-dont-ask-about-processes)

; Mak: not sure exactly what this does
(setq x-select-enable-clipboard t)

; Kill the annoying backup files.
(custom-set-variables
 '(make-backup-files nil)
)

(global-unset-key "\C-x\C-m")
(global-set-key "\C-j\C-m" 'fix-windows-file)


;; Use "y or n" for answers instead of complete words "yes or no"
(fset 'yes-or-no-p 'y-or-n-p)

;; If not on AC power line, then display battery status on the mode line
(and (require 'battery nil t)
     (functionp battery-status-function)
     (or (equal (cdr (assoc ?L (funcall battery-status-function))) "on-line")
         (display-battery-mode)))


; Blinking cursors are distracting - turn blink OFF
(and (fboundp 'blink-cursor-mode) (blink-cursor-mode (- (*) (*) (*))))


; need my theme in the erb files
(setq auto-mode-alist (cons '("\\.erb$" . sgml-mode) auto-mode-alist))


;--------------------
; Themes
;--------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")

; I need to adjust the color on selection highlighting
(load-theme 'twilight t)

;(load-theme-buffer-local 'wheatgrass (current-buffer))
;(add-hook 'java-mode (lambda nil (color-theme-buffer-local 'color-theme-robin-hood (current-buffer))))

;--------------------
; Improved buffer switching using built-in Iswitch Buffers
;--------------------
(iswitchb-mode 1)

; allow left and right arrow keys to function
(defun iswitchb-local-keys ()
      (mapc (lambda (K)
	      (let* ((key (car K)) (fun (cdr K)))
    	        (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	    '(("<right>" . iswitchb-next-match)
	      ("<left>"  . iswitchb-prev-match)
	      ("<up>"    . ignore             )
	      ("<down>"  . ignore             ))))
    (add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

; buffers to ignore with switching
;(setq iswitchb-buffer-ignore '("^ " "*Buffer")) ;hide all *...* buffers
(add-to-list 'iswitchb-buffer-ignore "^ ")
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "*Disabled")


;--------------------
; *scratch* buffer
;--------------------

; Nukecu initial message
(setq initial-scratch-message nil)

; delete text instead of killing *scratch* buffer
(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)


;--------------------
; open groups of files
;--------------------

; emacs files
(defun jakemacs-files()
  (interactive)

  (find-file "~/src/dohdots/emacs/emacs.jake")
  (find-file "~/.emacs.local")
)



;--------------------------------------------------
; Archive - old but might want to reference
;--------------------------------------------------

; these next few things related to windows and ctrl-m still aren't quite what I want, but getting closer
;(defun fix-windows-file()
;  (interactive)
;  (beginning-of-buffer)
;  (replace-ctrl-m)
;  (delete-trailing-save-buffer)
;)

(fset 'fix-windows-file
   [?\M-x ?b ?e ?g ?i ?n ?n ?i ?n ?g ?- ?o ?f ?- ?b ?u ?f ?f ?e ?r return ?\M-x ?r ?e ?p ?l ?a ?c ?e ?- ?c ?t ?r ?l ?- ?m return ?\M-x ?d ?e ?l ?e ?t ?e ?- ?t ?r ?a ?i ?l ?i ?n ?g ?- ?s ?a ?v ?e ?- ?b ?u ?f ?f ?e ?r return])

; old - doesn't appear to be needed or used anymore
; (setq my-frame-position '(0 . 0))
; (setq my-frame-size '(176 . 64))
