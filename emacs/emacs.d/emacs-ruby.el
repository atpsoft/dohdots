;;(add-to-list 'load-path "~/.emacs.d/emacs-rails/")
;;(require 'rails)
(add-to-list 'load-path "~/.emacs.d/elisp-ruby/rinari/")
(setq enable_ruby_pabbrev 1)
(setq enable_ruby_abbrev 1)

(defun kems-ruby-mode-hook()
  (my-mode-key-bindings)
  (pabbrev-mode enable_ruby_pabbrev)
  (abbrev-mode enable_ruby_abbrev)
  )
(add-hook 'ruby-mode-hook 'kems-ruby-mode-hook)

;; Simple Lisp Files
(add-to-list 'load-path "~/.emacs.d/elisp-ruby/el")
(require 'pabbrev)

;; Ruby Mode
(add-to-list 'load-path "~/.emacs.d/elisp-ruby/ruby-mode")

(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(require 'ruby-electric)

(defun ruby-eval-buffer () (interactive)
  "Evaluate the buffer with ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

(defun my-ruby-mode-hook ()
;;  (font-lock-mode t)
  (setq standard-indent 2)
  (ruby-electric-mode t)
  (define-key ruby-mode-map "\C-c\C-a" 'ruby-eval-buffer))
(add-hook 'ruby-mode-hook 'my-ruby-mode-hook)

(setq auto-mode-alist (cons '("\\.rb\\'" . ruby-mode) auto-mode-alist))

;; Rinari Mode (Rails)
(add-to-list 'load-path "~/.emacs.d/elisp-ruby/rinari")
(add-to-list 'load-path "~/.emacs.d/elisp-ruby/rinari/rhtml")
(require 'rinari)
(setq auto-mode-alist (cons '("\\.rhtml\\'" . rhtml-mode) auto-mode-alist))

;;(add-hook 'ruby-mode-hook 'kems-ruby-mode-hook)
