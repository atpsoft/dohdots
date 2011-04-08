(require 'easymenu)
(require 'cl)

(defconst bookmark-map (make-sparse-keymap))
(defconst bookmark-menu nil)
(defconst bookmark-mode nil)
(defvar   bookmark-flag nil)

;; (define-key bookmark-map "" 'bookmark-1)
;; (define-key bookmark-map "" 'bookmark-2)
;; (define-key bookmark-map "" 'bookmark-3)

(easy-menu-define
  bookmark-menu
  (if (boundp 'xemacs-logo) nil (list bookmark-map))
  "bookmarks"
  (list
	"Bookmark"

	["toggle-bookmark-current-line" toggle-bookmark-current-line t]
	["bookmark-regexp" bookmark-regexp t]
	["toggle-bookmarked-and-unbookmarked-lines" toggle-bookmarked-and-unbookmarked-lines t]
	"---"
	["cut-bookmarked-lines" cut-bookmarked-lines t]
	["copy-bookmarked-lines" copy-bookmarked-lines t]
	["delete-bookmarked-lines" delete-bookmarked-lines t]
	"---"
	["toggle-bookmark-display" toggle-bookmark-display t]
	"---"
	["bookmark-query-replace" bookmark-query-replace t]
	["bookmark-replace-string" bookmark-replace-string t]
	"---"
	["delete-all-bookmarks" delete-all-bookmarks t]
))


;;  Add mode to minor mode list
;;
(unless bookmark-flag                         ;Add only once
  (setq bookmark-flag t)
  (push (cons 'bookmark-mode bookmark-map)  minor-mode-map-alist))

;;   Toggle mode and add the menu, not the menu is available
;;
(setq bookmark-mode nil)
(setq bookmark-mode t)
(easy-menu-add bookmark-menu)

;; X window note:
;; Trying this does not enable choice "3" in XEmacs 19.14
;; In Emacs 19.30 it works ok.
;;
;; (progn (put 'bookmark-menu 'menu-flag t) (force-mode-line-update))
;; (progn (put 'bookmark-menu 'menu-flag nil) (force-mode-line-update))


;; Another Test, you need this in XEmacs, but not actually
;; in Emacs. --> Use it for portability.
;;
;; (easy-menu-remove bookmark-menu)

;; end of code
