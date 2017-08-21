;;; Various emacs defaults that I don't want scattered throughout my .emacs file

(menu-bar-mode -1) ;; Turn off the menu bar

;; Use 'y' or 'n' instead of 'yes' or 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; Show line numbers in the status bar
(setq line-number-mode t)

;; Show column numbers in the status bar
(setq column-number-mode t)

;; Set the default line wrap to 78
(set-default 'fill-column 78)

;; Highlight parenthesis after you type/navigate over them
(show-paren-mode t)

;; Don't wrap long lines
(setq-default truncate-lines nil)

;; End sentences with a single space
(setq sentence-end-double-space nil)

(provide 'init-emacs-defaults)
