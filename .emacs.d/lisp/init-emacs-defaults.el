;;; Various emacs defaults that I don't want scattered throughout my .emacs file

(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
  (menu-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))
;; tooltips in echo-aera
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))


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

;; Only use spaces
(setq-default indent-tabs-mode nil)

(provide 'init-emacs-defaults)
