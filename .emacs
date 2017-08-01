;; Increase the GC threshold so we go longer without garbage collection

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq gc-cons-threshold 20000000)

;; Add our custom code directory to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-evil)
(require 'init-line-numbers)

(use-package org :ensure t)
;; {{{ org-mode settings
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files '("~/Dropbox/org"))
;; }}}

(use-package helm
  :ensure t
  :config
  (helm-autoresize-mode 1)
  (setq helm-autoresize-min-height 10)
  (setq helm-autoresize-max-height 20)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
)

(use-package lua-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
)

(use-package ir-black-theme
  :ensure t
)

(use-package markdown-mode
  :ensure t
)

(use-package rainbow-delimiters
  :ensure t
)

(use-package dash-at-point
  :ensure t
  :config
  (global-set-key "\C-cd" 'dash-at-point)
  )
;;}}}


;; Turn off the menu bar
(menu-bar-mode -1)

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

;; {{{ Custom color theme settings
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; }}}

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-delimiters markdown-mode evil arjen-grey-theme lua-mode helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
