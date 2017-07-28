;; Increase the GC threshold so we go longer without garbage collection
(setq gc-cons-threshold 20000000)

;; {{{ Package Setup
;; Enable the package repositories
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("marmalade" .  "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" .  "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up use-package
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

(use-package org
  :ensure t)

(use-package helm
  :ensure t
  :config
  (helm-autoresize-mode 1)
  (setq helm-autoresize-min-height 10)
  (setq helm-autoresize-max-height 20))

(use-package lua-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
)

(use-package ir-black-theme
  :ensure t
)

(use-package evil
  :ensure t
  :config
  (evil-mode 1))

(use-package markdown-mode
  :ensure t
)

;; {{{ org-mode settings
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files '("~/Dropbox/org"))
;; }}}

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
 '(custom-safe-themes
   (quote
    ("7ceb8967b229c1ba102378d3e2c5fef20ec96a41f615b454e0dc0bfa1d326ea6" default)))
 '(org-agenda-files (quote ("~/Dropbox/org/minecraft.org")))
 '(package-selected-packages
   (quote
    (markdown-mode evil arjen-grey-theme lua-mode helm use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
