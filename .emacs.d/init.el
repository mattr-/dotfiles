;; Increase the GC threshold so we go longer without garbage collection
(setq gc-cons-threshold 20000000)

;; Add our custom code directory to the load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-packages)
(require 'init-evil)
(require 'init-line-numbers)
(require 'init-org-mode)
(require 'init-magit)
(require 'init-emacs-defaults)


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

(use-package ir-black-theme :ensure t
)

(use-package markdown-mode :ensure t
)

(use-package rainbow-delimiters :ensure t
)

(use-package dash-at-point
  :ensure t
  :config
  (global-set-key "\C-cd" 'dash-at-point)
  )
;;}}}


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

;; (package-initialize)
