(setq gc-cons-threshold 128000000)
(add-hook 'after-init-hook #'(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold 800000)))

;; All the package set up goes here
(require 'package)

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up use-package
(eval-when-compile (require 'use-package))
(require 'bind-key)

(setq use-package-verbose t)
(setq use-package-always-ensure t)
(setq load-prefer-newer t)

(use-package auto-compile
  :defer t
  :config (auto-compile-on-load-mode))

(setq user-full-name  "Matt Rogers"
      user-mail-address "codemattr@gmail.com")

(when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
  (tool-bar-mode -1))
(when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
  (menu-bar-mode -1))
(when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
  (scroll-bar-mode -1))
;; tooltips in echo-aera
(when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
  (tooltip-mode -1))

(setq inhibit-startup-screen t
      initial-scratch-message ";; ready\n\n")

(fset 'yes-or-no-p 'y-or-n-p)

(setq line-number-mode t)
(setq column-number-mode t)

(setq sentence-end-double-space nil)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(use-package bind-map)

(bind-map my-normal-base-leader-map
  :keys ("M-m")
  :evil-keys (",")
  :evil-states (normal))

(setq whitespace-style '(face empty trailing tab-mark))

(add-hook 'prog-mode-hook 'whitespace-mode)

(bind-map-set-keys my-normal-base-leader-map
  "sw" 'whitespace-cleanup)

(use-package editorconfig
  :defer t
  :init
  (progn
    (with-eval-after-load 'editorconfig
      (diminish 'editorconfig-mode)))
  :config
  (progn
    (editorconfig-mode 1)))

(use-package evil
  :demand t
  :init
  (setq evil-want-C-u-scroll t
	evil-want-visual-char-semi-exclusive t
	evil-want-Y-yank-to-eol t
	evil-magic t
	evil-echo-state t
	evil-indent-convert-tabs t
	evil-ex-search-vim-style-regexp t
	evil-ex-substitute-global t
	evil-ex-visual-char-range t  ; column range for ex commands
	evil-insert-skip-empty-lines t
	evil-mode-line-format 'nil
	;; more vim-like behavior
	evil-symbol-word-search t
	;; don't activate mark on shift-click
	shift-select-mode nil)
  :config
  ;; Move to new split -- setting `evil-split-window-below' &
  ;; `evil-vsplit-window-right' to non-nil mimics this, but that doesn't update
  ;; window history. That means when you delete a new split, Emacs leaves you on
  ;; the 2nd to last window on the history stack, which is jarring.
  ;; Borrowed from doom-emacs
  (defun +evil*window-follow (&rest _)  (evil-window-down 1))
  (defun +evil*window-vfollow (&rest _) (evil-window-right 1))
  (advice-add #'evil-window-split  :after #'+evil*window-follow)
  (advice-add #'evil-window-vsplit :after #'+evil*window-vfollow))


(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package evil-numbers)
(use-package evil-tabs)
(evil-mode 1)

(use-package popwin
  :config
  (popwin-mode 1)
  (bind-map-set-keys my-normal-base-leader-map
      "wpm" 'popwin:messages
      "wpp" 'popwin:close-popup-window))

(use-package magit
  :defer t
  :config
  (with-eval-after-load 'magit
    (require 'evil-magit))
  (bind-map-set-keys my-normal-base-leader-map
    "gs" 'magit-status))
(use-package evil-magit :defer t)

(use-package doom-themes
  :init
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
  :config
    (load-theme 'doom-one t)
    (doom-themes-visual-bell-config) ; flash mode line on error
    (doom-themes-org-config) ; improve org-mode native fontification
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
