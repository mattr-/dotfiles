(setq gc-cons-threshold 128000000)
(add-hook 'after-init-hook #'(lambda ()
			       ;; restore after startup
			       (setq gc-cons-threshold 800000)))

;; All the package set up goes here
(require 'package)

(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Set up use-package
(eval-when-compile (require 'use-package))
(require 'diminish)
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

(when (display-graphic-p)
  (set-face-attribute 'default nil :font "Fira Code Retina 12")
  (set-frame-font "Fira Code Retina 12" nil t))

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

(use-package linum-relative
  :commands (linum-relative-toggle linum-relative-on))
(require 'linum)

(defcustom linum-disabled-modes-list
  '(eshell-mode
    wl-summary-mode
    compilation-mode
    org-mode
    dired-mode
    doc-view-mode
    image-mode)
  "* List of modes disabled when global linum mode is no"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled:"
  :group 'linum
  )

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp)
	      (member major-mode linum-disabled-modes-list)
	      (string-match "*" (buffer-name))
	      (> (buffer-size) 3000000)) ;; disable for buffers bigger than 3MB
    (linum-mode 1)))


(unless window-system
  (add-hook 'linum-before-numbering-hook
  (lambda ()
    (setq-local linum-format-fmt
		(let ((w (length (number-to-string
				  (count-lines (point-min) (point-max))))))
		  (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))

(global-linum-mode 1)

(bind-map-set-keys my-normal-base-leader-map
  "nr" 'linum-relative-toggle)

(setq whitespace-style '(face empty trailing tab-mark))

(add-hook 'prog-mode-hook 'whitespace-mode)

(eval-after-load "diminish"
'(progn
   (eval-after-load "whitespace"
     '(diminish 'global-whitespace-mode))
   (eval-after-load "whitespace"
     '(diminish 'whitespace-mode))))

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

(use-package flycheck
  :defer t
  :init
    (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
    (diminish 'flycheck-mode " ⓢ"))

(defvar mattr-default-company-backends
  '((company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends.
This variable is used to configure mode-specific company backends.
Backends in this list will always be active in these modes, as well as any
backends added by individual language support setups.")

(defmacro mattr|defvar-company-backends (mode)
  "Define a MODE specific company backend variable with default backends.
The variable name format is company-backends-MODE."
  `(defvar ,(intern (format "company-backends-%S" mode))
     ',mattr-default-company-backends
     ,(format "Company backend list for %S" mode)))

(defmacro mattr|add-company-hook (mode)
  "Enable company for the given MODE.
MODE must match the symbol passed in `mattr|defvar-company-backends'.
The initialization function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
	(func (intern (format "mattr//init-company-%S" mode)))
	(backend-list (intern (format "company-backends-%S" mode))))
    `(defun ,func ()
       ,(format "Initialize company for %S" mode)
       (set (make-variable-buffer-local 'auto-completion-front-end)
	    'company)
       (set (make-variable-buffer-local 'company-backends)
	    ,backend-list))
    `(add-hook ',mode-hook ',func t)
    `(add-hook ',mode-hook 'company-mode t)))

(use-package company
  :defer t
  :init
    (setq company-idle-delay 0.2
	  company-minimum-prefix-length 2
	  company-require-match nil
	  company-dabbrev-ignore-case nil
	  company-dabbrev-downcase nil)
  :config
    (diminish company-mode)
    (let ((map company-active-map))
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection)))

(use-package magit
  :defer t
  :config
    (bind-map-set-keys my-normal-base-leader-map
      "gs" 'magit-status))

(use-package evil-magit :defer t)

(use-package projectile
  :config
  (setq projectile-cache-file (concat user-emacs-directory "projectile.cache")
        projectile-known-projects-file (concat user-emacs-directory "projectile.project")))

;; From spacemacs
(defun spacemacs//enable-rbenv ()
  (require 'rbenv)
   (let ((version-file-path (rbenv--locate-file ".ruby-version")))
    (global-rbenv-mode)
    ;; try to use the ruby defined in .ruby-version
    (if version-file-path
        (progn
          (rbenv-use (rbenv--read-version-from-file version-file-path))
          (message (concat "[rbenv] Using ruby version "
                           "from .ruby-version file.")))
      (message "[rbenv] Using the currently activated ruby."))))

;; Setup the default backends
(mattr|defvar-company-backends enh-ruby-mode)

(use-package bundler
  :defer t
  :init
  (bind-map-set-keys my-normal-base-leader-map
    "bc" 'bundle-check
    "bi" 'bundle-install
    "bs" 'bundle-console
    "bu" 'bundle-update
    "bx" 'bundle-exec
    "bo" 'bundle-open))

(use-package rbenv
  :defer t
  :init
  (add-hook 'spacemacs//enable-rbenv 'enh-ruby-mode-hook))

(use-package enh-ruby-mode
  :defer t
  :mode (("Appraisals\\'" . enh-ruby-mode)
	 ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
	 ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :init
  (progn
    (setq enh-ruby-deep-indent-paren nil
	  enh-ruby-hanging-paren-deep-indent-level 2)))

(use-package robe
  :defer t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'robe-mode)
    (push 'company-robe company-backends-enh-ruby-mode))
  :config
  (with-eval-after-load 'diminish
    (diminish 'robe-mode)))

(use-package ruby-test-mode
  :defer t
  :config
  (progn
    (bind-map-set-keys my-normal-base-leader-map
      "tt" 'ruby-test-run
      "tn" 'ruby-test-run-at-point)))

(use-package rubocop
  :defer t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'rubocop-mode))
  :config
  (progn
    (with-eval-after-load 'rubocop
      (diminish 'rubocop-mode))
    (bind-map-set-keys my-normal-base-leader-map
      "rcp" 'rubocop-check-project
      "rcd" 'rubocop-check-directory
      "rcf" 'rubocop-check-current-file
      "rcP" 'rubocop-autocorrect-project
      "rcF" 'rubocop-autocorrect-current-file
      "rcD" 'rubocop-autocorrect-directory)))

(use-package ruby-refactor
  :defer t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch))
  :config
  (progn
    (bind-map-set-keys my-normal-base-leader-map
      "rll" 'ruby-refactor-extract-to-let)))


(mattr|add-company-hook enh-ruby-mode)
(with-eval-after-load 'company-dabbrev-code
  (push 'enh-ruby-mode company-dabbrev-code-modes))

(mattr|defvar-company-backends elixir-mode)

(use-package alchemist
  :defer t
  :init
  (progn
    (add-hook 'elixir-mode-hook 'alchemist-mode)
    (setq alchemist-compile-project-when-needed t)
    (push 'alchemist-company company-backends-elixir-mode)
    )
  :config
  (progn
    (dolist (mode (list alchemist-compile-mode-map
			alchemist-eval-mode-map
			alchemist-execute-mode-map
			alchemist-message-mode-map
			alchemist-help-minor-mode-map
			alchemist-mix-mode-map
			alchemist-macroexpand-mode-map
			alchemist-refcard-mode-map
			alchemist-test-report-mode-map))
    (evil-define-key 'normal mode
	(kbd "q") 'quit-window))))

; pull in spacemacs' elixir config but comment it out so that we can see it here for reference
;;;    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
;;;      "el" 'alchemist-eval-current-line
;;;      "eL" 'alchemist-eval-print-current-line
;;;      "er" 'alchemist-eval-region
;;;      "eR" 'alchemist-eval-print-region
;;;      "eb" 'alchemist-eval-buffer
;;;      "eB" 'alchemist-eval-print-buffer
;;;      "ej" 'alchemist-eval-quoted-current-line
;;;      "eJ" 'alchemist-eval-print-quoted-current-line
;;;      "eu" 'alchemist-eval-quoted-region
;;;      "eU" 'alchemist-eval-print-quoted-region
;;;      "ev" 'alchemist-eval-quoted-buffer
;;;      "eV" 'alchemist-eval-print-quoted-buffer
;;;
;;;      "pt" 'alchemist-project-find-test
;;;      "gt" 'alchemist-project-toggle-file-and-tests
;;;      "gT" 'alchemist-project-toggle-file-and-tests-other-window
;;;
;;;      "h:" 'alchemist-help
;;;      "hH" 'alchemist-help-history
;;;      "hh" 'alchemist-help-search-at-point
;;;      "hr" 'alchemist-help-search-marked-region
;;;
;;;      "m:" 'alchemist-mix
;;;      "mc" 'alchemist-mix-compile
;;;      "mx" 'alchemist-mix-run
;;;      "mh" 'alchemist-mix-help
;;;
;;;      "'"  'alchemist-iex-run
;;;      "sc" 'alchemist-iex-compile-this-buffer
;;;      "si" 'alchemist-iex-run
;;;      "sI" 'alchemist-iex-project-run
;;;      "sl" 'alchemist-iex-send-current-line
;;;      "sL" 'alchemist-iex-send-current-line-and-go
;;;      "sm" 'alchemist-iex-reload-module
;;;      "sr" 'alchemist-iex-send-region
;;;      "sR" 'alchemist-iex-send-region-and-go
;;;
;;;      "ta" 'alchemist-mix-test
;;;      "tb" 'alchemist-mix-test-this-buffer
;;;      "tt" 'alchemist-mix-test-at-point
;;;      "tf" 'alchemist-test-file
;;;      "tn" 'alchemist-test-jump-to-next-test
;;;      "tp" 'alchemist-test-jump-to-previous-test
;;;      "tr" 'alchemist-mix-rerun-last-test
;;;
;;;      "xb" 'alchemist-execute-this-buffer
;;;      "xf" 'alchemist-execute-file
;;;      "x:" 'alchemist-execute
;;;
;;;      "cb" 'alchemist-compile-this-buffer
;;;      "cf" 'alchemist-compile-file
;;;      "c:" 'alchemist-compile
;;;
;;;      "," 'alchemist-goto-jump-back)
;;;

(push '("*alchemist test report*" :noselect t) popwin:special-display-config)
(push '("*alchemist mix*" :noselect t) popwin:special-display-config)


(use-package elixir-mode
  :defer t)

(use-package flycheck-credo
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))

(mattr|add-company-hook elixir-mode)

(use-package lua-mode
  :defer t
  :mode (("\\.lua$" . lua-mode))
  :interpreter "lua"
)

(use-package toml-mode :mode "\\.toml$")
(use-package yaml-mode :mode "\\.ya?ml$")
(use-package dockerfile-mode :mode "/Dockerfile$")
(use-package vimrc-mode
  :mode "/\\.?g?vimrc$"
  :mode "\\.vim$")

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
 '(package-selected-packages
   (quote
    (doom-themes projectile yaml-mode vimrc-mode use-package toml-mode ruby-test-mode ruby-refactor rubocop robe rbenv rainbow-delimiters popwin org-plus-contrib memoize markdown-mode lua-mode linum-relative ir-black-theme helm font-lock+ flycheck-credo evil-tabs evil-surround evil-org evil-numbers evil-matchit evil-magit enh-ruby-mode editorconfig dockerfile-mode dash-at-point bundler bind-map auto-compile alchemist))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
