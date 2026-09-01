;;; init.el --- Main initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;; Entry point for this config.  This file:
;;   1. Bootstraps the `elpaca' package manager.
;;   2. Wires up `use-package' to install packages via elpaca.
;;   3. Decides how tree-sitter grammar management is handled,
;;      depending on whether we're running Emacs 31+ or a fallback.
;;   4. Requires each per-concern module from `user-lisp/'.
;;
;; Per-concern configuration lives in individual `init-*.el' files
;; under `user-lisp/', each of which `(provide)'s a matching feature
;; symbol and is `require'd from the bottom of this file.  See
;; `user-lisp/README.md' for the expected shape of those files.
;;
;; Minimum supported Emacs version: 29.1.
;; Recommended/target Emacs version: 31.

;;; Code:

;; --- Elpaca bootstrap -------------------------------------------------
;; Verbatim from https://github.com/progfolio/elpaca#installer
;; (elpaca-installer-version 0.12, as published there).  Don't hand-edit
;; this block without checking upstream for changes.

(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; --- use-package + elpaca integration ----------------------------------

(elpaca elpaca-use-package
  ;; Enable `:ensure' support so `use-package' forms install missing
  ;; packages via elpaca automatically.
  (elpaca-use-package-mode))

;; Virtually everything in this config is a third-party package, so
;; default every `use-package' form to `:ensure t' instead of repeating
;; it everywhere. Built-in features can opt out with `:ensure nil'.
(setq use-package-always-ensure t)

;; --- Tree-sitter grammar management (Emacs 31+ vs. fallback) ------------
;;
;; Emacs 31 added built-in `treesit-enabled-modes' and
;; `treesit-auto-install-grammar', which together cover automatically
;; switching to `-ts-mode' variants and installing missing grammars --
;; the same job the third-party `treesit-auto' package does on earlier
;; versions.  Per-language grammar sources still need to be configured
;; (that will live in a future `init-treesit.el'); this block just
;; decides which mechanism owns that job.

(if (boundp 'treesit-enabled-modes)
    (setq treesit-enabled-modes t
          treesit-auto-install-grammar 'ask)
  (use-package treesit-auto
    :demand t
    :custom
    (treesit-auto-install 'prompt)
    :config
    (global-treesit-auto-mode)))

;; --- Per-concern modules -------------------------------------------------
;; Each lives in `user-lisp/init-<concern>.el', `provide's a matching
;; feature symbol, and is loaded here.  Order matters where one
;; concern's keybindings/hooks assume another has already run (e.g.
;; init-theme before packages that add theme-specific faces, and evil
;; plus init-leader before init-vertico, which uses `dash-leader-def').

(require 'init-theme)
(require 'init-evil)
(require 'init-leader)
(require 'init-vertico)

(provide 'init)
;;; init.el ends here
