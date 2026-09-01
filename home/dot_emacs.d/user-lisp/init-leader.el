;;; init-leader.el --- Leader key infrastructure -*- lexical-binding: t; -*-

;;; Commentary:

;; Establishes the leader-key convention, matching the old Neovim
;; config exactly:
;;   - global leader: SPC   (Neovim: `mapleader = " "')
;;   - local leader:  ,     (Neovim: `maplocalleader = ","')
;;
;; Implemented with `general.el', the de-facto standard package for
;; this in the Emacs/evil ecosystem (the same mechanism Doom Emacs and
;; Spacemacs-style configs use under the hood).
;;
;; `dash-leader-def' is bound via evil's "override" keymap, so it is
;; genuinely global and always wins -- mirroring `mapleader' being
;; available in every buffer regardless of filetype.
;;
;; `dash-local-leader-def' is deliberately *not* bound globally.
;; Like Neovim's `maplocalleader', which only does anything where a
;; filetype's ftplugin actually defines mappings under it, this definer
;; is meant to be called from individual major-mode init files (e.g. a
;; future `init-ruby.el') with their own `:keymaps'.  Everywhere else,
;; `,' keeps its normal evil meaning (`evil-repeat-find-char-reverse').
;;
;; This file only establishes the mechanism and one permanent
;; convention binding (`SPC SPC' for M-x).  Concern-specific bindings
;; (files, git, buffers, etc.) belong in the files that introduce those
;; concerns, not here.

;;; Code:

(use-package general
  :after evil
  :config
  (general-create-definer dash-leader-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (general-create-definer dash-local-leader-def
    :states '(normal visual motion)
    :prefix ","
    :non-normal-prefix "M-,")

  ;; Standard Spacemacs/Doom convention: leader-leader runs M-x. Kept
  ;; here (rather than in a completion- or editor-specific file) since
  ;; it's leader-key infrastructure, not tied to any particular
  ;; package we haven't set up yet.
  (dash-leader-def
    "SPC" '(execute-extended-command :which-key "M-x")))

(provide 'init-leader)
;;; init-leader.el ends here
