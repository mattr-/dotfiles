;;; init-defaults.el --- Core editor defaults -*- lexical-binding: t; -*-

;;; Commentary:

;; General editing behavior shared by all modes. This translates the
;; useful parts of the old Neovim options.lua and keymaps.lua without
;; trying to force every Vim option onto Emacs.
;;
;; Package-specific settings belong in their respective init files.

;;; Code:

;; --- Customization -----------------------------------------------------

;; Keep settings written by Emacs's Customize interface out of init.el.
;; Intentional settings belong in the appropriate init-*.el file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; --- General behavior -------------------------------------------------

;; Use short y/n answers instead of requiring yes/no.
(setq use-short-answers t)

;; Keep prompts in the minibuffer instead of opening GUI dialog boxes.
(setq use-dialog-box nil
      use-file-dialog nil)

;; UTF-8 everywhere. It's 2026 after all.
(prefer-coding-system 'utf-8)

;; No sound!
(setq visible-bell t)

;; One space to end a sentence. 
(setq sentence-end-double-space nil)

;; Reload unmodified buffers when they change on disk
(global-auto-revert-mode 1)

;; Remember the last cursor position in visited files.
(save-place-mode 1)

;; --- Files and recovery -----------------------------------------------

;; Most places I work in don't need backups, autosaves, or lock files.
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; --- Lines and cursor -------------------------------------------------

;; Absolute line numbers, matching `number = true' without
;; `relativenumber' in the old Neovim config.
(setq display-line-numbers-type t)
(global-display-line-numbers-mode 1)

;; Line numbers are useful in editing buffers but mostly noise in help,
;; package-manager, compilation, and other special-purpose buffers.
(defun dash-disable-line-numbers ()
  "Disable line numbers in the current buffer."
  (display-line-numbers-mode -1))

(add-hook 'special-mode-hook #'dash-disable-line-numbers)

;; Show line and column position in the mode line.
(line-number-mode 1)
(column-number-mode 1)

;; Highlight the current line and matching delimiters.
(global-hl-line-mode 1)
(show-paren-mode 1)

;; Emacs 31 can avoid highlighting matching parens in comments and
;; strings. Earlier versions retain show-paren-mode's old behavior.
(when (boundp 'show-paren-not-in-comments-or-strings)
  (setq show-paren-not-in-comments-or-strings 'on-mismatch))

;; --- Scrolling and wrapping -------------------------------------------

;; Do not wrap long lines by default.
(setq-default truncate-lines t)

;; Keep context around the cursor, corresponding to Neovim's
;; scrolloff=3 and sidescrolloff=8.
(setq scroll-margin 3
      hscroll-margin 8)

;; Scroll by the smallest useful amount rather than recentering the
;; window aggressively.
(setq scroll-conservatively 101
      scroll-preserve-screen-position t)

;; --- Search ------------------------------------------------------------

;; Ignore case unless the query contains an uppercase character.
(setq-default case-fold-search t)
(setq search-upper-case t
      evil-ex-search-case 'smart)

;; Evil already implements Vim's direction-aware n/N behavior, so no
;; replacement mappings are necessary.

;; --- Indentation -------------------------------------------------------

;; Spaces and two-column indentation by default. Individual language
;; modes can override their own language-specific offset variables.
(setq-default indent-tabs-mode nil
              tab-width 2
              standard-indent 2)

;; Make Evil's </> operators use the same width and round indentation
;; to the nearest shift boundary.
(setq evil-shift-width 2
      evil-shift-round t)

;; Explicit Evil splits open below and to the right, matching
;; splitbelow/splitright.
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; --- Whitespace --------------------------------------------------------

;; Show tabs and trailing whitespace in source and prose buffers. This
;; corresponds to the useful parts of Neovim's list/listchars settings
;; without marking every ordinary space.
(use-package whitespace
  :ensure nil
  :custom
  (whitespace-style '(face tabs tab-mark trailing))
  (whitespace-display-mappings
   '((tab-mark ?\t [?\u25b8 ?\t])))
  :hook
  ((prog-mode . whitespace-mode)
   (text-mode . whitespace-mode)))

;; --- Buffer and search keybindings ------------------------------------

(defun dash-switch-to-other-buffer ()
  "Switch to the most recently used buffer."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

;; Emacs has no useful stable equivalent for Vim's :brewind/:blast
;; because its buffer list also contains internal and temporary
;; buffers, so [B/]B are intentionally omitted.
(general-def
  :states '(normal visual motion)
  :keymaps 'override
  "[b" 'previous-buffer
  "]b" 'next-buffer)

;; Clear Evil's search highlighting without changing its search pattern.
(general-def
  :states 'normal
  :keymaps 'override
  "RET" 'evil-ex-nohighlight)

(dash-leader-def
  "`" '(dash-switch-to-other-buffer :which-key "other buffer")
  "b d" '(kill-current-buffer :which-key "kill buffer"))

(provide 'init-defaults)
;;; init-defaults.el ends here
