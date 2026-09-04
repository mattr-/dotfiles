;;; init-vertico.el --- Fuzzy finding & minibuffer completion -*- lexical-binding: t; -*-

;;; Commentary:

;; This is the Emacs equivalent of `fzf-lua': fuzzy file/buffer/grep
;; finding, plus every other minibuffer prompt in Emacs, since
;; `completing-read' is used pervasively throughout Emacs, not just
;; for file finding.
;;
;; Packages:
;;   - vertico          vertical, incremental minibuffer completion UI
;;   - vertico-directory nicer path navigation while finding files (part of vertico itself)
;;   - savehist         persist minibuffer history across restarts; vertico sorts by it
;;   - consult          fuzzy-searchable commands built on vertico (files, grep, buffers, ...)
;;   - marginalia       annotations for completion candidates (keybindings, docstrings, etc.)
;;   - embark           act on the candidate at point, from any completion UI
;;   - embark-consult   glue between embark and consult
;;   - fussy            completion-style that can be backed by real fuzzy-matching engines
;;   - fzf-native       the actual `fzf' (junegunn) matching algorithm, as a native module
;;
;; `fussy' + `fzf-native' replace `orderless' entirely in this config.
;; `orderless' only does space-separated component AND-matching with a
;; configurable (but not fzf-identical) per-component style, whereas
;; `fzf-native' is literally fzf's own scoring algorithm (ported to C
;; from `telescope-fzf-native.nvim'), and `fussy' documents matching
;; fzf's actual operator syntax (`!', `^', `$', `|', space-separated
;; AND, `'' to force exact/fuzzy per term). This was a deliberate
;; choice to match the specific reason `fzf-lua' was chosen in the old
;; Neovim config -- fzf's search syntax and algorithm, not just "some
;; kind of fuzzy matching."

;;; Code:

(use-package vertico
  :init
  (vertico-mode))

;; Nicer directory navigation while finding files (part of the vertico
;; repo itself, not a separate package -- hence :ensure nil).
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Persist history over restarts; vertico sorts by history position,
;; giving a mild frecency-like effect for free.
(use-package savehist
  :ensure nil ; built into Emacs, not a separately-hosted package
  :init
  (savehist-mode))

;; Baseline minibuffer behavior recommended by vertico's own README.
(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :after vertico
  :config
  (dash-leader-def
    "f" '(:ignore t :which-key "find")
    "f f" '(consult-fd :which-key "find files")
    "/" '(consult-ripgrep :which-key "live grep")
    "b" '(:ignore t :which-key "buffer")
    "b b" '(consult-buffer :which-key "switch buffer")))

(use-package embark
  :after (vertico general)
  :bind
  (("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; `embark-act'/`embark-dwim' are meant to work from *any* evil
  ;; state, but a plain `use-package :bind' only binds in the
  ;; low-precedence `global-map', which evil's own state keymaps
  ;; shadow -- e.g. evil already claims "C-." for `evil-repeat-pop' in
  ;; normal state, silently swallowing a naive embark binding. Use the
  ;; same `general.el' override mechanism as the leader key (see
  ;; init-leader.el) so these always win regardless of evil state.
  (general-def
    :states '(normal insert visual motion emacs)
    :keymaps 'override
    "C-." 'embark-act
    "C-;" 'embark-dwim))

;; Only needs installing -- embark loads it after consult if present.
(use-package embark-consult
  :after (embark consult))

;; --- Matching engine: fussy + fzf-native --------------------------------

(use-package fzf-native
  :ensure t)

(use-package fussy
  :after fzf-native
  :config
  ;; Tie-breaking heuristic recommended by fussy's own author for the
  ;; fzf-native backend: among candidates with an identical score,
  ;; prefer ones used more recently in minibuffer history, then
  ;; shorter ones.
  (setq fussy-compare-same-score-fn 'fussy-histlen->strlen<)

  ;; Wires up `completion-styles'/`completion-category-overrides' for
  ;; fzf-native scoring (pushes `fussy' onto `completion-styles' rather
  ;; than replacing it, and falls back to `basic' per-category where
  ;; fussy shouldn't apply, e.g. TRAMP paths); also applies the
  ;; `consult' tofu-char compatibility fix documented at
  ;; https://github.com/minad/consult/issues/585.
  (fussy-setup-fzf)

  ;; Safe to call now even though `eglot'/`corfu' aren't configured
  ;; yet -- both are `with-eval-after-load'-guarded internally, so
  ;; they'll take effect automatically once those concerns exist.
  (fussy-eglot-setup)
  (fussy-corfu-setup))

(provide 'init-vertico)
;;; init-vertico.el ends here
