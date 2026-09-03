;;; init-evil.el --- Modal editing -*- lexical-binding: t; -*-

;;; Commentary:

;; Evil and its companion packages. 20 years of thinking in Vim is
;; hard to break from.
;;
;;   - evil                     modal editing itself
;;   - evil-collection          evil keybindings for built-in & third-party modes
;;   - evil-surround            `ys'/`cs'/`ds' surround text objects
;;   - evil-commentary          `gcc'/`gc{motion}' comment toggling
;;   - evil-numbers             `C-a'/`C-x' increment/decrement
;;   - evil-textobj-tree-sitter tree-sitter-powered `af'/`if' style text objects
;;

;;; Code:

(use-package evil
  :init
  ;; These *must* be set before evil loads.
  (setq evil-want-integration t   ; required by evil-collection
        evil-want-keybinding nil  ; ditto -- let evil-collection manage keymaps
        evil-want-C-u-scroll t    ; C-u scrolls instead of acting as a prefix arg
        evil-want-C-i-jump nil    ; avoid C-i/TAB ambiguity (matters once corfu is in the picture)
        evil-respect-visual-line-mode t
        evil-symbol-word-search t ; Let `#` and `*` search for symbols rather than words
        shift-select-mode nil     ; Avoid momentarily mark activation
        evil-undo-system 'undo-redo) ; use Emacs 28+'s built-in redo, no extra package needed
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-numbers
  :after evil
  :config
  ;; True vim parity: C-a/C-x increment/decrement, g C-a/g C-x for vim's
  ;; "incremental" sequential numbering (e.g. selecting a column of
  ;; `0's and turning them into 1, 2, 3, ...).
  (evil-define-key '(normal visual) 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
  (evil-define-key '(normal visual) 'global (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
  (evil-define-key '(normal visual) 'global (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  ;; Mirrors the `mini.ai' custom text objects from the old Neovim
  ;; config: `o' for block/conditional/loop, `f' for function, `c' for
  ;; class.  Works against either the built-in `treesit' or the older
  ;; `elisp-tree-sitter', so it's compatible with our built-in-treesit
  ;; choice with no extra setup.
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
  ;; NOTE: `evil-textobj-tree-sitter-get-textobj' is a macro; when
  ;; passing multiple textobj names it expects a literal, *unquoted*
  ;; list -- see the package README.  A quoted list here throws
  ;; "Wrong type argument: sequencep, quote".
  (define-key evil-outer-text-objects-map "o"
    (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
  (define-key evil-inner-text-objects-map "o"
    (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))

  ;; Mirrors `nvim-treesitter-textobjects' move bindings: ]f/[f/]F/[F
  ;; and ]c/[c/]C/[C jump to the next/previous function or class
  ;; start/end.
  (define-key evil-normal-state-map (kbd "]f")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  (define-key evil-normal-state-map (kbd "[f")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  (define-key evil-normal-state-map (kbd "]F")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[F")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
  (define-key evil-normal-state-map (kbd "]c")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer")))
  (define-key evil-normal-state-map (kbd "[c")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  (define-key evil-normal-state-map (kbd "]C")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[C")
    (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "class.outer" t t))))

(provide 'init-evil)
;;; init-evil.el ends here
