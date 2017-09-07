(mattr|defvar-company-backends elixir-mode)

(use-package alchemist
  :ensure t
  :init
  (progn
    (add-hook 'elixir-mode-hook 'alchemist-mode)
    (setq alchemist-compile-project-when-needed t)
    (push 'alchemist-company company-backends-elixir-mode)
    ))

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
;;;    (dolist (mode (list alchemist-compile-mode-map
;;;                        alchemist-eval-mode-map
;;;                        alchemist-execute-mode-map
;;;                        alchemist-message-mode-map
;;;                        alchemist-help-minor-mode-map
;;;                        alchemist-mix-mode-map
;;;                        alchemist-macroexpand-mode-map
;;;                        alchemist-refcard-mode-map
;;;                        alchemist-test-report-mode-map))
;;;      (evil-define-key 'normal mode
;;;        (kbd "q") 'quit-window))))
;;;

(use-package elixir-mode
  :ensure t
  :defer t)

(use-package flycheck-credo
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-credo-setup))

(mattr|add-company-hook elixir-mode)

(provide 'init-elixir)
