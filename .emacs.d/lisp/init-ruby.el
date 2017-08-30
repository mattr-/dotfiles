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
  :ensure t
  :defer t
  :init
  (bind-map-set-keys my-base-leader-map
    "bc" 'bundle-check
    "bi" 'bundle-install
    "bs" 'bundle-console
    "bu" 'bundle-update
    "bx" 'bundle-exec
    "bo" 'bundle-open))

(use-package rbenv
  :ensure t
  :init
  (add-hook 'spacemacs//enable-rbenv 'enh-ruby-mode-hook))

(use-package enh-ruby-mode
  :ensure t
  :mode (("Appraisals\\'" . enh-ruby-mode)
	 ("\\(Rake\\|Thor\\|Guard\\|Gem\\|Cap\\|Vagrant\\|Berks\\|Pod\\|Puppet\\)file\\'" . enh-ruby-mode)
	 ("\\.\\(rb\\|rabl\\|ru\\|builder\\|rake\\|thor\\|gemspec\\|jbuilder\\)\\'" . enh-ruby-mode))
  :interpreter "ruby"
  :init
  (progn
    (setq enh-ruby-deep-indent-paren nil
	  enh-ruby-hanging-paren-deep-indent-level 2)))

(use-package robe
  :ensure t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'robe-mode)
    (push 'company-robe company-backends-enh-ruby-mode))
  :config
  (with-eval-after-load 'diminish
    (diminish 'robe-mode)))

(use-package ruby-test-mode
  :ensure t
  :config
  (progn
    (bind-map-set-keys my-base-leader-map
      "tt" 'ruby-test-run
      "tn" 'ruby-test-run-at-point)))

(use-package rubocop
  :ensure t
  :defer t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'rubocop-mode))
  :config
  (progn
    (with-eval-after-load 'rubocop
      (diminish 'rubocop-mode))
    (bind-map-set-keys my-base-leader-map
      "rcp" 'rubocop-check-project
      "rcd" 'rubocop-check-directory
      "rcf" 'rubocop-check-current-file
      "rcP" 'rubocop-autocorrect-project
      "rcF" 'rubocop-autocorrect-current-file
      "rcD" 'rubocop-autocorrect-directory)))

(use-package ruby-refactor
  :ensure t
  :init
  (progn
    (add-hook 'enh-ruby-mode-hook 'ruby-refactor-mode-launch))
  :config
  (progn
    (bind-map-set-keys my-base-leader-map
      "rll", 'ruby-refactor-extract-to-let)))


(mattr|add-company-hook enh-ruby-mode)
(with-eval-after-load 'company-dabbrev-code
  (push 'enh-ruby-mode company-dabbrev-code-modes))


(provide 'init-ruby)
