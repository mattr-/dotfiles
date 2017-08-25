(use-package editorconfig
  :ensure t
  :init
  (progn
    (with-eval-after-load 'editorconfig
      (diminish 'editorconfig-mode)))
  :config
  (progn
    (editorconfig-mode 1)))

(provide 'init-editorconfig)
