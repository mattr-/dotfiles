(use-package flycheck :ensure t
  :init
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (with-eval-after-load 'flycheck
      (diminish 'flycheck-mode " ⓢ"))))

(provide 'init-flycheck)
