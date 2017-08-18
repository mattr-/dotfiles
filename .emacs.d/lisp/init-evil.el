(setq evil-want-C-u-scroll t) ; addicted to C-u scrolling

(use-package evil :ensure t)
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :ensure t
  :config
  (global-evil-matchit-mode 1))

(use-package evil-numbers :ensure t)


(evil-mode 1)

(provide 'init-evil)
