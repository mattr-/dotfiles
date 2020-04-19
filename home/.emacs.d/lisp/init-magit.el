(use-package magit :ensure t)

(use-package evil-magit :ensure t)

(bind-map-set-keys my-base-leader-map
                   "gs" 'magit-status
                   )

(provide 'init-magit)
