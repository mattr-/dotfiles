(use-package bind-map :ensure t)

(bind-map my-base-leader-map
          :keys ("M-m")
          :evil-keys (",")
          :evil-states (normal motion visual))

(provide 'init-keybindings)
