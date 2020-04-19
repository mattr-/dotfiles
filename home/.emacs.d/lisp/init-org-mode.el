(use-package org-plus-contrib :ensure t)
; (use-package org-evil
;   :ensure t
;   :after org
;   :commands (evil-org-mode evil-org-recompute-clocks)
;   :init
;   (add-hook 'org-mode-hook 'evil-org-mode)
;   (add-hook 'evil-org-mode-hook
;         (lambda()
;           (evil-org-set-key-theme))))

(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/todo.org")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files '("~/Dropbox/org"))


(provide 'init-org-mode)
