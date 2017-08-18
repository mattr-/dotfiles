(use-package org-plus-contrib :ensure t)
(setq org-directory "~/Dropbox/org")
(setq org-default-notes-file "~/Dropbox/org/todo.org")

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files '("~/Dropbox/org"))


(provide 'init-org-mode)
