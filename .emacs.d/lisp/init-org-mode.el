(use-package org-plus-contrib :ensure t)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-agenda-files '("~/Dropbox/org"))


(provide 'init-org-mode)
