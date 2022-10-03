;;; init-popwin --- and the summary
;;; Commentary:
;;; Stuff goes here

;;; Code:
(use-package popwin
  :ensure t
  :config
  (progn
    (popwin-mode 1)
    (bind-map-set-keys my-base-leader-map
      "wpm" 'popwin:messages
      "wpp" 'popwin:close-popup-window)))


(provide 'init-popwin)
;;; init-popwin.el ends here
