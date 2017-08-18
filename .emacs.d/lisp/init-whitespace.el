(setq whitespace-style '(trailing lines tab-mark))
(setq whitespace-line-column 90)
(setq-default show-trailing-whitespace t)
(require 'whitespace)
(global-whitespace-mode 1)
(eval-after-load "diminish"
'(progn
   (eval-after-load "whitespace"
     '(diminish 'global-whitespace-mode))
   (eval-after-load "whitespace"
     '(diminish 'whitespace-mode))))


(provide 'init-whitespace)
