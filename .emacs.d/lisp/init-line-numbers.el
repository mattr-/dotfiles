
(unless window-system
  (add-hook 'linum-before-numbering-hook
  (lambda ()
    (setq-local linum-format-fmt
		(let ((w (length (number-to-string
				  (count-lines (point-min) (point-max))))))
		  (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line)))

(unless window-system
  (setq linum-format 'linum-format-func))

(provide 'init-line-numbers)
