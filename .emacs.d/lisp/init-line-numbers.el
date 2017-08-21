(use-package linum-relative
  :commands (linum-relative-toggle linum-relative-on))

(require 'linum)

(defcustom linum-disabled-modes-list
  '(eshell-mode
    wl-summary-mode
    compilation-mode
    org-mode
    dired-mode
    doc-view-mode
    image-mode)
  "* List of modes disabled when global linum mode is no"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes where linum is disabled:"
  :group 'linum
  )

(defcustom linum-disable-starred-buffers 't
  "* Disable buffers that have stars in them like *scratch*"
  :type 'boolean
  :group 'linum)

(defun linum-on ()
  "* When linum is running globally, disable line number in modes defined in `linum-disabled-modes-list'. Changed by linum-off. Also turns off numbering in starred modes like *scratch*"

  (unless (or (minibufferp)
	      (member major-mode linum-disabled-modes-list)
	      (string-match "*" (buffer-name))
	      (> (buffer-size) 3000000)) ;; disable for buffers bigger than 3MB
    (linum-mode 1)))


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

(global-linum-mode 1)

(bind-map-set-keys my-base-leader-map
  "nr" 'linum-relative-toggle)

(provide 'init-line-numbers)
