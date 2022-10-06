;; Thanks to spacemacs for the good company configuration
(defvar mattr-default-company-backends
  '((company-dabbrev-code company-gtags company-etags company-keywords)
    company-files company-dabbrev)
  "The list of default company backends used by spacemacs.
This variable is used to configure mode-specific company backends in spacemacs.
Backends in this list will always be active in these modes, as well as any
backends added by individual spacemacs layers.")

(defmacro mattr|defvar-company-backends (mode)
  "Define a MODE specific company backend variable with default backends.
The variable name format is company-backends-MODE."
  `(defvar ,(intern (format "company-backends-%S" mode))
     ',mattr-default-company-backends
     ,(format "Company backend list for %S" mode)))

(use-package company
  :ensure t
  :defer t
  :init
  (progn
    (setq company-idle-delay 0.2
	  company-minimum-prefix-length 2
	  company-require-match nil
	  company-dabbrev-ignore-case nil
	  company-dabbrev-downcase nil))
  :config
  (progn
    (diminish company-mode)
    (let ((map company-active-map))
      (define-key map (kbd "C-j") 'company-select-next)
      (define-key map (kbd "C-k") 'company-select-previous)
      (define-key map (kbd "C-l") 'company-complete-selection))))

;; Steal spacemacs' add-company-hook macro so that we can configure company
;; and its backends per mode. Also slim it down since I don't intend on
;; supporting more than one completion framework
(defmacro mattr|add-company-hook (mode)
  "Enable company for the given MODE.
MODE must match the symbol passed in `spacemacs|defvar-company-backends'.
The initialization function is hooked to `MODE-hook'."
  (let ((mode-hook (intern (format "%S-hook" mode)))
	(func (intern (format "mattr//init-company-%S" mode)))
	(backend-list (intern (format "company-backends-%S" mode))))
    `(defun ,func ()
       ,(format "Initialize company for %S" mode)
       (set (make-variable-buffer-local 'auto-completion-front-end)
	    'company)
       (set (make-variable-buffer-local 'company-backends)
	    ,backend-list))
    `(add-hook ',mode-hook ',func t)
    `(add-hook ',mode-hook 'company-mode t)))


(provide 'init-company)
