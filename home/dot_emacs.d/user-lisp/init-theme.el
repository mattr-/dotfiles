;;; init-theme.el --- Color themes -*- lexical-binding: t; -*-

;;; Commentary:

;; Install both theme collections so switching later does not require
;; changing the package setup:
;;
;;   - doom-themes       broad, well-supported theme collection
;;   - catppuccin-theme  Catppuccin's Latte, Frappe, Macchiato, and Mocha flavors
;;
;; Use `doom-one' for now. It is doom-themes' flagship dark theme and
;; the default theme used by Doom Emacs. Catppuccin remains installed
;; but disabled.

;;; Code:

(defun dash-load-theme (theme)
  "Disable active themes, then load THEME without confirmation.

Emacs can layer multiple enabled themes. Disabling the active themes
first makes switching deterministic and lets doom-themes and
catppuccin-theme coexist without their faces being combined."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(use-package doom-themes
  ;; Wait so the selected theme is available during startup instead of
  ;; leaving the initial frame with the default theme while Elpaca's
  ;; asynchronous queue catches up.
  :ensure (:wait t)
  :demand t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (dash-load-theme 'doom-one))

(use-package catppuccin-theme
  :defer t
  :init
  ;; Match the Catppuccin flavor used by the old Neovim config when we
  ;; eventually switch to it. The package's default is also Mocha, but
  ;; spelling it out records the decision explicitly.
  (setq catppuccin-flavor 'mocha))

(provide 'init-theme)
;;; init-theme.el ends here
