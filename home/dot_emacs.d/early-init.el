;;; early-init.el --- Early initialization -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs loads this file before `package.el' initializes and before
;; the first frame is created.  Anything that affects frame chrome,
;; package-manager bootstrapping, or early startup performance belongs
;; here rather than in `init.el', so it takes effect before Emacs has
;; already drawn a frame or activated packages.
;;
;; Minimum supported Emacs version: 29.1.
;; Recommended/target Emacs version: 31.
;; Emacs-31-only behavior is guarded with `fboundp'/`boundp' checks and
;; falls back to equivalent behavior on 29/30 -- see comments below.

;;; Code:

;; --- Startup performance ----------------------------------------------

;; Raise the GC threshold during startup so we're not stopping the
;; world to collect garbage while loading `init.el' and installing
;; packages.  Restored to a saner value once startup has finished.
(defvar dash/gc-cons-threshold (* 16 1024 1024) ; 16MB
  "Value `gc-cons-threshold' is restored to after startup completes.")
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold dash/gc-cons-threshold)))

;; Avoid expensive/flickery frame resizes while we strip down the UI
;; immediately below.
(setq frame-inhibit-implied-resize t)

;; --- UI decluttering ----------------------------------------------------
;; Done here, rather than in `init.el', so the frame never shows the
;; default chrome before it gets disabled a moment later.

(setq inhibit-startup-screen t)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

;; --- package.el -----------------------------------------------------------
;; This config uses `elpaca' (see init.el) instead of package.el, so
;; disable package.el's own startup activation entirely.
(setq package-enable-at-startup nil)

;; --- User Lisp directory (Emacs 31+, with fallback) ------------------------
;;
;; Emacs 31 introduced a built-in "User Lisp directory" facility: any
;; .el files placed in `user-lisp-directory' are automatically
;; byte-compiled, scraped for autoload cookies, and added to
;; `load-path' at startup -- see (info "(emacs) User Lisp Directory").
;;
;; `user-lisp-directory' must be set here, in early-init.el, because
;; its value is consulted before the regular init file is loaded.
;;
;; Emacs 31 processes `user-lisp-directory' before loading `init.el'.
;; Automatic scraping also byte/native-compiles these modules at that
;; point.  That is too early for us since we depend on Elpaca
;; integration enabled later in `init.el'.  Disable automatic
;; compilation so the forms are expanded when the modules are loaded
;; after Elpaca has been initialized.
;;
;; With auto-scraping disabled, Emacs 31 still activates the directory
;; and adds it to `load-path'.  On Emacs 29/30, add it manually.

(defvar dash/lisp-directory (expand-file-name "user-lisp/" user-emacs-directory)
  "Directory containing this config's per-concern Lisp files.")

(if (boundp 'user-lisp-directory)
    ;; Emacs 31+: Activate the directory but skip auto compilation
    ;; and scraping so we can integrate with elpaca
    (setq user-lisp-directory dash/lisp-directory
          user-lisp-auto-scrape nil)
  ;; Emacs 29/30 fallback: just get our directory onto `load-path'.
  (add-to-list 'load-path dash/lisp-directory))

;;; early-init.el ends here
