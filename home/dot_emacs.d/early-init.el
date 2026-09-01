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
;; On Emacs < 31 neither the variable nor the underlying machinery
;; exists, so we fall back to adding the same directory to `load-path'
;; ourselves.  Byte-compilation on <31 happens lazily and naturally via
;; the normal `load'/`require' machinery instead of being managed for
;; us; that's an acceptable trade-off for the fallback path.

(defvar dash/lisp-directory (expand-file-name "user-lisp/" user-emacs-directory)
  "Directory containing this config's per-concern Lisp files.")

(if (boundp 'user-lisp-directory)
    ;; Emacs 31+: let the built-in machinery byte-compile, scrape
    ;; autoloads, and manage `load-path' for us.
    (setq user-lisp-directory dash/lisp-directory)
  ;; Emacs 29/30 fallback: just get our directory onto `load-path'.
  (add-to-list 'load-path dash/lisp-directory))

;;; early-init.el ends here
