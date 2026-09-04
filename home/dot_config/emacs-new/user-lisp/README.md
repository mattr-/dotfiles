# `user-lisp/`

This directory holds the per-concern configuration files for this Emacs
config, one file per topic (e.g. `init-evil.el`, `init-completion.el`,
`init-git.el`). This mirrors the structure of the old Neovim config
(`dashvim/plugins/*.lua`), just in Elisp.

## Conventions

- Each file is named `init-<concern>.el`.
- Each file ends with `(provide 'init-<concern>)`.
- Each file is `require`d from the bottom of `../init.el`, in an order
  that respects dependencies between concerns (e.g. `init-evil` before
  anything that assumes evil is already loaded).
- Package installation and configuration happens via `use-package`
  (with `:ensure` implied by `use-package-always-ensure`, set in
  `init.el`), so a typical file looks like:

  ```elisp
  ;;; init-example.el --- Short description -*- lexical-binding: t; -*-

  ;;; Commentary:
  ;; What this file is for, and why, in a sentence or two.

  ;;; Code:

  (use-package some-package
    :config
    (some-package-mode 1))

  (provide 'init-example)
  ;;; init-example.el ends here
  ```

## Why this directory, specifically

On Emacs 31+, `../early-init.el` points the built-in `user-lisp-directory`
facility at this directory, so every file here is automatically
byte-compiled, scraped for autoload cookies, and added to `load-path` at
startup -- see `(info "(emacs) User Lisp Directory")`.

On Emacs 29/30, this directory is just added to `load-path` directly
instead (see the fallback branch in `early-init.el`); byte-compilation
happens lazily via the normal `load`/`require` machinery rather than
being managed automatically.

Either way, files are added here the same way regardless of Emacs
version -- only the mechanism that picks them up differs.
