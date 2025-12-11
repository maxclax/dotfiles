;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Load all configuration modules
(load! "lisp/init-dired.el")
(load! "lisp/init-templates.el")
(load! "lisp/init-lsp.el")
(load! "lisp/init-reading.el")
(load! "lisp/init-git.el")
(load! "lisp/init-org.el")
(load! "lisp/init-pomodoro.el")
(load! "lisp/init-bibliography.el")
(load! "lisp/init-ai.el")
(load! "lisp/init-denote")
(load! "lisp/init-bookmarks.el")
(load! "lisp/init-whichkey.el")
(load! "lisp/init-windows.el")
(load! "lisp/init-erc.el")
(load! "lisp/init-private.el")
(load! "lisp/init-tramp.el")
(load! "+misc")
(load! "+ui")
(load! "+keys")

;; delete to trash
(setq delete-by-moving-to-trash t)

;; Increase kill ring size (default is 120)
(setq kill-ring-max 250)


(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)

;; Enable repeat mode for repeatable keys (Emacs 28+)
(when (fboundp 'repeat-mode)
  (repeat-mode 1))

;; Keep everything in sync with the real world
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)   ; ← silence the "reverted buffer …" messagen
