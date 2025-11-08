;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Load all configuration modules from the root directory
(load! "+os")
(load! "+git")
(load! "+misc")
(load! "+text")
(load! "+prog")
(load! "+ui")
(load! "+keys")
(load! "+denote")

;; Autoload all files from the modules directory
(let ((modules-dir (expand-file-name "helpers" doom-user-dir)))
  (when (file-directory-p modules-dir)
    (dolist (file (directory-files modules-dir t "\\.el$"))
      (load file nil 'nomessage))))

;; Conditional LSP configuration
(cond
 ((modulep! :tools lsp +eglot) (load! "+eglot"))
 ((modulep! :tools lsp) (load! "+lsp")))

(setq bookmark-default-file "~/.config/doom/bookmarks")

(setq user-full-name "dev"
      user-mail-address "dev@site.com")

(setq doom-scratch-buffer-major-mode 'emacs-lisp-mode
      confirm-kill-emacs nil)

(setq-default fill-column 120
              delete-trailing-lines t)

;; Delete the selection when pasting
(delete-selection-mode 1)

;; Remap Cmd+C and Cmd+V to copy/paste in GUI Emacs
;; Note: s- is remapped to M- in +keys.el, so we use M- here
(when (display-graphic-p)
  (global-set-key (kbd "M-c") 'kill-ring-save) ; Cmd+C (remapped to Meta)
  (global-set-key (kbd "M-v") 'yank))          ; Cmd+V (remapped to Meta)

;; disable risky local variables warning
(advice-add 'risky-local-variable-p :override #'ignore)

(add-hook! 'find-file-hook #'+my/find-file-check-make-large-file-read-only-hook)

;; Global Auto-Revert Mode is a global minor mode that reverts any
;; buffer associated with a file when the file changes on disk
(global-auto-revert-mode)

;; check minified-file
(add-to-list 'magic-mode-alist (cons #'+my/check-minified-file 'fundamental-mode))

;; Manually edit .local/custom.el will break doom updates
(when (file-directory-p custom-file)
  (message (concat "Please delete " custom-file ". And customization in config.el and +ui.el.")))

(custom-set-variables
 '(warning-suppress-log-types '((lsp-mode) (iedit)))
 '(warning-suppress-types '((iedit))))

;; Load system profile for different machines and work config
(dolist (config '("~/.config/doom/local.el"))
  (let ((config-file (file-truename config)))
    (when (file-exists-p config-file)
      (load-file config-file))))
