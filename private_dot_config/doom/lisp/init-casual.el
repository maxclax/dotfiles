;;; lisp/init-casual.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Casual — transient menus for built-in Emacs modes
;; Single entry point: C-o auto-detects current mode and opens the right menu.
;; https://github.com/kickingvegas/casual

(use-package! casual

  :config

  (defun my/casual-open ()
    "Open the Casual menu for the current major mode."
    (interactive)
    (cond
     ((derived-mode-p 'dired-mode)      (casual-dired-tmenu))
     ((derived-mode-p 'ibuffer-mode)    (casual-ibuffer-tmenu))
     ((derived-mode-p 'org-agenda-mode) (casual-agenda-tmenu))
     ((derived-mode-p 'org-mode)        (casual-org-tmenu))
     ((derived-mode-p 'calc-mode)       (casual-calc-tmenu))
     ((derived-mode-p 'Info-mode)       (casual-info-tmenu))
     ((derived-mode-p 'help-mode)       (casual-help-tmenu))
     ((derived-mode-p 'makefile-mode)   (casual-make-tmenu))
     ((derived-mode-p 'css-mode)        (casual-css-tmenu))
     ((derived-mode-p 'web-mode)        (casual-html-tmenu))
     ((derived-mode-p 'html-mode)       (casual-html-tmenu))
     (t                                 (casual-editkit-main-tmenu))))

  )

(provide 'init-casual)
