;;; +git.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! git-link
  (setq git-link-open-in-browser nil
        git-link-use-commit t)

  ;; OVERRIDE
  (advice-add #'git-link--select-remote :override #'git-link--read-remote))

(use-package! magit-delta
  :after magit
  :init
  (when (executable-find "delta")
    (add-hook! magit-mode #'magit-delta-mode))
  :config
  (setq magit-delta-default-light-theme "GitHub")
  (setq magit-delta-default-dark-theme "OneHalfDark")
  )

(after! magit-todos
  (setq magit-todos-exclude-globs '("third-party/*" "third_party/*")))


;; magit-todos uses hl-todo-keywords
(custom-theme-set-faces! doom-theme
  `(hl-todo :foreground ,(doom-color 'bg)))
(after! hl-todo
  (setq hl-todo-color-background t
        hl-todo-keyword-faces
        `(("TODO"  . ,(doom-color 'orange))
          ("HACK"  . ,(doom-color 'orange))
          ("TEMP"  . ,(doom-color 'orange))
          ("DONE"  . ,(doom-color 'green))
          ("NOTE"  . ,(doom-color 'green))
          ("DONT"  . ,(doom-color 'red))
          ("DEBUG"  . ,(doom-color 'red))
          ("FAIL"  . ,(doom-color 'red))
          ("FIXME" . ,(doom-color 'red))
          ("XXX"   . ,(doom-color 'blue))
          ("XXXX"  . ,(doom-color 'blue)))))
