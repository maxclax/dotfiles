;;; +misc.el -*- lexical-binding: t; -*-

(use-package! screenshot
  :defer t)

(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project))

(use-package consult-todo
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! keycast
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! dirvish
  (setq dirvish-quick-access-entries
        `(("h" "~/" "Home")
          ("c" "~/.config" "config")
          ("C" "~/.config/chezmoi" "config/chezmoi")
          ("d" "~/.local/share/chezmoi" "Dotfiles")
          ("D" "~/Downloads" "Downloads")
          ("p" "~/playground" "Playground")
          ("w" "~/workspace" "Workspace")
          ("e" ,doom-user-dir "Doom directory")
          ("E" ,doom-emacs-dir "Emacs directory")
          ))
)
