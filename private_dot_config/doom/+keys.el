;;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

(map! :leader
      :desc "Select all in buffer" "ba" #'(lambda () (interactive) (mark-whole-buffer))
      :desc "Make buffer empty without yank" "be" #'(lambda () (interactive) (let ((inhibit-read-only t)) (erase-buffer)))
      :desc "Open Dirfish (Project Directory)" "e" #'dirvish-side)
      ;; :desc "Open Treemacs" "e" #'treemacs)


;; Dotfiles commands
(map! :leader
      :prefix "l"
      :desc "Open LINK under cursor" "l" #'browse-url-at-point
      :desc "Start backup" "b" (lambda () (interactive) (compile "cd ~/ && make backup_create"))
      :desc "Update all" "u" (lambda () (interactive) (compile "cd ~/ && make update_apps && make update_macos"))
      :desc "Chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply")))
