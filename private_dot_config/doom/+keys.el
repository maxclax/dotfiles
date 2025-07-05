;;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

(map! :leader
      :desc "Select all in buffer" "ba" #'(lambda () (interactive) (mark-whole-buffer))
      ;; :desc "Delete entire buffer without yank" "bd" #'(lambda () (interactive) (let ((inhibit-read-only t)) (erase-buffer)))
      :desc "Open Dirfish (Project Directory)" "e" #'dirvish-side)
      ;; :desc "Open Treemacs" "e" #'treemacs)


;; Project navigation
(map! :prefix "M-p"
      :desc "Chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply")))
