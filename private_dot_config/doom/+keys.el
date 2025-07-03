;;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

;; ;; Select all in buffer
;; (map! :leader
;;       :desc "Select all in buffer"
;;       "ba" #'(lambda () (interactive) (mark-whole-buffer)))
;;
;; ;; Delete entire buffer without yank
;; (map! :leader
;;       :desc "Delete entire buffer without yank"
;;       "bd" #'(lambda () (interactive) (let ((inhibit-read-only t)) (erase-buffer))))
;;
;; Open Treemacs
(map! :leader
      :desc "Open Treemacs"
      "e" #'treemacs)

