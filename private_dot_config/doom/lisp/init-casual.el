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
     (t                                 (casual-editkit-main-tmenu))))

  ;; Single global binding — context-aware
  (keymap-global-set "C-o" #'my/casual-open)

  ;; Isearch needs its own map (active during search, not a major mode)
  (keymap-set isearch-mode-map "C-o" #'casual-isearch-tmenu)

  ;; calc-alg-map only exists after calc loads
  (with-eval-after-load 'calc
    (keymap-set calc-alg-map "C-o" #'casual-calc-tmenu)))

(provide 'init-casual)
