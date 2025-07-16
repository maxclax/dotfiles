;;; +ui.el -*- lexical-binding: t; -*-


(setq doom-theme 'doom-one)
(setq fancy-splash-image (concat doom-private-dir "assets/GNUEmacs.png"))

;; Enable rainbow-mode for CSS, HTML, and other files with color codes
(add-hook 'css-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)
(add-hook 'web-mode-hook #'rainbow-mode)  ; If using web-mode
(add-hook 'prog-mode-hook #'rainbow-mode) ; For all programming modes (optional)

(beacon-mode 1)

;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)
;; Set the default font
(set-face-attribute 'default nil
                    :font "Berkeley Mono Variable-18"
                    :weight 'normal
                    :width 'normal)


(set-popup-rules! '(("^\\*helpful" :size 0.35 :modeline nil)
                    ("^\\*Ibuffer\\*$" :size 0.35 :modeline nil)
                    ("^\\*info.*" :size 80 :side right :modeline nil)
                    ("^\\*Man.*" :size 80 :side right :modeline nil)
                    ("^\\*keycast.*" :size 50 :side right :modeline nil)
                    ("^\\*Customize" :actions display-buffer :modeline nil)
                    ("^\\*edit-indirect" :size 0.6 :modeline nil)
                    ("^\\*YASnippet Tables\\*$" :size 0.35 :modeline nil)
                    ("^\\*grep\\*$" :size 0.35 :modeline nil)
                    ("^\\*pytest\\*" :size 0.35 :modeline nil)
                    ("^\\*aider.*$" :size 0.35 :side right :modeline nil)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil :modeline nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35 :modeline nil)
                    ))

(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name
