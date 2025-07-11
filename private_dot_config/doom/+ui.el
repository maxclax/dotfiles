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
                    :family "Berkeley Mono Variable"
                    :height 180       ;; 20pt
                    :weight 'normal
                    :width 'normal)

;; ;; Set the big font variant
;; (set-face-attribute 'default nil
;;                     :family "Berkeley Mono Variable"
;;                     :height 360       ;; 36pt
;;                     :weight 'bold)

 ; ;; Set the bold font variant
;; (set-face-attribute 'default nil
;;                     :family "Berkeley Mono Variable"
;;                     :height 200       ;; 20pt
;;                     :weight 'bold
;;                     :width 'normal)

;; (when (display-graphic-p)
;;   (setq user-font
;;         (cond
;;          ((find-font (font-spec :name "Berkeley Mono Variable")) "Berkeley Mono Variable")
;;          ((find-font (font-spec :name "Maple Mono Normal NF")) "Maple Mono Normal NF")))

;;   ;; Some font uses Light font as regular, not sure why. Only use medium weight for this font.
;;   (setq user-font-weight
;;         (cond
;;          ((string= user-font "Berkeley Mono Variable") 'normal)
;;          (t 'normal))
;;         )

;;   ;; calculate the font size based on display-pixel-height
;;   (setq resolution-factor (eval (/ (x-display-pixel-height) 1080.0)))
;;   (setq doom-font (font-spec :family user-font :weight user-font-weight :size (eval (round (* 14 resolution-factor))))
;;         doom-big-font (font-spec :family user-font :weight user-font-weight :size (eval (round (* 18 resolution-factor))))
;;         doom-variable-pitch-font (font-spec :family user-font :weight user-font-weight :size (eval (round (* 13 resolution-factor))))
;;         doom-modeline-height (eval (round (* 24 resolution-factor))))
;;   (setq doom-font-increment 1))


(set-popup-rules! '(("^\\*helpful" :size 0.35)
                    ("^\\*Ibuffer\\*$" :size 0.35)
                    ("^\\*info.*" :size 80 :side right)
                    ("^\\*Man.*" :size 80 :side right)
                    ("^\\*keycast.*" :size 50 :side right)
                    ("^\\*Customize" :actions display-buffer)
                    ("^\\*edit-indirect" :size 0.6)
                    ("^\\*YASnippet Tables\\*$" :size 0.35)
                    ("^\\*grep\\*$" :size 0.35)
                    ("^\\*pytest\\*" :size 0.35)
                    ("^\\*aider.*$" :size 0.35 :side right)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35)
                    ))

(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

