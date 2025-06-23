;;; +ui.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Berkeley Mono Variable"
                          :size 20
                          :weight 'normal
                          :width 'normal))

;; Set the big font variant
(setq doom-big-font (font-spec :family "Berkeley Mono Variable"
                               :size 36
                               :weight 'bold))

;; Set the bold font variant
(setq doom-font-bold (font-spec :family "Berkeley Mono Variable"
                               :size 20
                               :weight 'bold
                               :width 'normal))


(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
(setq doom-modeline-workspace-name t)

(setq fancy-splash-image (concat doom-user-dir "assets/GNUEmacs.png"))
