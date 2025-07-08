;;; +text.el -*- lexical-binding: t; -*-

(use-package! pomm
  :defer t
  :commands (pomm pomm-third-time)
  :config
  (setq pomm-work-period 40
        pomm-long-break-period 25
        pomm-short-break-period 5
        alert-default-style (if IS-MAC 'osx-notifier 'libnotify)
        pomm-audio-enabled t)
  (pomm-mode-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (custom-set-faces
;;  '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
;;  '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
;;  '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
;;  '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
;;  '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
;;  '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
;;  '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))
