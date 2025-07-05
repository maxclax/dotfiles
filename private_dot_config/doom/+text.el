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
