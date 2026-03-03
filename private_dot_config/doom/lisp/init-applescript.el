;;; init-applescript.el -*- lexical-binding: t; -*-

(use-package! applescript-mode
  :mode "\\.applescript\\'"
  :hook (applescript-mode . font-lock-mode))
