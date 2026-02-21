;;; lisp/init-http.el -*- lexical-binding: t; -*-

;; restclient — send HTTP requests from .http files
(use-package! restclient
  :mode ("\\.http\\'" . restclient-mode))

;; verb — HTTP client integrated with org-mode
(use-package! verb
  :after org
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'init-http)
