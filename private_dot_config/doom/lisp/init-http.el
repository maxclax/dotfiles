;;; lisp/init-http.el -*- lexical-binding: t; -*-

;; restclient — send HTTP requests from .http files
(use-package! restclient
  :mode ("\\.http\\'" . restclient-mode))

;; verb — HTTP client integrated with org-mode
(use-package! verb
  :after org)

(provide 'init-http)
