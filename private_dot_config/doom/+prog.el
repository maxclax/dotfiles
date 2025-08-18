;;; +prog.el -*- lexical-binding: t; -*-

(use-package! format-all
  :defer t)

(use-package! which-func
  :defer t
  :commands which-function)

(add-hook! 'json-mode-hook (setq-local format-all-formatters '(("JSON" prettier))))


(use-package restclient
  :defer t
  :mode (("\\.http\\'" . restclient-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! '(web-mode-hook html-mode-hook) (setq-local format-all-formatters '(("HTML" prettier))))
(add-hook! 'typescript-mode-hook (setq-local format-all-formatters '(("TypeScript" prettier))))
(add-hook! 'rjsx-mode-hook (setq-local format-all-formatters '(("JavaScript" prettier))))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))
