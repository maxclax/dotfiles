;;; autoload/ui.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/fold-toggle ()
  "Toggle fold — uses web-mode native fold in HTML modes, +fold/toggle elsewhere."
  (interactive)
  (if (derived-mode-p 'web-mode 'html-mode 'mhtml-mode)
      (web-mode-fold-or-unfold)
    (+fold/toggle)))

;;;###autoload
(defun my/toggle-display-line-numbers-type ()
  (interactive)
  (if display-line-numbers-type
      (setq display-line-numbers-type nil)
    (setq display-line-numbers-type t))
  (revert-buffer-no-confirm))
