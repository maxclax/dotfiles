;;; init-aider.el -*- mode: emacs-lisp; lexical-binding: t; -*-

(use-package! aider
  :commands (aider-run-aider
             aider-function-or-region-change
             aider-add-current-file-or-dired-marked-files
             aider-ask-question
             aider-write-unit-test
             aider-switch-to-buffer)
  :config
  (setq aider-args '()))

(defun my/aider-project ()
  "Start aider session from the current project root."
  (interactive)
  (let ((default-directory (doom-project-root)))
    (aider-run-aider)))

(defun my/aider-commit-message ()
  "Send /commit to the running aider session."
  (interactive)
  (require 'aider-core)
  (aider--send-command "/commit" t))

(provide 'init-aider)
