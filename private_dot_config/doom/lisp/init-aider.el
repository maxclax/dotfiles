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

(defun my/aider-eat ()
  "Start aider in eat terminal on the right side."
  (interactive)
  (let ((default-directory (doom-project-root)))
    (split-window-right)
    (other-window 1)
    (eat "aider" t)))

(defun my/aider-flymake-fix-errors ()
  "Send flymake errors in the current region or buffer to aider for fixing."
  (interactive)
  (require 'aider-core)
  (let* ((start (if (use-region-p) (region-beginning) (point-min)))
         (end   (if (use-region-p) (region-end)       (point-max)))
         (diags (flymake-diagnostics start end))
         (errors (seq-filter (lambda (d)
                               (eq :error (flymake-diagnostic-type d)))
                             diags)))
    (if (null errors)
        (message "No flymake errors in scope.")
      (let ((report (mapconcat
                     (lambda (d)
                       (format "Line %d: %s"
                               (line-number-at-pos (flymake-diagnostic-beg d))
                               (flymake-diagnostic-text d)))
                     errors "\n")))
        (aider--send-command
         (format "Fix these errors in %s:\n%s"
                 (buffer-file-name) report)
         t)))))

(defun my/aider-commit-message ()
  "Send /commit to the running aider session."
  (interactive)
  (require 'aider-core)
  (aider--send-command "/commit" t))

(provide 'init-aider)
