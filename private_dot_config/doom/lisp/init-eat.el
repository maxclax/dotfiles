;;; init-eat.el -*- lexical-binding: t; -*-

(after! eat
  ;; Use a well-known TERM value so shells configure readline correctly.
  ;; eat's default "eat-256color" is unknown to many shells and causes
  ;; erratic input behaviour including character doubling. (Purcell approach)
  (defun my/eat-term-name (&optional display)
    (let ((colors (display-color-cells display)))
      (cond ((> colors 8) "xterm-256color")
            ((> colors 1) "xterm-color")
            (t "xterm"))))
  (setq eat-term-name #'my/eat-term-name)

  ;; Large scrollback
  (setq-default eat-term-scrollback-size (* 2 1024 1024))

  ;; Close buffer on clean process exit (exit code 0)
  (defun my/eat-exit-handler (process)
    (when (zerop (process-exit-status process))
      (kill-buffer)
      (unless (eq (selected-window) (next-window))
        (delete-window))))
  (add-hook 'eat-exit-hook #'my/eat-exit-handler)

  ;; Force emacs state so evil doesn't intercept keypresses
  (add-hook 'eat-mode-hook #'evil-emacs-state))
