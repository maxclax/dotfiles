;;; autoload/frames.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/make-2-frames ()
  "Create 2 new frames and switch aerospace to workspace 4."
  (interactive)
  (make-frame-command)
  (make-frame-command)
  (run-with-timer 0.3 nil (lambda ()
    (call-process "aerospace" nil nil nil "workspace" "4"))))

;;;###autoload
(defun my/make-3-frames ()
  "Create 3 new frames and switch aerospace to workspace 4."
  (interactive)
  (make-frame-command)
  (make-frame-command)
  (make-frame-command)
  (run-with-timer 0.3 nil (lambda ()
    (call-process "aerospace" nil nil nil "workspace" "4"))))
