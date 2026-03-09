;;; autoload/frames.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/make-2-frames ()
  "Create 2 extra frames: frame1 → ws5, frame2 → ws6, return to ws4."
  (interactive)
  (make-frame-command)
  (make-frame-command)
  (run-with-timer 0.6 nil
    (lambda ()
      (start-process-shell-command "aero-arrange" nil
        "aerospace move-node-to-workspace 5 && \
         sleep 0.1 && \
         aerospace move-node-to-workspace 6 && \
         aerospace workspace 4"))))

;;;###autoload
(defun my/make-3-frames ()
  "Create 3 extra frames: frame1 → ws5, frame2 → ws6, frame3 → ws1, return to ws4."
  (interactive)
  (make-frame-command)
  (make-frame-command)
  (make-frame-command)
  (run-with-timer 0.6 nil
    (lambda ()
      (start-process-shell-command "aero-arrange" nil
        "aerospace move-node-to-workspace 5 && \
         sleep 0.1 && \
         aerospace move-node-to-workspace 6 && \
         sleep 0.1 && \
         aerospace move-node-to-workspace 1 && \
         aerospace workspace 4"))))
