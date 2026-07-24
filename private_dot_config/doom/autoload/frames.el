;;; autoload/frames.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/close-other-frames ()
  "Close every frame except this one and park it on ws5."
  (interactive)
  (delete-other-frames)
  (start-process-shell-command "aero-arrange" nil
    "aerospace move-node-to-workspace 5 && \
     aerospace workspace 5"))

;;;###autoload
(defun my/make-1-frame ()
  "Create 1 extra frame: frame1 → ws4, return to ws5."
  (interactive)
  (make-frame-command)
  (run-with-timer 0.6 nil
    (lambda ()
      (start-process-shell-command "aero-arrange" nil
        "aerospace move-node-to-workspace 4 && \
         aerospace workspace 5"))))

;;;###autoload
(defun my/make-2-frames ()
  "Create 2 extra frames: frame1 → ws4, frame2 → ws6, return to ws5."
  (interactive)
  (make-frame-command)
  (make-frame-command)
  (run-with-timer 0.6 nil
    (lambda ()
      (start-process-shell-command "aero-arrange" nil
        "aerospace move-node-to-workspace 4 && \
         sleep 0.1 && \
         aerospace move-node-to-workspace 6 && \
         aerospace workspace 5"))))

;;;###autoload
(defun my/make-3-frames ()
  "Create 3 extra frames: frame1 → ws4, frame2 → ws6, frame3 → ws D, return to ws5."
  (interactive)
  (make-frame-command)
  (make-frame-command)
  (make-frame-command)
  (run-with-timer 0.6 nil
    (lambda ()
      (start-process-shell-command "aero-arrange" nil
        "aerospace move-node-to-workspace 4 && \
         sleep 0.1 && \
         aerospace move-node-to-workspace 6 && \
         sleep 0.1 && \
         aerospace move-node-to-workspace D && \
         aerospace workspace 5"))))
