;;; lisp/init-timing.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;; ── TMR Timer (Vitamin-R style) ─────────────────────────────────────
;; Flexible time-boxing with sound notifications

(use-package! tmr
  :config
  ;; Completion sound when timer finishes
  (setq tmr-sound-file "/System/Library/Sounds/Glass.aiff")

  ;; Use macOS afplay instead of ffplay
  (setq tmr-sound-command "afplay")

  ;; Keep history of timer descriptions
  (setq tmr-description-list 'tmr-description-history)

  ;; Common focus durations (Vitamin-R style)
  (setq tmr-description-history
        '("Deep Work" "Quick Task" "Review" "Break" "Meeting")))

;; ── Ticking sound during focus (Vitamin-R clock sounds) ─────────────
(defvar my/tick-process nil "Process playing the ticking loop.")

(defvar my/tick-sounds
  (directory-files (expand-file-name "assets" doom-private-dir) t "clocktick.*\\.m4a$")
  "Available clock tick sound files.")

(defvar my/tick-current-sound nil "Currently selected tick sound.")

(defvar my/tick-volume "0.3"
  "Volume for ticking sound (0.0 to 1.0).")

(defun my/tick-start (&optional sound)
  "Start looping clock tick sound. With prefix arg, choose sound."
  (interactive
   (list (when current-prefix-arg
           (completing-read "Tick sound: "
                            (mapcar #'file-name-nondirectory my/tick-sounds)))))
  (my/tick-stop)
  (let ((file (if sound
                  (seq-find (lambda (f) (string-match-p sound f)) my/tick-sounds)
                (or my/tick-current-sound (car my/tick-sounds)))))
    (when (and file (file-exists-p file))
      (setq my/tick-current-sound file)
      (setq my/tick-process
            (start-process "tick-loop" nil
                           "bash" "-c"
                           (format "while true; do afplay -v %s '%s'; done"
                                   my/tick-volume file))))
    (message "Ticking: %s" (file-name-nondirectory (or file "none")))))

(defun my/tick-stop ()
  "Stop ticking sound."
  (interactive)
  (when (and my/tick-process (process-live-p my/tick-process))
    (kill-process my/tick-process)
    (start-process "kill-afplay" nil "pkill" "-f" "afplay.*clocktick"))
  (setq my/tick-process nil)
  (message "Ticking stopped"))

(defun my/tick-toggle ()
  "Toggle ticking sound."
  (interactive)
  (if (and my/tick-process (process-live-p my/tick-process))
      (my/tick-stop)
    (my/tick-start)))

;; Auto-stop ticking when any tmr timer finishes
(add-hook 'tmr-timer-finished-functions
          (lambda (_timer) (my/tick-stop)))

(provide 'init-timing)
