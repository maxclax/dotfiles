;;; lisp/init-timing.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;; ── TMR Timer  ─────────────────────────────────────
;; Flexible time-boxing with sound notifications

(use-package! tmr
  :init
  ;; Suppress DBUS warning on macOS — we override notifications with osascript
  (add-to-list 'warning-suppress-types '(tmr))

  :config

  ;; Completion sound
  (setq tmr-sound-file "/System/Library/Sounds/Glass.aiff")

  ;; Keep history of timer descriptions
  (setq tmr-description-list 'tmr-description-history)

  ;; Common focus durations
  (setq tmr-description-history
        '("Deep Work" "Quick Task" "Review" "Break" "Meeting" "Administration"))

  ;; Auto-start ticking when a timer is created
  (add-hook 'tmr-timer-created-functions
            (lambda (_timer) (my/tick-start)))

  ;; Show remaining time in mode line
  (tmr-mode-line-mode 1))

;; ── Ticking sound during focus ─────────────
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

;; ── Countdown alerts before timer ends ──────────────────────────────
(defvar my/tmr-countdown-timers nil "Internal timers for countdown alerts.")

(defun my/tmr-play-sound (sound &optional volume)
  "Play macOS system SOUND at VOLUME (default 0.5)."
  (start-process "tmr-alert" nil "afplay" "-v" (or volume "0.5")
                 (format "/System/Library/Sounds/%s.aiff" sound)))

(defun my/tmr-schedule-countdown (timer)
  "Schedule countdown alerts for TIMER: 2min, 1min, and 10s-0s."
  (my/tmr-cancel-countdown)
  (let* ((end-date (tmr--timer-end-date timer))
         (remaining (float-time (time-subtract end-date (current-time)))))
    ;; 2 minutes warning
    (when (> remaining 120)
      (push (run-at-time (- remaining 120) nil
                         (lambda () (my/tmr-play-sound "Submarine" "0.4")
                           (message "TMR: 2 minutes remaining")))
            my/tmr-countdown-timers))
    ;; 1 minute warning
    (when (> remaining 60)
      (push (run-at-time (- remaining 60) nil
                         (lambda () (my/tmr-play-sound "Blow" "0.5")
                           (message "TMR: 1 minute remaining")))
            my/tmr-countdown-timers))
    ;; 10s countdown
    (dotimes (i 11)
      (let ((secs-left (- 10 i)))
        (when (> remaining secs-left)
          (push (run-at-time (- remaining secs-left) nil
                             (lambda (s)
                               (my/tmr-play-sound "Tink" "0.6")
                               (message "TMR: %ds" s))
                             secs-left)
                my/tmr-countdown-timers))))))

(defun my/tmr-cancel-countdown ()
  "Cancel all scheduled countdown alerts."
  (dolist (timer my/tmr-countdown-timers)
    (when (timerp timer) (cancel-timer timer)))
  (setq my/tmr-countdown-timers nil))

(defun my/tmr-notify (timer)
  "Show macOS notification and raise Emacs when TIMER finishes."
  (let ((desc (or (tmr--timer-description timer) "Timer")))
    ;; macOS notification banner
    (start-process "tmr-notify" nil "osascript" "-e"
                   (format "display notification \"%s finished\" with title \"TMR\" sound name \"Glass\""
                           desc))
    ;; Raise Emacs window
    (raise-frame)
    (when IS-MAC
      (start-process "focus-emacs" nil "osascript" "-e"
                     "tell application \"Emacs\" to activate"))))

;; Schedule countdown when timer starts, cancel+stop ticking when finished
(add-hook 'tmr-timer-created-functions #'my/tmr-schedule-countdown)
(add-hook 'tmr-timer-finished-functions
          (lambda (timer) (my/tmr-cancel-countdown) (my/tick-stop) (my/tmr-notify timer)))
(add-hook 'tmr-timer-cancelled-functions
          (lambda (_timer) (my/tmr-cancel-countdown) (my/tick-stop)))

(provide 'init-timing)
