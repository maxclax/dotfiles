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

(defun my/tmr-focus-emacs ()
  "Raise and focus Emacs when a timer finishes."
  (raise-frame)
  (when (fboundp 'x-focus-frame)
    (x-focus-frame (selected-frame)))
  (when IS-MAC
    (start-process "focus-emacs" nil "osascript" "-e"
                   "tell application \"Emacs\" to activate")))

;; Schedule countdown when timer starts, cancel+stop ticking when finished
(add-hook 'tmr-timer-created-functions #'my/tmr-schedule-countdown)
(add-hook 'tmr-timer-finished-functions
          (lambda (_timer) (my/tmr-cancel-countdown) (my/tick-stop) (my/tmr-focus-emacs)))
(add-hook 'tmr-timer-cancelled-functions
          (lambda (_timer) (my/tmr-cancel-countdown) (my/tick-stop)))

;; ── Timer persistence across restarts ───────────────────────────────
(defvar my/tmr-save-file (expand-file-name "tmr-timers.el" doom-cache-dir)
  "File to persist active TMR timers.")

(defun my/tmr-save-timers ()
  "Save active (non-finished) timers to disk."
  (let ((active (seq-remove #'tmr--timer-finishedp tmr--timers)))
    (when active
      (with-temp-file my/tmr-save-file
        (insert ";; TMR saved timers\n")
        (dolist (timer active)
          (let ((end (float-time (tmr--timer-end-date timer)))
                (desc (tmr--timer-description timer))
                (ack (tmr--timer-acknowledgep timer))
                (paused (tmr--timer-paused-remaining timer)))
            (prin1 (list :end end
                         :description desc
                         :acknowledgep ack
                         :paused (when paused (float-time paused)))
                   (current-buffer))
            (insert "\n")))))
    (unless active
      (when (file-exists-p my/tmr-save-file)
        (delete-file my/tmr-save-file)))))

(defun my/tmr-restore-timers ()
  "Restore saved timers from disk, recreating those still in the future."
  (when (file-exists-p my/tmr-save-file)
    (let ((restored 0))
      (with-temp-buffer
        (insert-file-contents my/tmr-save-file)
        (goto-char (point-min))
        ;; Catch end-of-file (normal loop exit) AND any other errors so
        ;; delete-file always runs even if a timer entry is malformed.
        (condition-case err
            (while t
              (let* ((data (read (current-buffer)))
                     (end-time (plist-get data :end))
                     (desc (plist-get data :description))
                     (ack (plist-get data :acknowledgep))
                     (paused-secs (plist-get data :paused))
                     (remaining (- end-time (float-time))))
                (cond
                 ;; Paused timer — restore as paused
                 (paused-secs
                  (let ((timer (tmr--timer-create
                                :description desc
                                :acknowledgep ack
                                :creation-date (current-time)
                                :end-date (time-add (current-time) paused-secs)
                                :input (format "%ss" (round paused-secs)))))
                    (setf (tmr--timer-timer-object timer)
                          (run-with-timer paused-secs nil #'tmr--complete timer))
                    (setf (tmr--timer-paused-remaining timer) paused-secs)
                    (cancel-timer (tmr--timer-timer-object timer))
                    (push timer tmr--timers)
                    (cl-incf restored)))
                 ;; Still has time left — restore as active
                 ((> remaining 1)
                  (let ((timer (tmr--timer-create
                                :description desc
                                :acknowledgep ack
                                :creation-date (current-time)
                                :end-date (seconds-to-time end-time)
                                :input (format "%ss" (round remaining)))))
                    (setf (tmr--timer-timer-object timer)
                          (run-with-timer remaining nil #'tmr--complete timer))
                    (push timer tmr--timers)
                    ;; Call only our own hooks directly — avoids fragile TMR
                    ;; internal hooks (e.g. mode-line) that expect a fully
                    ;; TMR-created struct and can throw errors that prevent
                    ;; delete-file from running on subsequent restarts.
                    (my/tmr-schedule-countdown timer)
                    (my/tick-start)
                    (cl-incf restored))))))
          (end-of-file nil)
          (error (message "TMR restore error: %s" (error-message-string err)))))
      (run-hooks 'tmr--update-hook)
      (when (> restored 0)
        (message "TMR: restored %d timer%s" restored (if (= restored 1) "" "s")))
      (delete-file my/tmr-save-file))))

(add-hook 'kill-emacs-hook #'my/tmr-save-timers)
(add-hook 'doom-first-buffer-hook #'my/tmr-restore-timers)

(provide 'init-timing)
