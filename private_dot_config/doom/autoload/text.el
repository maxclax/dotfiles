;;; autoload/text.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/insert-current-time ()
  "Insert current time in [H:M] format."
  (interactive)
  (insert (format-time-string "[%H:%M]")))

;;;###autoload
(defun my/insert-current-date ()
  "Insert current date in YYYY-MM-DD format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun my/insert-timestamp ()
  "Insert current timestamp in [YYYY-MM-DD H:M] format."
  (interactive)
  (insert (format-time-string "[%Y-%m-%d %H:%M]")))

;;;###autoload
(defun my/journal-log-finish ()
  "Add finish time and calculate duration for current journal entry."
  (interactive)
  (save-excursion
    ;; Find the current journal entry (heading with time format like [10:49])
    (when (re-search-backward "^\\*\\* \\[\\([0-9]\\{2\\}:[0-9]\\{2\\}\\)\\]" nil t)
      (let* ((start-time (match-string 1))
             (current-time (format-time-string "%H:%M"))
             (start-minutes (+ (* (string-to-number (substring start-time 0 2)) 60)
                              (string-to-number (substring start-time 3 5))))
             (current-minutes (+ (* (string-to-number (substring current-time 0 2)) 60)
                                (string-to-number (substring current-time 3 5))))
             (duration-minutes (- current-minutes start-minutes))
             (duration-text (if (>= duration-minutes 60)
                               (format "≈%dh %dmin"
                                      (/ duration-minutes 60)
                                      (% duration-minutes 60))
                             (format "≈%d min" duration-minutes))))
        ;; Go to end of this journal entry
        (forward-line 1)
        (while (and (not (eobp))
                   (not (looking-at "^\\*\\*"))
                   (not (looking-at "^\\*[^*]")))
          (forward-line 1))
        (backward-char 1)
        (end-of-line)
        ;; Add finish line
        (insert (format "\n- Finished at [%s] (%s)" current-time duration-text))))))

;;;###autoload
(defun my/journal-log-start ()
  "Insert a new journal entry with current time."
  (interactive)
  (insert (format "** [%s] " (format-time-string "%H:%M"))))
