;;; autoload/text.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/kill-inside-pair ()
  "Kill content inside the surrounding pair — like vim's ci' / ci\" / ci(."
  (interactive)
  (let ((pairs '((?\" . ?\") (?\' . ?\') (?\` . ?\`)
                 (?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}))))
    (let ((start nil)
          (open-char nil)
          (pos (point)))
      ;; Search backward for the nearest opening delimiter
      (save-excursion
        (while (and (not start) (not (bobp)))
          (backward-char)
          (let* ((ch (char-after))
                 (pair (or (assq ch pairs)
                           (rassq ch pairs))))
            (when pair
              (setq start (point)
                    open-char (car pair))))))
      (if (not start)
          (message "No surrounding pair found")
        (let ((close-char (cdr (assq open-char pairs))))
          (goto-char (1+ start))
          (let ((inside (point-marker)))
            (if (search-forward (string close-char) nil t)
                (progn
                  (kill-region inside (1- (point)))
                  (goto-char inside))
              (message "No closing %c found" close-char))))))))

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

;;;###autoload
(defun my/markdown-to-org-buffer ()
  "Convert common Markdown syntax to Org in current buffer."
  (interactive)
  (save-excursion
    (let ((count 0))
      ;; Markdown images: ![alt](../assets/file) → [[file:.attach/file]]
      (goto-char (point-min))
      (while (re-search-forward "!\\[.*?\\](\\.\\.?/assets/\\(.*?\\))" nil t)
        (replace-match "[[file:.attach/\\1]]")
        (setq count (1+ count)))
      ;; Markdown links to assets: [text](../assets/file) → [[file:.attach/file]]
      (goto-char (point-min))
      (while (re-search-forward "\\[.*?\\](\\.\\.?/assets/\\(.*?\\))" nil t)
        (replace-match "[[file:.attach/\\1]]")
        (setq count (1+ count)))
      ;; Markdown links to URLs: [text](http...) → [[url][text]]
      (goto-char (point-min))
      (while (re-search-forward "\\[\\(.*?\\)\\](\\(https?://.*?\\))" nil t)
        (replace-match "[[\\2][\\1]]")
        (setq count (1+ count)))
      ;; Remove standalone [[ ]] wrappers (not org links)
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[\\([^][\n]+?\\)\\]\\]" nil t)
        (unless (string-match-p "^\\(file\\|http\\|https\\|denote\\|attachment\\|today-journal\\):" (match-string 1))
          (replace-match "\\1")
          (setq count (1+ count))))
      ;; Markdown bold **text** → org bold *text*
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\(.*?\\)\\*\\*" nil t)
        (replace-match "*\\1*")
        (setq count (1+ count)))
      ;; Markdown inline code `code` → org verbatim =code=
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`\n]+?\\)`" nil t)
        (replace-match "=\\1=")
        (setq count (1+ count)))
      ;; Markdown headings: # → *, ## → **, etc.
      (goto-char (point-min))
      (while (re-search-forward "^\\(#{1,6}\\) " nil t)
        (let ((stars (make-string (length (match-string 1)) ?*)))
          (replace-match (concat stars " ")))
        (setq count (1+ count)))
      (message "Converted %d Markdown elements to Org" count))))
