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
                  (delete-region inside (1- (point)))
                  (goto-char inside))
              (message "No closing %c found" close-char))))))))

;;;###autoload
(defun my/copy-inside-pair ()
  "Copy content inside the surrounding pair into the kill ring — like vim's yi' / yi\" / yi(."
  (interactive)
  (let ((pairs '((?\" . ?\") (?\' . ?\') (?\` . ?\`)
                 (?\( . ?\)) (?\[ . ?\]) (?\{ . ?\}))))
    (let ((start nil)
          (open-char nil))
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
          (save-excursion
            (goto-char (1+ start))
            (let ((inside (point)))
              (if (search-forward (string close-char) nil t)
                  (progn
                    (kill-ring-save inside (1- (point)))
                    (message "Copied inside %c...%c" open-char close-char))
                (message "No closing %c found" close-char)))))))))

;;;###autoload
(defun my/copy-line ()
  "Copy the current line to the kill ring."
  (interactive)
  (kill-ring-save (line-beginning-position) (line-end-position))
  (message "Line copied"))

;;;###autoload
(defun my/copy-to-end-of-line ()
  "Copy text from point to the end of the line into the kill ring."
  (interactive)
  (kill-ring-save (point) (line-end-position))
  (message "Copied to end of line"))

;;;###autoload
(defun my/copy-value (&optional whole)
  "Copy the value after the first `key SEP value' separator on the current line.
SEP is a colon (`Key: value', `:PROP: value'), or a space-padded -, =, or |
\(`Key - value'). Requiring spaces around -/=/| means hyphenated words like
`triada-d1-set-week' are never split (and em-dashes in prose are left alone).
With no separator — or with prefix arg WHOLE — the whole trimmed line is
copied. Works from anywhere on the line."
  (interactive "P")
  (let* ((line (string-trim
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position))))
         (val (cond
               (whole line)
               ;; colon: tight is fine (key:value, :PROP: value)
               ((string-match "\\`:?[^:\n]+:[ \t]*\\(.*\\)\\'" line)
                (match-string 1 line))
               ;; -, =, | : only when space-padded (so words aren't split)
               ((string-match "\\`.+?[ \t]+[-=|][ \t]+\\(.*\\)\\'" line)
                (match-string 1 line))
               (t line))))
    (setq val (string-trim val))
    (if (string-empty-p val)
        (user-error "Nothing to copy on this line")
      (kill-new val)
      (message "Copied: %s" val))))

;;;###autoload
(defun my/denote-copy-title ()
  "Copy the current note's title to the kill ring.
Uses the `#+title:' value (without the keyword), falling back to the
Denote title derived from the file name."
  (interactive)
  (let ((title (or (and (derived-mode-p 'org-mode)
                        (cadr (assoc "TITLE" (org-collect-keywords '("TITLE")))))
                   (and (buffer-file-name)
                        (fboundp 'denote-retrieve-title-or-filename)
                        (denote-retrieve-title-or-filename
                         (buffer-file-name)
                         (denote-filetype-heuristics (buffer-file-name)))))))
    (if (and title (not (string-empty-p (string-trim title))))
        (progn (kill-new title)
               (message "Copied title: %s" title))
      (user-error "No title found in this buffer"))))

;;;###autoload
(defun my/org-copy-block ()
  "Copy the contents of the Org block at point to the kill ring.
Works from anywhere inside a #+BEGIN_…/#+END_… block — SRC, EXAMPLE, EXPORT,
QUOTE, VERSE, CENTER, COMMENT, special, or dynamic blocks — copying just the
body, without the #+BEGIN/#+END lines."
  (interactive)
  (require 'org-element)
  (let ((el (org-element-lineage
             (org-element-context)
             '(src-block example-block export-block comment-block
               quote-block verse-block special-block center-block dynamic-block)
             t)))
    (unless el (user-error "Point is not inside an Org block"))
    (let* ((raw (or (org-element-property :value el) ; src/example/export/comment
                    (let ((b (org-element-property :contents-begin el))
                          (e (org-element-property :contents-end el)))
                      (when (and b e) (buffer-substring-no-properties b e)))))
           (text (and raw (string-trim-right raw))))
      (if (and text (not (string-empty-p (string-trim text))))
          (progn (kill-new text)
                 (message "Copied block contents (%d chars)" (length text)))
        (user-error "Block is empty")))))

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
