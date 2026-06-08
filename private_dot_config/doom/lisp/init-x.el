;;; init-x.el -*- lexical-binding: t; -*-
;;
;; X (Twitter) from Org, in one place. There is no maintained Emacs package for
;; the X API v2, so this drives the `x' CLI (shell/bin/x). Three things:
;;
;;   my/x-post        publish the current Org subtree/region (--- splits a thread)
;;   my/x-followers   diff followers vs. a stored Denote note (new / unfollowed)
;;   my/x-trends      pull trends as an Org list
;;
;; All behind the `my/x' transient menu (bound to C-c X in +keys.el).
;; Token lives in the CLI (X_BEARER_TOKEN / X_BEARER_OP_REF → 1Password).

(defgroup my/x nil "X (Twitter) integration." :group 'my)

(defcustom my/x-cli "~/.config/shell/bin/x"
  "Path to the `x' CLI that talks to the X API."
  :type 'string :group 'my/x)

(defcustom my/x-keywords '("x" "followers")
  "Category Denote keywords marking follower-tracking notes.
The tracked username is also stored as its own filename keyword and in the
note's `:X_USER:' property (authoritative)."
  :type '(repeat string) :group 'my/x)

(defcustom my/x-trends-woeid 23424977
  "Default WOEID for `my/x-trends'. 23424977 = USA (matches the US-audience
strategy). Others: 1 worldwide, 23424750 Austria, 23424975 UK, 23424976 Ukraine.
Use a prefix arg (C-u) to pick a different one per call."
  :type 'integer :group 'my/x)

(defcustom my/x-thread-separator "^-\\{3,\\}[ \t]*$"
  "Regexp for a line separating tweets within a thread subtree."
  :type 'regexp :group 'my/x)

;; ── CLI runner ───────────────────────────────────────────────────────────

(defun my/x--run (&rest args)
  "Run the x CLI with ARGS; return trimmed stdout, or signal on failure."
  (with-temp-buffer
    (let* ((exit (apply #'call-process my/x-cli nil t nil args))
           (out (string-trim (buffer-string))))
      (if (and (integerp exit) (zerop exit)) out
        (user-error "x %s: %s" (or (car args) "") out)))))

(defun my/x--run-input (input &rest args)
  "Run the x CLI with ARGS, feeding INPUT on stdin; return trimmed stdout."
  (with-temp-buffer
    (insert input)
    (let* ((exit (apply #'call-process-region (point-min) (point-max)
                        my/x-cli nil t nil args))
           (out (string-trim (buffer-string))))
      (if (and (integerp exit) (zerop exit)) out
        (user-error "x %s: %s" (or (car args) "") out)))))

;; ── Post ─────────────────────────────────────────────────────────────────

(defun my/x--clean-text (s)
  "Strip Org markup from S to plain text suitable for X."
  (let ((s (or s "")))
    (setq s (replace-regexp-in-string
             "\\[\\[\\([^][]+\\)\\]\\[\\([^][]+\\)\\]\\]" "\\2 \\1" s))
    (setq s (replace-regexp-in-string "\\[\\[\\([^][]+\\)\\]\\]" "\\1" s))
    (setq s (replace-regexp-in-string
             "\\([*/=~_]\\)\\([^ \t\n*/=~_][^*/=~_]*?\\)\\1" "\\2" s))
    (setq s (replace-regexp-in-string "^[ \t]*#\\+.*$" "" s))
    (setq s (replace-regexp-in-string "^[ \t]*# .*$" "" s))
    (setq s (replace-regexp-in-string "\n\\{3,\\}" "\n\n" s))
    (string-trim s)))

(defun my/x--body ()
  "Return the active region, else the current Org subtree body."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (save-excursion
      (org-back-to-heading t)
      (let ((beg (progn (forward-line 1)
                        (while (looking-at-p "^[ \t]*\\(:.*:\\|SCHEDULED\\|DEADLINE\\|CLOSED\\)")
                          (forward-line 1))
                        (point)))
            (end (progn (org-end-of-subtree t t) (point))))
        (buffer-substring-no-properties beg end)))))

(defun my/x--segments (body)
  "Split BODY into cleaned tweet strings on `my/x-thread-separator'."
  (let ((parts (split-string body (concat "\n" my/x-thread-separator "\n") t)))
    (when (= (length parts) 1)
      (setq parts (split-string body (format "\n*%s\n*" my/x-thread-separator) t)))
    (delq nil (mapcar (lambda (p)
                        (let ((c (my/x--clean-text p)))
                          (and (org-string-nw-p c) c)))
                      parts))))

;;;###autoload
(defun my/x-post ()
  "Publish the current Org subtree (or region) to X.
One block posts a single tweet; blocks split by a line of --- post as a thread.
Previews each tweet and asks before sending."
  (interactive)
  (let ((segments (my/x--segments (my/x--body))))
    (unless segments (user-error "Nothing to post"))
    (with-output-to-temp-buffer "*X post preview*"
      (let ((i 0) (n (length segments)))
        (dolist (s segments)
          (setq i (1+ i))
          (princ (format "── %d/%d (%d chars)%s\n%s\n\n"
                         i n (length s) (if (> (length s) 280) "  ⚠ >280" "") s)))))
    (when (yes-or-no-p (format "Post %d tweet(s) to X? " (length segments)))
      (let (reply first)
        (dolist (s segments)
          (let ((id (if reply
                        (my/x--run-input s "post" "--reply-to" reply)
                      (my/x--run-input s "post"))))
            (setq reply id)
            (unless first (setq first id))))
        (let ((url (format "https://x.com/i/status/%s" first)))
          (kill-new url)
          (message "Posted. %s (URL copied)" url))))))

;; ── Followers (diff against a Denote note) ───────────────────────────────

(defun my/x--normalize (handle)
  "Return HANDLE without a leading @ and lower-cased, or nil if empty."
  (let ((h (replace-regexp-in-string "\\`@+" "" (string-trim (or handle "")))))
    (and (org-string-nw-p h) (downcase h))))

(defun my/x--fmt-count (n)
  "Human-readable follower count N."
  (cond ((>= n 1000000) (format "%.1fM" (/ n 1e6)))
        ((>= n 1000)    (format "%.1fk" (/ n 1000.0)))
        (t              (number-to-string n))))

(defun my/x--vtype (raw)
  "Normalize X verified_type RAW to a short label (\"\" for none)."
  (pcase raw
    ("blue" "premium") ("business" "business") ("government" "gov") (_ "")))

(defun my/x--annot-line (pl)
  "One-line annotation from a follower PL plist (for changelog/report)."
  (let ((type (plist-get pl :type)) (n (or (plist-get pl :followers) 0)))
    (concat (if (org-string-nw-p type) (format "  ✔%s" type) "")
            (format "  · %sf" (my/x--fmt-count n)))))

(defun my/x--fetch-followers (username)
  "Return a cons (HANDLES . INFO) for USERNAME.
HANDLES is a deduped list of handles (no @); INFO is a hash handle→plist
with :type :followers :following."
  (let ((handles '())
        (info (make-hash-table :test 'equal))
        (seen (make-hash-table :test 'equal)))
    (dolist (line (split-string (my/x--run "followers" username) "\n" t))
      (let* ((cols (split-string line "\t"))
             (h (my/x--normalize (nth 0 cols))))
        (when (and h (not (gethash h seen)))
          (puthash h t seen)
          (push h handles)
          (puthash h (list :type (my/x--vtype (nth 1 cols))
                           :followers (string-to-number (or (nth 2 cols) "0"))
                           :following (string-to-number (or (nth 3 cols) "0")))
                   info))))
    (unless handles (user-error "No followers returned for @%s" username))
    (cons (nreverse handles) info)))

(defun my/x--note-title (username) (format "X Followers %s" username))

(defun my/x--note-user (file)
  "Return the username in FILE's `:X_USER:' property, or nil."
  (with-temp-buffer
    (insert-file-contents file nil 0 4000)
    (goto-char (point-min))
    (when (re-search-forward "^:X_USER:[ \t]+\\(.+?\\)[ \t]*$" nil t)
      (match-string 1))))

(defun my/x--category-files ()
  "Follower-tracking notes (files carrying all `my/x-keywords')."
  (when (fboundp 'denote-directory-files)
    (seq-filter (lambda (f)
                  (let ((kw (ignore-errors (denote-extract-keywords-from-path f))))
                    (seq-every-p (lambda (k) (member k kw)) my/x-keywords)))
                (denote-directory-files))))

(defun my/x--all-users ()
  "Alist of (USERNAME . FILE) for every follower note."
  (delq nil (mapcar (lambda (f) (let ((u (my/x--note-user f))) (and u (cons u f))))
                    (my/x--category-files))))

(defun my/x--find-note (username)
  (cdr (assoc-string username (my/x--all-users) t)))

(defun my/x--create-note (username)
  (unless (fboundp 'denote) (user-error "Denote is not available"))
  (let* ((denote-prompts nil)
         (user-kw (if (fboundp 'denote-sluggify-keyword)
                      (denote-sluggify-keyword username) (downcase username)))
         (keywords (append my/x-keywords
                           (and (org-string-nw-p user-kw) (list user-kw)))))
    (denote (my/x--note-title username) keywords)
    (goto-char (point-max))
    (insert "\n* Followers\n:PROPERTIES:\n:X_USER: " username
            "\n:COUNT: 0\n:END:\n\n* Changelog\n\n"
            "* Set\n#+begin_src text :name followers\n#+end_src\n")
    (save-buffer)
    (buffer-file-name)))

(defun my/x--note-file (username)
  (or (my/x--find-note username) (my/x--create-note username)))

(defun my/x--read-set (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+begin_src text :name followers[ \t]*$" nil t)
      (forward-line 1)
      (let ((start (point)))
        (when (re-search-forward "^#\\+end_src[ \t]*$" nil t)
          (delq nil (mapcar #'my/x--normalize
                            (split-string (buffer-substring-no-properties
                                           start (line-beginning-position))
                                          "\n"))))))))

(defun my/x--parse-note (file)
  "Parse FILE; return a plist (:frontmatter :xuser :since :changelog).
SINCE is a hash handle→first-seen-date read from the existing table, so dates
survive across runs. Works on the old layout too (for one-time migration)."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((since (make-hash-table :test 'equal)) fm xuser changelog)
      (goto-char (point-min))
      (setq fm (buffer-substring-no-properties
                (point-min)
                (if (re-search-forward "^\\* " nil t) (match-beginning 0) (point-max))))
      (goto-char (point-min))
      (when (re-search-forward "^:X_USER:[ \t]+\\(.+?\\)[ \t]*$" nil t)
        (setq xuser (match-string 1)))
      ;; table rows: | [[..][@handle]] | type | nf | ng | YYYY-MM-DD |
      (goto-char (point-min))
      (while (re-search-forward
              "^|[^|]*@\\([A-Za-z0-9_]+\\)[^|]*|[^|]*|[^|]*|[^|]*| *\\([0-9]\\{4\\}-[0-9-]+\\) *|" nil t)
        (puthash (downcase (match-string 1)) (match-string 2) since))
      (goto-char (point-min))
      (when (re-search-forward "^\\* Changelog[ \t]*$" nil t)
        (forward-line 1)
        (setq changelog (string-trim
                         (buffer-substring-no-properties
                          (point)
                          (if (re-search-forward "^\\* " nil t)
                              (match-beginning 0) (point-max))))))
      (list :frontmatter (string-trim-right fm) :xuser xuser
            :since since :changelog (or changelog "")))))

(defun my/x--write-note (file handles new gone info)
  "Reconstruct FILE: sortable table (top) + changelog + diff src block (bottom).
HANDLES is the current set; NEW/GONE the diff; INFO a handle→plist map."
  (let* ((p (my/x--parse-note file))
         (fm (plist-get p :frontmatter))
         (xuser (or (plist-get p :xuser) "?"))
         (since (plist-get p :since))
         (old-cl (plist-get p :changelog))
         (today (format-time-string "%Y-%m-%d"))
         (rows (mapcar (lambda (h)
                         (let ((pl (gethash h info)))
                           (list h
                                 (or (plist-get pl :type) "")
                                 (or (plist-get pl :followers) 0)
                                 (or (plist-get pl :following) 0)
                                 (or (gethash h since) today))))
                       handles))
         (rows (sort rows (lambda (a b) (> (nth 2 a) (nth 2 b))))))
    ;; Reconstruct in the visiting buffer (avoids stale-buffer clobber).
    (with-current-buffer (find-file-noselect file)
      (erase-buffer)
      (insert fm "\n\n")
      (insert "* Followers\n:PROPERTIES:\n:X_USER: " xuser
              "\n:COUNT: " (number-to-string (length handles))
              "\n:UPDATED: " (format-time-string "[%Y-%m-%d %a %H:%M]")
              "\n:END:\n\n")
      ;; Sortable table. Numbers are raw ints so org's C-c ^ sorts numerically.
      (insert "| Handle | Type | Followers | Following | Since |\n"
              "|--------+------+-----------+-----------+-------|\n")
      (dolist (r rows)
        (insert (format "| [[https://x.com/%s][@%s]] | %s | %d | %d | %s |\n"
                        (nth 0 r) (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r) (nth 4 r))))
      (insert "\n* Changelog\n"
              (format "** %s  +%d / -%d\n"
                      (format-time-string "[%Y-%m-%d %a %H:%M]")
                      (length new) (length gone)))
      (dolist (h (sort (copy-sequence new) #'string<))
        (insert (format "+ @%s%s\n" h (my/x--annot-line (gethash h info)))))
      (dolist (h (sort (copy-sequence gone) #'string<))
        (insert (format "- @%s\n" h)))
      (when (org-string-nw-p old-cl) (insert "\n" old-cl "\n"))
      (insert "\n* Set\n#+begin_src text :name followers\n")
      (dolist (h (sort (copy-sequence handles) #'string<)) (insert h "\n"))
      (insert "#+end_src\n")
      (goto-char (point-min))
      (when (re-search-forward "^| Handle " nil t)
        (ignore-errors (org-table-align)))
      (save-buffer))))

(defun my/x--report (username new gone total file info)
  (let ((buf (get-buffer-create (format "*X Followers: %s*" username))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "@%s — %s\nTotal %d   (+%d new, -%d unfollowed)\n\n"
                        username (format-time-string "%Y-%m-%d %H:%M")
                        total (length new) (length gone)))
        (insert "── New ──\n")
        (if new (dolist (h (sort (copy-sequence new) #'string<))
                  (insert (format "  + @%s%s\n" h (my/x--annot-line (gethash h info)))))
          (insert "  (none)\n"))
        (insert "\n── Unfollowed ──\n")
        (if gone (dolist (h (sort (copy-sequence gone) #'string<)) (insert (format "  - @%s\n" h)))
          (insert "  (none)\n"))
        (insert (format "\nNote: %s\n" file)))
      (goto-char (point-min)) (view-mode 1))
    (display-buffer buf)))

(defun my/x--read-username ()
  "Pick a username: select an existing tracked handle or type a new one."
  (let* ((names (mapcar #'car (my/x--all-users)))
         (new-label "＋ New username…")
         (choice (completing-read "X account (select or type new): "
                                  (append names (list new-label)) nil nil)))
    (my/x--normalize (if (string= choice new-label)
                         (read-string "New X username (no @): ")
                       choice))))

;;;###autoload
(defun my/x-followers (username)
  "Fetch USERNAME's followers, diff against the stored note, report changes.
Username is read from existing notes (or type a new one). First run records a
baseline; later runs show who is NEW and who UNFOLLOWED."
  (interactive (list (my/x--read-username)))
  (unless (org-string-nw-p username) (user-error "No username"))
  (let* ((existed (my/x--find-note username))
         (file (my/x--note-file username))
         (previous (and existed (my/x--read-set file)))
         (fetched (my/x--fetch-followers username))
         (current (car fetched))
         (info (cdr fetched))
         (cur (let ((h (make-hash-table :test 'equal)))
                (dolist (x current h) (puthash x t h))))
         (prev (let ((h (make-hash-table :test 'equal)))
                 (dolist (x previous h) (puthash x t h))))
         (new (seq-remove (lambda (h) (gethash h prev)) current))
         (gone (seq-remove (lambda (h) (gethash h cur)) previous)))
    (my/x--write-note file current new gone info)
    (if existed
        (my/x--report username new gone (length current) file info)
      (my/x--report username nil nil (length current) file info)
      (message "Baseline: %d followers for @%s" (length current) username))))

;; ── Trends ───────────────────────────────────────────────────────────────

(defun my/x--trends (&optional woeid)
  "Return trends for WOEID as a list of (NAME . COUNT)."
  (delq nil
        (mapcar (lambda (line)
                  (when (string-match "\\`\\([0-9]+\\)\t\\(.+\\)\\'" line)
                    (cons (match-string 2 line) (string-to-number (match-string 1 line)))))
                (split-string (my/x--run "trends" (number-to-string (or woeid my/x-trends-woeid)))
                              "\n" t))))

(defun my/x--trends-org (trends)
  (mapconcat (lambda (tc)
               (if (> (cdr tc) 0)
                   (format "- %s — %d posts" (car tc) (cdr tc))
                 (format "- %s" (car tc))))
             trends "\n"))

;;;###autoload
(defun my/x-trends (&optional woeid)
  "Show X trends for WOEID (prefix prompts) in an Org buffer."
  (interactive (list (if current-prefix-arg
                         (read-number "WOEID: " my/x-trends-woeid) my/x-trends-woeid)))
  (let ((trends (my/x--trends woeid)))
    (unless trends (user-error "No trends (token/WOEID?)"))
    (with-current-buffer (get-buffer-create "*X Trends*")
      (let ((inhibit-read-only t))
        (erase-buffer) (org-mode)
        (insert (format "#+title: X Trends (WOEID %s) — %s\n\n"
                        woeid (format-time-string "%Y-%m-%d %H:%M")))
        (insert (my/x--trends-org trends) "\n"))
      (goto-char (point-min)) (display-buffer (current-buffer)))))

;;;###autoload
(defun my/x-trends-insert (&optional woeid)
  "Insert X trends as an Org list at point (prefix prompts WOEID)."
  (interactive (list (if current-prefix-arg
                         (read-number "WOEID: " my/x-trends-woeid) my/x-trends-woeid)))
  (let ((trends (my/x--trends woeid)))
    (unless trends (user-error "No trends (token/WOEID?)"))
    (insert (my/x--trends-org trends) "\n")))

;; ── Menu ─────────────────────────────────────────────────────────────────

;;;###autoload (autoload 'my/x "init-x" nil t)
(transient-define-prefix my/x ()
  "X (Twitter)."
  ["X (Twitter)"
   ("p" "Post subtree/region" my/x-post)
   ("f" "Followers check"     my/x-followers)
   ("t" "Trends"              my/x-trends)
   ("T" "Insert trends here"  my/x-trends-insert)])

(provide 'init-x)
