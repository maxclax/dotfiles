;;; +git.el -*- lexical-binding: t; -*-

;; Make Emacs subprocesses (magit push/pull over SSH, TRAMP) authenticate with
;; the 1Password SSH agent. GUI Emacs is launched without a login shell, so it
;; doesn't inherit the agent socket the way a terminal does — without this,
;; magit's `ssh' can't offer the GitHub key and push fails with "failed to push
;; some refs". Point SSH_AUTH_SOCK straight at the 1Password agent.
(let ((op-agent (expand-file-name
                 "Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock"
                 (getenv "HOME"))))
  (when (file-exists-p op-agent)
    (setenv "SSH_AUTH_SOCK" op-agent)))

(after! git-link
  (setq git-link-open-in-browser nil
        git-link-use-commit t)

  ;; OVERRIDE
  (advice-add #'git-link--select-remote :override #'git-link--read-remote))

(after! magit
  ;; Performance optimizations
  (setq magit-save-repository-buffers nil
        git-commit-style-convention-checks nil
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1
        
        ;; Don't ALSO refresh the status buffer after commands issued from
        ;; other magit buffers (diff/log) — it refreshes when you return to it
        magit-refresh-status-buffer nil
        magit-refresh-verbose nil
        
        ;; Reduce expensive operations
        magit-revision-show-gravatars nil
        magit-log-show-refname-after-summary t
        
        ;; Limit log entries for performance
        magit-log-auto-more nil
        
        ;; Word-level diff highlighting in all hunks
        magit-diff-refine-hunk 'all

        ;; Ahead/behind counts in the refs buffer (y): <N ahead, N> behind, =
        magit-refs-show-commit-count 'all

        ;; Faster diffs
        magit-diff-highlight-hunk-region-functions nil
        magit-diff-paint-whitespace nil
        magit-diff-highlight-trailing nil
        
        ;; Balanced status sections - keep essential info but optimize performance
        magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes
          magit-insert-stashes
          magit-insert-unpushed-to-pushremote
          magit-insert-unpushed-to-upstream
          magit-insert-unpulled-from-pushremote
          magit-insert-unpulled-from-upstream
          magit-insert-recent-commits
          magit-insert-local-branches)
        
        ;; Show recent commits in log
        magit-log-section-commit-count 10
        
        ;; Show all branches in log by default
        magit-log-arguments '("--graph" "--color" "--decorate" "-n256")
        
        ;; Display branch info in headers
        magit-status-headers-hook
        '(magit-insert-error-header
          magit-insert-diff-filter-header
          magit-insert-repo-header
          magit-insert-head-branch-header
          magit-insert-upstream-branch-header
          magit-insert-push-branch-header
          magit-insert-remote-header
          magit-insert-tags-header)
        
        ;; Show branch information
        magit-status-show-hashes-in-headers t)

  ;; Submodule visit strategies — exactly ONE of these advised at a time.
  ;; Workspace-per-submodule (previous behavior, kept for easy rollback):
  (defun my/magit-submodule-in-workspace (fn module &rest args)
    (let ((name (file-name-nondirectory (directory-file-name module))))
      (unless (+workspace-exists-p name)
        (+workspace/new name))
      (+workspace/switch-to name)
      (apply fn module args)))

  ;; Tab-per-submodule (testing): submodule opens in a native tab inside
  ;; the current workspace. `tab-bar-switch-to-tab' creates the tab when
  ;; it doesn't exist yet. Roll back by advising the workspace fn instead.
  (defun my/magit-submodule-in-tab (fn module &rest args)
    (let ((name (file-name-nondirectory (directory-file-name module))))
      (tab-bar-switch-to-tab name)
      (apply fn module args)))

  (advice-add 'magit-submodule-visit :around #'my/magit-submodule-in-tab)

  ;; RET on a submodule line in unstaged/staged sections goes through
  ;; magit-diff-visit-file, not magit-submodule-visit — give it the same
  ;; open-in-tab behavior.
  (defun my/magit-visit-submodule-in-tab-a (orig-fn &rest args)
    (let* ((file (magit-file-at-point))
           (abs (and file (expand-file-name file (magit-toplevel)))))
      (if (and abs (file-directory-p abs)
               (file-exists-p (expand-file-name ".git" abs)))
          (let ((name (file-name-nondirectory (directory-file-name abs))))
            (tab-bar-switch-to-tab name)
            (magit-status-setup-buffer abs))
        (apply orig-fn args))))
  (advice-add 'magit-diff-visit-file :around #'my/magit-visit-submodule-in-tab-a)

  ;; Submodule sections — overview + unpulled only (no duplicate @{push})
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-stashes t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-stashes t)

  ;; Pin-origin tag on each Modules overview line (and the `o l` list):
  ;;   ≡ main     gitlink identical to parent `main` (inherited)
  ;;   ● <branch> gitlink differs from `main` (this branch moved it)
  ;;   + new      gitlink not on `main` at all
  ;;   !          working-tree checkout ≠ recorded gitlink
  ;; Gitlinks are submodule oids — compare as strings. Never magit-rev-eq
  ;; in the parent repo: the object is not there.
  (defun my/magit--gitlinks (rev)
    "Alist of (PATH . SHA1) for submodule gitlinks recorded at REV."
    (let (alist)
      (dolist (line (magit-git-lines "ls-tree" "-r" rev))
        (when (string-match "\\`160000 commit \\([0-9a-f]+\\)\t\\(.+\\)\\'" line)
          (push (cons (match-string 2 line) (match-string 1 line)) alist)))
      alist))

  (defun my/magit--abbrev-oid (oid)
    (and oid (if (> (length oid) 7) (substring oid 0 7) oid)))

  (defun my/magit--main-rev ()
    "Parent-repo rev to treat as the template branch, or nil."
    (cond ((magit-rev-verify "refs/heads/main") "refs/heads/main")
          ((magit-rev-verify "origin/main") "origin/main")))

  (defun my/magit--origin-badge (kind label help &optional extra)
    "KIND is `main', `new', or `here'. EXTRA is appended (e.g. drifted !)."
    (let* ((face (pcase kind
                   ('main 'magit-dimmed)
                   ('new 'magit-diff-added)
                   ('here 'magit-branch-local)))
           (icon-name (pcase kind
                        ('main "nf-oct-dot_fill")
                        ('new "nf-oct-plus")
                        ('here "nf-oct-git_branch")))
           (icon (and (display-graphic-p)
                      (fboundp 'nerd-icons-octicon)
                      (nerd-icons-octicon icon-name :face face
                                          :height 0.85 :v-adjust 0.0)))
           (ascii (pcase kind ('main "= ") ('new "+ ") ('here "* ")))
           (text (concat (or icon ascii)
                         (and icon " ")
                         (propertize label 'font-lock-face face)
                         (or extra ""))))
      (propertize text 'help-echo help)))

  (defun my/magit--module-origin-tag (module branch head-sha main-sha live-sha)
    "Propertized pin-origin suffix for MODULE. Empty when already on main."
    (if (equal branch "main")
        ""
      (let* ((drifted (and head-sha live-sha (not (equal head-sha live-sha))))
             (kind (cond ((and head-sha main-sha (equal head-sha main-sha)) 'main)
                         ((not main-sha) 'new)
                         (t 'here)))
             (label (pcase kind
                      ('main "main")
                      ('new "new")
                      ('here (or branch "here"))))
             (help (format
                    (concat "%s\nrecorded on %s: %s\n"
                            "recorded on main: %s\ncheckout: %s%s")
                    (pcase kind
                      ('main "Pin identical to main — this branch did not move it.")
                      ('new "Not on main — added on this branch.")
                      ('here "Pin unique to this branch — differs from main."))
                    (or branch "HEAD")
                    (or (my/magit--abbrev-oid head-sha) "—")
                    (or (my/magit--abbrev-oid main-sha) "—")
                    (or (my/magit--abbrev-oid live-sha) "—")
                    (if drifted "\nCheckout does not match the recorded pin." "")))
             (mark (if drifted
                       (concat " " (propertize "!" 'font-lock-face 'warning))
                     "")))
        (propertize (my/magit--origin-badge kind label help mark)
                    'module module))))

  (defun my/magit--insert-modules-overview (&optional _section repos)
    "Like `magit--insert-modules-overview', plus pin-origin vs `main'."
    (magit-with-toplevel
      (let* ((modules (or repos (magit-list-module-paths)))
             (path-format (format "%%-%ds "
                                  (min (apply #'max (mapcar #'length modules))
                                       (/ (window-width) 2))))
             (branch-format (format "%%-%ds " (min 25 (/ (window-width) 3))))
             (parent-branch (magit-get-current-branch))
             (head-links (my/magit--gitlinks "HEAD"))
             (main-rev (my/magit--main-rev))
             (main-links (and main-rev (my/magit--gitlinks main-rev)))
             (tag-pins (and main-rev (not (equal parent-branch "main")))))
        (dolist (module modules)
          (let* ((default-directory
                  (expand-file-name (file-name-as-directory module)))
                 (populated (file-exists-p ".git")))
            (magit-insert-section (module module t)
              (insert (propertize (format path-format module)
                                  'font-lock-face 'magit-diff-file-heading))
              (if (not populated)
                  (insert "(unpopulated)")
                (insert
                 (format
                  branch-format
                  (if-let ((branch (magit-get-current-branch)))
                      (propertize branch 'font-lock-face 'magit-branch-local)
                    (propertize "(detached)" 'font-lock-face 'warning))))
                (cond-let
                  ([desc (magit-git-string "describe" "--tags")]
                   (when (and magit-modules-overview-align-numbers
                              (string-match-p "\\`[0-9]" desc))
                     (insert ?\s))
                   (insert (propertize desc 'font-lock-face 'magit-tag)))
                  ([abbrev (magit-rev-format "%h")]
                   (insert (propertize abbrev 'font-lock-face 'magit-hash)))))
              (when tag-pins
                (let ((tag (my/magit--module-origin-tag
                            module parent-branch
                            (cdr (assoc module head-links))
                            (cdr (assoc module main-links))
                            (and populated (magit-rev-parse "HEAD")))))
                  (unless (string-empty-p tag)
                    (insert "  " tag))))
              (insert ?\n))))))
    (insert ?\n))

  (advice-add 'magit--insert-modules-overview :override
              #'my/magit--insert-modules-overview)

  (defun my/magit-modulelist-column-origin (_id)
    "Pin origin vs parent `main'. `default-directory' is the module."
    (when-let* ((super (magit-git-string "rev-parse" "--show-superproject-working-tree"))
                (path (directory-file-name
                       (file-relative-name
                        (directory-file-name (expand-file-name default-directory))
                        (directory-file-name (expand-file-name super)))))
                (live (magit-rev-parse "HEAD")))
      (let* ((default-directory super)
             (branch (magit-get-current-branch))
             (main-rev (my/magit--main-rev)))
        (when (and main-rev (not (equal branch "main")))
          (my/magit--module-origin-tag
           path branch
           (cdr (assoc path (my/magit--gitlinks "HEAD")))
           (cdr (assoc path (my/magit--gitlinks main-rev)))
           live)))))

  (add-to-list 'magit-submodule-list-columns
               '("Pin" 12 my/magit-modulelist-column-origin ())
               t)

  ;; Same badge on untracked / unstaged / staged file lines in magit-status:
  ;;   ≡ main  path exists on main (HEAD blob matches, if HEAD has it)
  ;;   ● <br>  path exists on main, but this branch committed a different blob
  ;;   + new   path does not exist on main at all
  (defun my/magit--file-origin-tag (file)
    "Badge for FILE vs parent `main'. Empty on main or if main is missing."
    (let ((branch (magit-get-current-branch))
          (main-rev (my/magit--main-rev))
          (path (directory-file-name file)))
      (if (or (not main-rev) (equal branch "main") (string-empty-p path))
          ""
        (let* ((main-oid (magit-git-string "rev-parse" "--verify" "--quiet"
                                           (concat main-rev ":" path)))
               (head-oid (magit-git-string "rev-parse" "--verify" "--quiet"
                                           (concat "HEAD:" path)))
               (kind (cond ((not main-oid) 'new)
                           ((or (not head-oid) (equal head-oid main-oid)) 'main)
                           (t 'here)))
               (label (pcase kind
                        ('main "main")
                        ('new "new")
                        ('here (or branch "here"))))
               (help (format "%s\nHEAD: %s\nmain: %s"
                             (pcase kind
                               ('main "Also on main.")
                               ('new "Not on main — only here.")
                               ('here "On main too, but this branch has a different committed version."))
                             (or (my/magit--abbrev-oid head-oid) "—")
                             (or (my/magit--abbrev-oid main-oid) "—"))))
          (my/magit--origin-badge kind label help)))))

  (defvar my/magit--orig-format-file nil)

  (defun my/magit-format-file-with-origin (kind file face &optional status orig)
    (let ((base (funcall my/magit--orig-format-file kind file face status orig)))
      (if (and (derived-mode-p 'magit-status-mode)
               (memq kind '(list diff)))
          (let ((tag (my/magit--file-origin-tag file)))
            (if (string-empty-p tag) base (concat base "  " tag)))
        base)))

  (unless (eq magit-format-file-function #'my/magit-format-file-with-origin)
    (setq my/magit--orig-format-file magit-format-file-function
          magit-format-file-function #'my/magit-format-file-with-origin))

  ;; Status: short age (" 9h") — 12 cols. Log buffers: full datetime — 24 cols.
  ;; WIDTH must be an integer. Magit's `magit-log-margin-width' *function* is
  ;; only resolved in `magit-set-buffer-margins'; `magit-log-format-margin'
  ;; reads the raw option first and `(- magit-log-margin-width …)` errors
  ;; (persp restore, fetch sentinel). `setq', not `customize-set-variable':
  ;; magit's :set walks every magit-status/log buffer and refreshes it, which
  ;; on doom/reload freezes Emacs before init-org.el has loaded.
  (setq magit-status-margin '(t age-abbreviated 12 t 7)
        magit-log-margin '(t "%Y-%m-%d %H:%M" 24 t 7))


  ;; Auto-save WIP to hidden refs — never lose uncommitted work
  (magit-wip-mode 1)

  ;; Fetch remotes when opening magit status. Async — magit refreshes after
  ;; fetch completes so unpulled sections show real remote state.
  (defadvice! my/magit-fetch-on-status (&rest _)
    :before #'magit-status
    (when (magit-toplevel)
      (let ((proc (start-process "magit-fetch" nil "git" "fetch" "--all" "--quiet")))
        (set-process-sentinel proc
                              (lambda (_proc event)
                                (when (string-match-p "finished" event)
                                  (magit-refresh-all))))))))

(use-package! magit-delta
  :after magit
  :hook (magit-mode . magit-delta-mode)
  :config
  ;; --no-gitconfig: gitconfig `delta.line-numbers' leaks through --color-only
  ;; and replaces the -/+ markers magit parses — sections collapse and diff
  ;; text lands orphaned at the buffer bottom (terminal delta is unaffected).
  ;;
  ;; --dark/--light must be explicit *and chosen at call time*. magit-delta
  ;; only injects --syntax-theme; delta still auto-detects dark/light for
  ;; plus/minus and merge-conflict headers, and a pipe looks like a light
  ;; terminal. Caching the flags at load/theme-hook time is wrong with
  ;; auto-dark: config often runs on a dark/unspecified daemon frame, then
  ;; doom-one-light is applied and Magit keeps painting dark hunks.
  (defun my/emacs-bg-dark-p ()
    "Non-nil if the current frame is dark."
    (pcase (frame-parameter nil 'background-mode)
      ('dark t)
      ('light nil)
      (_
       (let* ((bg (face-background 'default nil t))
              (rgb (and bg (color-name-to-rgb bg))))
         (and rgb (< (+ (nth 0 rgb) (nth 1 rgb) (nth 2 rgb)) 1.5))))))

  (defadvice! my/magit-delta--make-delta-args-a (orig-fn)
    "Pick --dark/--light from the live frame, not from a cached list."
    :around #'magit-delta--make-delta-args
    (let ((magit-delta-delta-args
           (append '("--max-line-distance" "0.6"
                     "--true-color" "always"
                     "--color-only"
                     "--no-gitconfig")
                   (if (my/emacs-bg-dark-p)
                       '("--dark" "--syntax-theme" "OneHalfDark")
                     '("--light" "--syntax-theme" "OneHalfLight")))))
      (funcall orig-fn)))

  (defun my/magit-delta-refresh (&rest _)
    ;; skip killed buffers — org restart can race the auto-dark timer
    (when after-init-time
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (derived-mode-p 'magit-mode)
              (magit-refresh-buffer)))))))

  (when (boundp 'enable-theme-functions)
    (add-hook 'enable-theme-functions #'my/magit-delta-refresh))
  (add-hook 'auto-dark-dark-mode-hook #'my/magit-delta-refresh)
  (add-hook 'auto-dark-light-mode-hook #'my/magit-delta-refresh))

;; TODO/FIXME/NOTE items from the repo as a section in magit status.
;; Keywords and colors come from hl-todo (configured in +ui.el).
(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-insert-after '(bottom)   ; last section, below recent commits
        magit-todos-max-items 15
        ;; rescan at most every 30s — staging commands reuse the cached scan
        ;; instead of re-running rg + popping the section in late every time
        magit-todos-update 30
        ;; the branch-list scan pipes `git diff <merge-base>' through Emacs —
        ;; 100MB+ on big repos, freezing every magit refresh ("Running…" forever)
        magit-todos-branch-list nil
        ;; `magit-todos-keywords' needs its custom :set — plain setq is
        ;; ignored, so set the derived list the scanners actually read
        magit-todos-keywords-list '("TODO" "FIXME" "FAIL" "DEBUG")
        ;; static/**/plugins = vendored js libs (amcharts etc.); own static
        ;; assets outside plugins/ still get scanned
        magit-todos-exclude-globs '(".git/" "node_modules/" "vendor/" "vendors/"
                                    "dist/" "docs/" "static/**/plugins/"
                                    "*.min.js" "*.min.css" "*.map"))

  ;; Stock scanners are comment-blind: python `if DEBUG:' matches same as a
  ;; `DEBUG:' comment. This scanner requires the keyword right after a
  ;; comment marker (annotation style), so commented-out code like
  ;; `# if settings.DEBUG:' doesn't count. Org `* TODO' headings are NOT
  ;; matched: they are agenda tasks, owned by org-agenda, not code
  ;; annotations — in a notes repo they drowned the section (396 items).
  (magit-todos-defscanner "rg comments"
    :availablep (lambda () (executable-find "rg"))
    :directory-form (if (equal directory default-directory)
                        nil ; prevent leading "./" in filenames
                      (f-relative directory default-directory))
    :allow-exit-codes (0 1)
    :command (let ((kws (mapconcat #'regexp-quote magit-todos-keywords-list "|")))
               (list "rg" "--no-heading" "--line-number"
                     (when depth
                       (list "--maxdepth" (1+ depth)))
                     (when magit-todos-ignore-case
                       "--ignore-case")
                     (when magit-todos-exclude-globs
                       (--map (list "--glob" (concat "!" it))
                              magit-todos-exclude-globs))
                     (unless magit-todos-submodule-list
                       (--map (list "--glob" (concat "!" it))
                              (magit-list-module-paths)))
                     extra-args
                     (format "(?:#+|/{2,}|;+|-{2,}|/\\*+|<!--|^[ \\t]*\\*+)[ \\t]*(?:%s)(?:[\\[(][^\\])]*[)\\]])?:"
                             kws)
                     directory)))
  (setq magit-todos-scanner #'magit-todos--scan-with-rg-comments)
  (magit-todos-mode 1))

;; Background fetch: git can only show incoming (↓ unpulled) after a fetch,
;; so fetch quietly on a timer for repos with an open magit status buffer.
;; Refresh only when the fetch actually brought refs — idle repos cost nothing.
(defvar my/magit-autofetch-interval 300
  "Seconds between background fetches of open magit status repos.")

(defvar my/magit-autofetch--timer nil)

(defun my/magit-autofetch--repos ()
  "Directories of all live magit status buffers, without duplicates."
  (delete-dups
   (mapcar (lambda (buf) (buffer-local-value 'default-directory buf))
           (seq-filter (lambda (buf)
                         (with-current-buffer buf
                           (derived-mode-p 'magit-status-mode)))
                       (buffer-list)))))

(defun my/magit-autofetch--fetch (dir)
  (unless (or (file-remote-p dir)
              (get-process (concat "autofetch:" dir)))
    (let ((default-directory dir))
      (make-process
       :name (concat "autofetch:" dir)
       :buffer (generate-new-buffer " *magit-autofetch*")
       :command (list magit-git-executable "--no-pager" "fetch")
       :noquery t
       :sentinel
       (lambda (proc _event)
         (when (memq (process-status proc) '(exit signal))
           (let ((out (with-current-buffer (process-buffer proc)
                        (buffer-string))))
             (kill-buffer (process-buffer proc))
             (when (and (eq (process-exit-status proc) 0)
                        (string-match-p "[^ \t\n]" out))
               (let ((default-directory dir))
                 (when-let ((buf (magit-get-mode-buffer 'magit-status-mode)))
                   (with-current-buffer buf (magit-refresh-buffer))))))))))))

(defun my/magit-kill-stale-buffers ()
  "Kill magit buffers whose repository directory no longer exists."
  (interactive)
  (let ((n 0))
    (dolist (buf (buffer-list))
      (let ((dir (buffer-local-value 'default-directory buf)))
        (when (and (with-current-buffer buf (derived-mode-p 'magit-mode))
                   (not (file-remote-p dir))
                   (not (file-directory-p dir)))
          (kill-buffer buf)
          (setq n (1+ n)))))
    (when (called-interactively-p 'any)
      (message "Killed %d stale magit buffer(s)" n))
    n))

(defun my/magit-autofetch ()
  ;; a deleted worktree leaves its status buffer behind; fetching there errors
  (my/magit-kill-stale-buffers)
  (mapc #'my/magit-autofetch--fetch (my/magit-autofetch--repos)))

(after! magit
  (when my/magit-autofetch--timer (cancel-timer my/magit-autofetch--timer))
  (setq my/magit-autofetch--timer
        (run-with-timer 60 my/magit-autofetch-interval #'my/magit-autofetch)))

;; ── Magit buffers are global; tab window-configs resurrect them in foreign
;; workspaces. Claim each magit buffer for the workspace it was opened in,
;; and on workspace switch bury any displayed magit buffer that does not
;; belong to the target workspace (the tab's window then shows its previous
;; buffer instead of another project's status).
(defun my/magit-claim-buffer-h ()
  ;; `this-command' guard: only claim on user-driven creation/refresh —
  ;; the background autofetch refresh must not steal buffers into
  ;; whatever workspace happens to be active.
  (when (and (bound-and-true-p persp-mode) this-command)
    (persp-add-buffer (current-buffer) (get-current-persp) nil nil)))
(add-hook 'magit-mode-hook #'my/magit-claim-buffer-h)
(add-hook 'magit-refresh-buffer-hook #'my/magit-claim-buffer-h)

(defun my/magit-bury-foreign-h (&rest _)
  (when (bound-and-true-p persp-mode)
    (dolist (win (window-list))
      (let ((buf (window-buffer win)))
        (when (and (buffer-live-p buf)
                   (provided-mode-derived-p
                    (buffer-local-value 'major-mode buf) 'magit-mode)
                   (not (persp-contain-buffer-p buf (get-current-persp))))
          (with-selected-window win (previous-buffer)))))))
(add-hook 'persp-activated-functions #'my/magit-bury-foreign-h)

;; ── Promote working-tree changes to `main' ──────────────────────────────────
;; Several projects here keep the framework/template on `main' while daily
;; work happens on another branch (personal / develop / application), with
;; `main' checked out in a worktree. Changes meant for the template are
;; usually noticed while sitting on the wrong branch. This commits them ON
;; main (through that worktree) and merges main back — no branch switching,
;; no stash dance. Aborts untouched if the patch does not apply on main.

(defun my/git-main-worktree (&optional dir)
  "Path of the worktree that has branch `main' checked out, or nil."
  (let ((default-directory (or dir default-directory))
        wt found)
    (dolist (line (ignore-errors
                    (process-lines "git" "worktree" "list" "--porcelain")))
      (cond ((string-prefix-p "worktree " line)
             (setq wt (substring line (length "worktree "))))
            ((equal line "branch refs/heads/main")
             (setq found wt))))
    found))

(defun my/magit-promote-to-main (message &optional include-untracked)
  "Commit all working-tree changes on `main', push it, then merge into HEAD.
With a prefix argument, also promote untracked files. The commit is made
in the worktree that has `main' checked out; this buffer's branch is
never switched.

Where main is simply behind, files it has not yet caught up on are merged
three-way rather than refused. Real conflicts abort, and the local copies
are only discarded once main is confirmed to have moved."
  (interactive (list (magit-read-string "Commit message (on main)")
                     current-prefix-arg))
  (let* ((src (or (magit-toplevel) (user-error "Not inside a git repository")))
         (default-directory src)
         (branch (magit-get-current-branch))
         (wt (my/git-main-worktree src))
         (patch (make-temp-file "magit-promote" nil ".patch"))
         (untracked (and include-untracked (magit-untracked-files)))
         before pushed)
    (when (equal branch "main")
      (user-error "Already on main — just commit normally"))
    (unless wt
      (user-error "No worktree of this repo has branch `main' checked out"))
    ;; Backing out a failed apply means hard-resetting main's worktree, so
    ;; refuse to start while it still holds anything of its own.
    (when (let ((default-directory wt)) (magit-git-string "status" "--porcelain"))
      (user-error "The `main' worktree is dirty — commit or discard there first"))
    (setq before (magit-rev-parse "main"))
    (unwind-protect
        (progn
          ;; intent-to-add makes untracked files visible to `git diff'
          (dolist (f untracked) (magit-call-git "add" "-N" f))
          (with-temp-file patch
            (call-process "git" nil t nil "diff" "HEAD"))
          (when (zerop (file-attribute-size (file-attributes patch)))
            (user-error "Nothing to promote"))
          (let ((default-directory wt))
            ;; The patch is cut against this branch's HEAD. If main is merely
            ;; behind, its copy of a touched file has older context and a
            ;; straight apply fails — retry as a three-way merge, which
            ;; reconstructs the base from the blob ids in the patch.
            ;; NB: `--3way --check' exits 0 even when it would conflict, so
            ;; the real apply is the only usable gate.
            (unless (zerop (call-process "git" nil nil nil "apply" patch))
              (unless (zerop (call-process "git" nil nil nil "apply" "--3way" patch))
                (let ((bad (magit-git-lines "diff" "--name-only" "--diff-filter=U")))
                  (magit-call-git "reset" "--hard" "HEAD")
                  (magit-call-git "clean" "-fd")
                  (user-error "Conflicts on main in %s — promote those by hand"
                              (string-join bad ", ")))))
            (magit-call-git "add" "--all")
            (magit-call-git "commit" "-m" message))
          ;; Did main actually gain a commit? A rejected commit-msg hook, a
          ;; signing failure or an empty tree all leave it where it was, and
          ;; `magit-call-git' reports none of them. Never discard the local
          ;; copies on the strength of a commit that did not happen.
          (when (equal before (magit-rev-parse "main"))
            (let ((default-directory wt))
              (magit-call-git "reset" "--hard" "HEAD")
              (magit-call-git "clean" "-fd"))
            (user-error "Nothing was committed on main — your changes are untouched"))
          ;; main is the shared branch, so publish it here — a promote left
          ;; sitting locally is half done. A rejected push is reported, not
          ;; fatal: the commit is on main either way, and merging back still
          ;; beats keeping a duplicate copy on this branch.
          (setq pushed
                (let ((default-directory wt))
                  (zerop (magit-call-git
                          "push" (or (magit-get "branch.main.remote") "origin")
                          "main"))))
          ;; drop the now-duplicated local copies, then merge main back
          (magit-call-git "reset" "--hard" "HEAD")
          (dolist (f untracked)
            (ignore-errors (delete-file (expand-file-name f src))))
          (magit-call-git "merge" "main" "--no-edit")
          (magit-refresh)
          (message "Promoted to main%s and merged into %s"
                   (if pushed " and pushed" " (PUSH FAILED — push main by hand)")
                   branch))
      (delete-file patch))))

;; ── Cherry-pick a finished commit onto `main' ───────────────────────────────
;; The counterpart of `my/magit-promote-to-main' for work already committed on
;; this branch: the commit is copied to main (through its worktree) and pushed.
;; It stays on this branch too — two SHAs for one patch, which merges cleanly;
;; removing it here would mean rewriting the branch, so it is left alone.

(defun my/magit-cherry-pick-to-main (commit)
  "Cherry-pick COMMIT onto `main' in its worktree, then push main.
Defaults to HEAD; with a prefix argument, prompt for the commit. This
buffer's branch is never switched and keeps the commit as well."
  (interactive
   (list (if current-prefix-arg
             (magit-read-branch-or-commit "Cherry-pick to main")
           (or (magit-rev-parse "HEAD")
               (user-error "No commit at HEAD")))))
  (let* ((src (or (magit-toplevel) (user-error "Not inside a git repository")))
         (default-directory src)
         (branch (magit-get-current-branch))
         (wt (my/git-main-worktree src))
         (subject (magit-rev-format "%s" commit))
         before pushed)
    (when (equal branch "main")
      (user-error "Already on main — nothing to cherry-pick"))
    (unless wt
      (user-error "No worktree of this repo has branch `main' checked out"))
    ;; HEAD is a merge commit right after `git merge main' — git refuses to
    ;; cherry-pick one without -m, so say which commit was meant instead.
    (when (> (length (magit-commit-parents commit)) 1)
      (user-error "%s is a merge commit — pick the real one with C-u" 
                  (magit-rev-abbrev commit)))
    (when (let ((default-directory wt)) (magit-git-string "status" "--porcelain"))
      (user-error "The `main' worktree is dirty — commit or discard there first"))
    (setq before (let ((default-directory wt)) (magit-rev-parse "HEAD")))
    (let ((default-directory wt))
      ;; A conflict leaves main mid-sequence; back it out so the worktree is
      ;; usable again and the resolution can be done deliberately.
      (unless (zerop (magit-call-git "cherry-pick" commit))
        (let ((bad (magit-git-lines "diff" "--name-only" "--diff-filter=U")))
          (magit-call-git "cherry-pick" "--abort")
          (user-error "Conflicts on main in %s — cherry-pick it by hand"
                      (string-join bad ", "))))
      ;; A rejected commit-msg hook or an empty pick leaves main where it was.
      (when (equal before (magit-rev-parse "HEAD"))
        (user-error "Nothing landed on main — main is unchanged"))
      (setq pushed (zerop (magit-call-git
                           "push" (or (magit-get "branch.main.remote") "origin")
                           "main"))))
    (magit-refresh)
    (message "Cherry-picked \"%s\" onto main%s"
             subject
             (if pushed " and pushed" " (PUSH FAILED — push main by hand)"))))
