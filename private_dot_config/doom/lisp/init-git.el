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
        
        ;; Speed up status buffer but keep auto-refresh
        magit-refresh-status-buffer t
        magit-refresh-verbose nil
        
        ;; Reduce expensive operations
        magit-revision-show-gravatars nil
        magit-log-show-refname-after-summary t
        
        ;; Limit log entries for performance
        magit-log-auto-more nil
        
        ;; Word-level diff highlighting in all hunks
        magit-diff-refine-hunk 'all

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

  ;; Open submodule in its own dedicated workspace when navigating to it.
  (defadvice! my/magit-submodule-in-workspace (fn module &rest args)
    :around #'magit-submodule-visit
    (let ((name (file-name-nondirectory (directory-file-name module))))
      (unless (+workspace-exists-p name)
        (+workspace/new name))
      (+workspace/switch-to name)
      (apply fn module args)))

  ;; Submodule sections — overview + unpulled only (no duplicate @{push})
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-overview
                          'magit-insert-stashes t)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules-unpulled-from-upstream
                          'magit-insert-stashes t)

  ;; Right margin: author + absolute datetime + relative age, in status
  ;; recent-commits AND log buffers (format: (INIT STYLE WIDTH AUTHOR AUTHOR-W))
  (customize-set-variable 'magit-status-margin '(t "%Y-%m-%d %H:%M" 30 t 7))
  (customize-set-variable 'magit-log-margin '(t "%Y-%m-%d %H:%M" 30 t 7))

  ;; Append "(2d)"-style age after the datetime. Magit's stock renderer does
  ;; datetime OR age, never both — this override (copy of
  ;; `magit-log-format-author-margin' from the pinned magit) adds the suffix.
  (advice-add 'magit-log-format-author-margin :override
              (defun my/magit-margin-datetime+age-a (author date)
                (pcase-let ((`(,_ ,style ,width ,details ,details-width)
                             (or magit--right-margin-config
                                 (symbol-value (magit--right-margin-option))
                                 (error "No margin format specified for %s" major-mode))))
                  (magit-make-margin-overlay
                   (concat (and details
                                (concat (magit--propertize-face
                                         (truncate-string-to-width
                                          (or author "") details-width nil ?\s
                                          (magit--ellipsis 'margin))
                                         'magit-log-author)
                                        " "))
                           (magit--propertize-face
                            (if (stringp style)
                                (concat
                                 (format-time-string
                                  style (seconds-to-time (string-to-number date)))
                                 (pcase-let ((`(,cnt ,unit) (magit--age date t)))
                                   (format " (%d%c)" cnt unit)))
                              (pcase-let* ((abbr (eq style 'age-abbreviated))
                                           (`(,cnt ,unit) (magit--age date abbr)))
                                (format (format (if abbr "%%2d%%-%dc" "%%2d %%-%ds")
                                                (- width (if details (1+ details-width) 0)))
                                        cnt unit)))
                            'magit-log-date))))))

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
  :hook (magit-mode . magit-delta-mode))

;; TODO/FIXME/NOTE items from the repo as a section in magit status.
;; Keywords and colors come from hl-todo (configured in +ui.el).
(use-package! magit-todos
  :after magit
  :config
  (setq magit-todos-insert-after '(bottom)   ; last section, below recent commits
        magit-todos-max-items 15
        magit-todos-exclude-globs '(".git/" "node_modules/" "vendor/" "vendors/"
                                    "dist/" "*.min.js" "*.min.css" "*.map"))
  (magit-todos-mode 1))
