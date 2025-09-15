;;; +git.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        
        ;; Speed up status buffer
        magit-refresh-status-buffer nil
        magit-refresh-verbose t
        
        ;; Reduce expensive operations
        magit-revision-show-gravatars nil
        magit-log-show-refname-after-summary t
        
        ;; Limit log entries for performance
        magit-log-auto-more nil
        magit-log-show-margin nil
        
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
          magit-insert-stashes             ; Show stashes
          magit-insert-unpushed-to-pushremote  ; Show unpushed commits
          magit-insert-unpushed-to-upstream    ; Show unpushed commits
          magit-insert-unpulled-from-pushremote ; Show unpulled commits
          magit-insert-unpulled-from-upstream   ; Show unpulled commits
          magit-insert-recent-commits          ; Show recent commits
          )
        
        ;; Show recent commits in log
        magit-log-section-commit-count 10
        
        ;; Show all branches in log by default
        magit-log-arguments '("--graph" "--color" "--decorate" "-n256")
        
        ;; Display branch info in headers
        magit-status-headers-hook
        '(magit-insert-error-header
          magit-insert-diff-filter-header
          magit-insert-head-branch-header
          magit-insert-upstream-branch-header
          magit-insert-push-branch-header
          magit-insert-tags-header)
        
        ;; Show branch information
        magit-status-show-hashes-in-headers t)

  ;; Add git-credential-manager-core support
  (add-hook 'magit-process-prompt-functions
            'magit-process-git-credential-manager-core)

  ;; fix magit prompt for midway auth
  (cl-callf2 append '("Kerberos authentication failed.  Password:")
    magit-process-password-prompt-regexps)

  (magit-wip-after-apply-mode -1)
  (magit-wip-before-change-mode -1))

(use-package! magit-delta
  :after magit
  :init
  (when (executable-find "delta")
    (add-hook! magit-mode #'magit-delta-mode))
  :config
  (setq magit-delta-default-light-theme "GitHub"
        magit-delta-default-dark-theme "OneHalfDark")
  )

(after! magit-todos
  (setq magit-todos-exclude-globs '("third-party/*" "third_party/*" "node_modules/*" "vendor/*" "*.log" "*.min.*")
        magit-todos-max-items 20  ; Limit number of todos shown
        magit-todos-depth 1       ; Only search top-level directories
        magit-todos-auto-group-items nil)) ; Disable grouping for speed


;; magit-todos uses hl-todo-keywords
(custom-theme-set-faces! doom-theme
  `(hl-todo :foreground ,(doom-color 'bg)))
(after! hl-todo
  (setq hl-todo-color-background t
        hl-todo-keyword-faces
        `(("TODO"  . ,(doom-color 'orange))
          ("HACK"  . ,(doom-color 'orange))
          ("TEMP"  . ,(doom-color 'orange))
          ("DONE"  . ,(doom-color 'green))
          ("NOTE"  . ,(doom-color 'green))
          ("DONT"  . ,(doom-color 'red))
          ("DEBUG"  . ,(doom-color 'red))
          ("FAIL"  . ,(doom-color 'red))
          ("FIXME" . ,(doom-color 'red))
          ("XXX"   . ,(doom-color 'blue))
          ("XXXX"  . ,(doom-color 'blue)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional Magit Performance Optimizations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Disable automatic refresh in magit buffers for better performance
(after! magit
  ;; Disable auto-refresh of magit buffers
  (remove-hook 'after-save-hook 'magit-after-save-refresh-status)
  (remove-hook 'server-switch-hook 'magit-commit-diff--reset-timer)
  
  ;; Speed up magit by reducing process calls
  (setq magit-process-popup-time 5  ; Show process buffer after 5 seconds
        magit-diff-refine-hunk nil  ; Don't highlight word-level changes
        magit-revision-insert-related-refs nil)) ; Don't insert related refs

;; Optimize git performance for magit
(setenv "GIT_PAGER" "")  ; Disable git pager

;; Keybinding to manually refresh when needed
(map! :after magit
      :map magit-mode-map
      "g r" #'magit-refresh)
