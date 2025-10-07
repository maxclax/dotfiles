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
        
        ;; Speed up status buffer but keep auto-refresh
        magit-refresh-status-buffer t
        magit-refresh-verbose nil
        
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
  (setq magit-delta-default-light-theme "GitHub")
  (setq magit-delta-default-dark-theme "OneHalfDark")
  )

(use-package! magit-todos
  :after magit
  :config
  (magit-todos-mode 1)

  (setq magit-todos-max-items 15          ; Limit displayed items for better performance
        magit-todos-exclude-globs
        '("node_modules/**/*"              ; JavaScript dependencies
          "static/**/*"                    ; Static assets
          "build/**/*"                     ; Build artifacts
          "dist/**/*"                      ; Distribution files
          "_dist/**/*"                      ; Distribution files
          "target/**/*"                    ; Rust/Java build directory
          ".git/**/*"                      ; Git internals
          "vendor/**/*"                    ; Go/PHP dependencies
          "venv/**/*"                      ; Python virtual environment
          ".venv/**/*"                     ; Python virtual environment alt
          "__pycache__/**/*"               ; Python cache
          "*.min.js"                       ; Minified JavaScript
          "*.bundle.js"                    ; Bundled JavaScript
          "coverage/**/*"                  ; Test coverage
          ".next/**/*"                     ; Next.js build
          ".nuxt/**/*"                     ; Nuxt.js build
          "public/assets/**/*"             ; Public assets
          "assets/**/*"                    ; General assets
          "*.log"                          ; Log files
          "*.tmp"                          ; Temporary files
          ".sass-cache/**/*"               ; Sass cache
          "bower_components/**/*"          ; Bower components
          "*.map"                          ; Source maps
          ".nyc_output/**/*"               ; NYC coverage
          "tmp/**/*"                       ; Temporary directory
          "temp/**/*"                      ; Temporary directory alt
          ".idea/**/*"                     ; IntelliJ IDEA
          ".vscode/**/*"                   ; VS Code
          "*.class"                        ; Java compiled classes
          "*.jar"                          ; Java archives
          "*.war"                          ; Java web archives
          "*.pyc"                          ; Python compiled
          "*.pyo"                          ; Python optimized
          ".tox/**/*"                      ; Python tox
          ".pytest_cache/**/*"             ; Pytest cache
          "elm-stuff/**/*"                 ; Elm dependencies
          ".stack-work/**/*"               ; Haskell Stack
          "_build/**/*"                    ; OCaml/Erlang build
          ".eunit/**/*"                    ; Erlang eunit
          "deps/**/*"                      ; Erlang/Elixir dependencies
          "_rel/**/*"                      ; Erlang releases
          ".cargo/**/*"                    ; Rust cargo cache
          "Cargo.lock"                     ; Rust lock file
          "package-lock.json"              ; NPM lock file
          "yarn.lock"                      ; Yarn lock file
          "*.orig"                         ; Git merge backup files

          "migrations/**/*"                ; Django migrations
          "staticfiles/**/*"               ; Django collected static
          "media/**/*"                     ; Django media files
          "locale/**/*"                    ; Django locale files
          ".pytest_cache/**/*"             ; Pytest cache
          ".coverage"                      ; Coverage data file
          "htmlcov/**/*"                   ; Coverage HTML reports
          "*.egg-info/**/*"                ; Python egg info
          ".mypy_cache/**/*"               ; MyPy cache
          ".ruff_cache/**/*"               ; Ruff cache
          "HUGO/**/*"                      ; Hugo files
          "_dist/**/*"                     ; Distribution directory
          "public/**/*"                    ; Hugo public output
          "resources/**/*"                 ; Hugo resources
          ".hugo_build.lock"               ; Hugo build lock
          ".cache/**/*"                    ; General cache
          ".parcel-cache/**/*"             ; Parcel cache
          ".turbo/**/*"                    ; Turborepo cache
          ".vercel/**/*"                   ; Vercel cache
          )))

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
