;;; +lsp.el -*- lexical-binding: t; -*-


;; Typescript
(setq lsp-clients-typescript-init-opts
      '(:importModuleSpecifierPreference "relative"))

;; Use format-all by default
(setq +format-with-lsp nil)

(setq +lsp-prompt-to-install-server 'quiet)

(after! lsp-mode
  (add-hook! 'lsp-help-mode-hook (visual-line-mode 1))
  
  ;; Force single workspace per project
  (setq lsp-auto-guess-root nil)
  (setq lsp-restart 'auto-restart)
  
  ;; Disable LSP diagnostics completely
  (setq lsp-diagnostics-provider :none)         ; Disable LSP diagnostics
  (setq lsp-eldoc-enable-hover nil)             ; Disable hover info
  (setq lsp-modeline-diagnostics-enable nil)    ; Disable modeline diagnostics
  (setq lsp-modeline-code-actions-enable t)     ; Disable code actions in modeline
  (setq lsp-signature-auto-activate nil)        ; Disable signature help
  (setq lsp-signature-render-documentation nil) ; Disable signature docs
  
  ;; Configure LSP completion for corfu
  (setq lsp-completion-provider :capf) ; Use completion-at-point-functions
  
  ;; Configure completion settings for better import suggestions
  (setq lsp-completion-enable t
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-completion-enable-additional-text-edit t  ; Enable automatic imports
        lsp-completion-show-label-description t      ; Show more detail
        lsp-completion-filter-on-incomplete t        ; Filter as you type
        lsp-completion-sort-initial-results t)       ; Sort results initially
  
  ;; Ensure completion triggers include common import scenarios
  (setq lsp-completion-trigger-kind 1)              ; Invoked completion
  (setq lsp-enable-snippet t)                      ; Enable snippets for completions

  (setq lsp-log-io nil
        lsp-file-watch-threshold 4000
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-icons-enable nil           ; Enable icons for better visual
        lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols)  ; Show more context
        lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method Enum Interface
                                       Function Variable Constant Struct Event Operator TypeParameter))

  ;; Improve breadcrumb appearance
  (setq lsp-headerline-breadcrumb-enable-symbol-numbers nil
        lsp-headerline-breadcrumb-enable-diagnostics nil)

  ;; Custom breadcrumb faces for better visibility
  (custom-set-faces!
   '(lsp-headerline-breadcrumb-symbols-face :foreground "#7c7c75" :weight bold)
   '(lsp-headerline-breadcrumb-path-face :foreground "#51afef")
   '(lsp-headerline-breadcrumb-separator-face :foreground "#5B6268"))

  (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
                 "[/\\\\]\\.mypy_cache\\'"
                 "[/\\\\]\\.pytest_cache\\'"
                 "[/\\\\]\\.cache\\'"
                 "[/\\\\]\\.clwb\\'"
                 "[/\\\\]\\.env\\'"
                 "[/\\\\]__pycache__\\'"
                 "[/\\\\]bazel-bin\\'"
                 "[/\\\\]bazel-code\\'"
                 "[/\\\\]bazel-genfiles\\'"
                 "[/\\\\]bazel-out\\'"
                 "[/\\\\]bazel-testlogs\\'"
                 "[/\\\\]third_party\\'"
                 "[/\\\\]third-party\\'"
                 "[/\\\\]buildtools\\'"
                 "[/\\\\]out\\'"
                 "[/\\\\]build\\'"
                 ))
    (push dir lsp-file-watch-ignored-directories))
  )

(after! lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-lens-enable nil
        lsp-ui-sideline-enable nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-height 15
        lsp-ui-doc-max-width 100))
