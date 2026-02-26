;;; init-minuet.el -*- mode: emacs-lisp; lexical-binding: t; -*-

(use-package! minuet
  :commands (minuet-show-suggestion minuet-complete-with-minibuffer)
  :config
  (setq minuet-provider 'openai-fim-compatible)
  (setq minuet-n-completions 1)         ; 1 is enough for cloud, faster
  (setq minuet-request-timeout 5)       ; cloud may need more time
  (setq minuet-auto-suggestion-debounce-delay 1.0)
  (setq minuet-show-error-message-on-minibuffer t) ; show errors for debugging

  (plist-put minuet-openai-fim-compatible-options :end-point "http://localhost:11434/v1/completions")
  (plist-put minuet-openai-fim-compatible-options :model "qwen3-coder-next:cloud")
  (plist-put minuet-openai-fim-compatible-options :api-key "TERM") ; Ollama needs any existing env var
  (minuet-set-optional-options minuet-openai-fim-compatible-options :max_tokens 256)

  ;; Auto-enable in code buffers
  (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode))

(provide 'init-minuet)
