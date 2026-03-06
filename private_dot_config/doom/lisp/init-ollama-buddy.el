;;; init-ollama-buddy.el -*- lexical-binding: t; -*-
;; Ollama Buddy - Local LLM integration
;; https://github.com/captainflasmr/ollama-buddy

;; API keys read from ~/.authinfo (managed by chezmoi/1Password):
;;   machine api.anthropic.com login apikey password <KEY>
;;   machine api.openai.com login apikey password <KEY>
;;   machine generativelanguage.googleapis.com login apikey password <KEY>
;;   machine api.deepseek.com login apikey password <KEY>
;;   machine ollama.com login apikey password <KEY>

(use-package! ollama-buddy
  :ensure t
  :config
  ;; Default model — Ollama Cloud
  (setq ollama-buddy-default-model "qwen3.5:cloud")

  ;; Suppress the verbose welcome screen
  (setq ollama-buddy-full-welcome-enabled nil)

  ;; ── Cloud providers ──────────────────────────────────────────────────────

  (setq ollama-buddy-claude-api-key
        (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey"))
  (require 'ollama-buddy-claude nil t)

  (setq ollama-buddy-openai-api-key
        (auth-source-pick-first-password :host "api.openai.com" :user "apikey"))
  (require 'ollama-buddy-openai nil t)

  (setq ollama-buddy-gemini-api-key
        (auth-source-pick-first-password :host "generativelanguage.googleapis.com" :user "apikey"))
  (require 'ollama-buddy-gemini nil t)

  (setq ollama-buddy-deepseek-api-key
        (auth-source-pick-first-password :host "api.deepseek.com" :user "apikey"))
  (require 'ollama-buddy-deepseek nil t)

  ;; Ollama Cloud session token
  (setq ollama-buddy-cloud-session-token
        (auth-source-pick-first-password :host "ollama.com" :user "apikey"))

  ;; ── Per-command model assignments ────────────────────────────────────────
  (with-eval-after-load 'ollama-buddy
    (ollama-buddy-update-menu-entry 'git-commit        :model "qwen3.5:cloud")
    (ollama-buddy-update-menu-entry 'proofread         :model "qwen3.5:cloud")
    (ollama-buddy-update-menu-entry 'describe-code     :model "qwen3.5:cloud")
    (ollama-buddy-update-menu-entry 'refactor-code     :model "qwen3.5:cloud")
    (ollama-buddy-update-menu-entry 'dictionary-lookup :model "qwen3.5:cloud")
    (ollama-buddy-update-menu-entry 'synonym           :model "qwen3.5:cloud"))

  ;; ── Dired: attach marked files to a conversation ─────────────────────────
  (with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "C-c C-a") #'ollama-buddy-dired-attach-marked-files)))

(provide 'init-ollama-buddy)
