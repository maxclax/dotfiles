;;; init-ollama-buddy.el -*- lexical-binding: t; -*-
;; Ollama Buddy - Local LLM integration with web search support
;; https://github.com/captainflasmr/ollama-buddy

;; API key is read from ~/.authinfo (managed by chezmoi)
;; Add to .authinfo.gpg:
;;   machine ollama.com login ollama password YOUR_API_KEY

(use-package! ollama-buddy
  :ensure t
  :config
  ;; Default model for chat
  (setq ollama-buddy-default-model "qwen3.5:cloud"))

(provide 'init-ollama-buddy)
