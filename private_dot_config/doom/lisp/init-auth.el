;;; init-auth.el -*- lexical-binding: t; -*-
;; Central credential lookup (auth-source) for ALL of Emacs — gptel, TRAMP,
;; forge, smtp, … 1Password answers first via the `op' CLI: one 1P item per
;; host with a field per login (e.g. item "api.anthropic.com" with field
;; "apikey"). Secrets never touch disk; first use per session prompts the
;; 1Password unlock. ~/.authinfo remains as a fallback for entries not (yet)
;; migrated — hosts with odd names (slashes) can stay there permanently.

(use-package! auth-source-1password
  :config
  (setq auth-source-1password-vault "Private")
  (auth-source-1password-enable))

(provide 'init-auth)
