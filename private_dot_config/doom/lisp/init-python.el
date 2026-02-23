;;; init-python.el -*- lexical-binding: t; -*-

;; Basic Python settings
(after! python
  (setq python-indent-offset 4
        python-shell-interpreter "python3"))

;; Use ruff as the formatter (ruff also handles import sorting via `I` rules)
(add-hook! 'python-mode-hook
  (setq-local format-all-formatters '(("Python" ruff))))

;; Highlight breakpoints on open
(add-hook 'python-mode-hook    #'my/python-annotate-pdb)
(add-hook 'python-ts-mode-hook #'my/python-annotate-pdb)


;; Pyright LSP configuration
(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd "python3"
        lsp-pyright-multi-root nil
        lsp-pyright-type-checking-mode "basic"
        lsp-pyright-auto-import-completions t
        lsp-pyright-disable-organize-imports nil
        lsp-pyright-venv-path ""))
