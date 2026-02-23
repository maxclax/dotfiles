;;; init-checker.el -*- lexical-binding: t; -*-

;; Spell checking is handled by cspell via flymake-cspell, not flyspell/aspell.
(after! flyspell
  (remove-hook 'text-mode-hook #'flyspell-mode)
  (remove-hook 'org-mode-hook #'flyspell-mode)
  (remove-hook 'markdown-mode-hook #'flyspell-mode))

;; Disable flymake by default — enable manually per buffer with C-c t f
(after! flymake
  (remove-hook 'prog-mode-hook #'flymake-mode)
  (remove-hook 'text-mode-hook #'flymake-mode)
  (add-hook 'prog-mode-hook (lambda () (flymake-mode -1)))
  (add-hook 'text-mode-hook (lambda () (flymake-mode -1))))

;; Suppress warning/note fringe indicators — only show errors
(setq flymake-margin-indicators-string
      '((error "!" compilation-error)
        (warning "" compilation-warning)
        (note "" compilation-info)))

;; cspell integration (requires cspell in PATH)
(use-package! flymake-cspell
  :when (executable-find "cspell")
  :hook (prog-mode . flymake-cspell-setup))

(defvar my/cspell-config-file (expand-file-name "~/.config/cspell/cspell.json"))

(defun my/cspell-check-buffer ()
  "Run cspell on the current buffer file."
  (interactive)
  (if (executable-find "cspell")
      (compilation-start
       (string-join `("cspell" "--config" ,my/cspell-config-file
                      ,(shell-quote-argument (buffer-file-name)))
                    " ")
       'grep-mode)
    (message "cspell not found in PATH")))

(defun my/cspell-check-diff ()
  "Run cspell on all files changed since HEAD."
  (interactive)
  (if (executable-find "cspell")
      (let ((default-directory (doom-project-root)))
        (compilation-start
         (string-join `("git diff --name-only HEAD | xargs -I{} cspell"
                        "--config" ,my/cspell-config-file "{}")
                      " ")
         'grep-mode))
    (message "cspell not found in PATH")))
