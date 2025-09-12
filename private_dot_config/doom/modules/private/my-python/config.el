;;; private/my-python/config.el -*- lexical-binding: t; -*-

(map!
 (:after python
  :localleader
  :map python-mode-map
  :desc "Insert breakpoint" "b" #'+python/toggle-breakpoint
  :desc "Insert default breakpoint" "B" #'+python/toggle-default-breakpoint
  :desc "Toggle debugpy lines" "d" #'+python/toggle-debugpy-lines
  (:prefix "t"
   :desc "Copy python breakpoint" "b" #'+python/copy-pdb-breakpoint-of-current-line
   :desc "Copy python cmd" "p" #'+python/copy-python-cmd
   :desc "Copy pytest cmd" "y" #'+python/copy-pytest-cmd
   :desc "Copy unittest cmd" "u" #'+python/copy-unittest-cmd)
  (:prefix ("i" . "Import")
   :desc "Remove unused impoorts" "r" #'+python/autoflake-remove-imports
   :desc "Isort buffer"    "s" #'python-isort-autosave-mode
   :desc "Insert copied import" "p" #'+python/insert-temp-import
   :desc "Copy module import " "i" #'+python/yank-module-import)
  (:prefix ("v" . "ENV")
   "c" #'conda-env-activate
   "C" #'conda-env-deactivate
   :desc "Activate .venv" "p" (λ! (pyvenv-activate (concat (doom-project-root) ".venv")))
   ;; deactivate not work well. Just try activate again.
   "P" #'pyvenv-deactivate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! python
  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        pippel-python-command "python3"
        importmagic-python-interpreter "python3"
        flycheck-python-pylint-executable "pylint"
        flycheck-python-flake8-executable "flake8"))

(add-hook! 'python-mode-hook #'+python/annotate-pdb)
(add-hook! 'python-mode-hook (setq-local format-all-formatters '(("Python" ruff))))

;; Remove custom completion hook - let Doom handle LSP + corfu integration

(after! lsp-pyright
  (setq lsp-pyright-python-executable-cmd "python3")
  ;; Set Pyright as highest priority Python LSP server
  (set-lsp-priority! 'pyright 1)
  ;; Ensure pyright uses correct workspace root
  (setq lsp-pyright-multi-root nil)
  ;; Disable other Python LSP clients to prevent conflicts  
  (setq lsp-disabled-clients '(pylsp pyls mspyls))
  ;; Ensure single workspace per project
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 2000)
  
  ;; Python-specific completion settings
  (setq lsp-pyright-disable-organize-imports nil)
  (setq lsp-pyright-auto-import-completions t)
  (setq lsp-pyright-auto-search-paths t)
  
  ;; Ensure pyright provides completion capabilities
  (setq lsp-pyright-disable-language-services nil)
  (setq lsp-pyright-disable-tag-complete nil))

(use-package! py-isort
  :defer t
  :init
  (defvar my-enable-isort-before-save t)
  (defun my-python-isrot-before-save ()
    (when my-enable-isort-before-save
      (py-isort-before-save)))
  (define-minor-mode python-isort-autosave-mode
    "Isort autosave mode."
    :lighter " Isort"
    :global nil
    (when (not (derived-mode-p 'python-mode))
      (error "Isort only works with Python buffers"))
    (if python-isort-autosave-mode
        (add-hook! 'before-save-hook :local #'my-python-isrot-before-save)
      (remove-hook! 'before-save-hook :local #'my-python-isrot-before-save)))

  ;; (add-hook! 'python-mode-hook #'python-isort-autosave-mode)
  )


;; (use-package! importmagic
;;   :defer t
;;   :hook (python-mode . importmagic-mode)
;;   :commands (importmagic-fix-imports importmagic-fix-symbol-at-point)
;;   :config
;;   (dolist (func '(importmagic-fix-imports importmagic-fix-symbol-at-point))
;;     (advice-add func :before #'revert-buffer-no-confirm)))


(after! pipenv
  (setq pipenv-with-projectile t)
  ;; Override pipenv--clean-response to trim color codes
  (defun pipenv--clean-response (response)
    "Clean up RESPONSE from shell command."
    (replace-regexp-in-string "\n\[0m$" "" (s-chomp response)))

  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pipenv-activate pipenv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))


(after! conda
  (setq conda-anaconda-home (cond ((file-directory-p "~/.miniconda") "~/.miniconda")
                                  ((file-directory-p "~/.anaconda") "~/.anaconda")))
  (when (file-exists-p! "~/.conda")
    (setq conda-env-home-directory (expand-file-name "~/.conda")))
  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(conda-env-activate conda-env-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

(after! pyvenv
  ;; restart flycheck-mode after env activate and deactivate
  (dolist (func '(pyvenv-activate pyvenv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

(after! poetry
  (remove-hook 'python-mode-hook #'poetry-tracking-mode)
  (dolist (func '(poetry-venv-workon poetry-venv-deactivate))
    (progn
      (when (modulep! :checkers syntax)
        (advice-add func :after #'reset-flycheck))
      (advice-add func :after #'+lsp/restart))))

;; For pytest-mode
(set-evil-initial-state! '(comint-mode) 'normal)
(set-evil-initial-state! '(shell-mode) 'normal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DAPE (Debug Adapter Protocol for Emacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (modulep! :tools debugger)
  (after! dape
    (add-to-list 'dape-configs
                 `(debugpy-remote
                   modes (python-mode python-ts-mode)
                   host "localhost"
                   port 5678
                   :request "attach"
                   :type "python"
                   :pathMappings [(:localRoot ,(lambda () (projectile-project-root)) :remoteRoot "/usr/src/app")]
                   :justMyCode t
                   :showReturnValue t
                   :console "integratedTerminal"
                   :name "Debugpy Remote"))))
