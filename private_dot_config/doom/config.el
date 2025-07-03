;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+os")
(load! "+git")
(load! "+misc")
(load! "+text")
(load! "+prog")
(load! "+ui")
(load! "+keys")
(cond
 ((modulep! :tools lsp) (load! "+lsp")))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;;
;; https://blog.serghei.pl/posts/emacs-python-ide/
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
(with-eval-after-load 'yasnippet
  (yas-reload-all))

;; Only if you use `flymake-mode'.
(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Set LSP keymap prefix.
(setopt lsp-keymap-prefix "C-c l")

;; Shut down LSP server after close all buffers associated with the server.
(setopt lsp-keep-workspace-alive nil)

;; Configure LSP UI enhancements.
(setopt lsp-headerline-breadcrumb-segments
        '(path-up-to-project
          file
          symbols))

(with-eval-after-load 'lsp-ui
  ;; Remap `xref-find-definitions' (bound to M-. by default).
  (define-key lsp-ui-mode-map
              [remap xref-find-definitions]
              #'lsp-ui-peek-find-definitions)

  ;; Remap `xref-find-references' (bound to M-? by default).
  (define-key lsp-ui-mode-map
              [remap xref-find-references]
              #'lsp-ui-peek-find-references))

;; Configure LSP mode for enhanced experience.
(with-eval-after-load 'lsp-mode
  ;; Remap `lsp-treemacs-errors-list' (bound to C-c l g e).
  (define-key lsp-mode-map
              [remap lsp-treemacs-errors-list]
              #'consult-lsp-diagnostics)

  ;; Remap `xref-find-apropos' (bound to C-c l g a).
  (define-key lsp-mode-map
              [remap xref-find-apropos]
              #'consult-lsp-symbols)

  ;; Enable `which-key-mode' integration for LSP.
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Auto configure dap minor mode.
(setopt dap-auto-configure-mode t)

(defmacro company-backend-for-hook (hook backends)
  `(add-hook ,hook (lambda ()
                     (set (make-local-variable 'company-backends)
                          ,backends))))

(defun setup-python-environment ()
  "Setup a Python development environment in the current buffer."
  ;; Update the current buffer's environment.
  (envrc--update)

  ;; Enable YASnippet mode.
  (yas-minor-mode 1)

  ;; Setup active backends for `python-mode'.
  (company-backend-for-hook 'lsp-completion-mode-hook
                            '((company-capf :with company-yasnippet)
                              company-dabbrev-code))

  ;; Prevent `lsp-pyright' start in multi-root mode.
  ;; This must be set before the package is loaded.
  (setq-local lsp-pyright-multi-root nil)

  ;; Enable LSP support in Python buffers.
  (require 'lsp-pyright)
  (lsp-deferred)

  ;; Enable DAP support in Python buffers.
  (require 'dap-python)
  (setq-local dap-python-debugger 'debugpy)

  (dap-mode 1))

;; Configure hooks after `python-mode' is loaded.
(add-hook 'python-mode-hook #'setup-python-environment)

;; Setup buffer-local direnv integration for Emacs.
(when (executable-find "direnv")
  ;; `envrc-global-mode' should be enabled after other global minor modes,
  ;; since each prepends itself to various hooks.
  (add-hook 'after-init-hook #'envrc-global-mode))

;; Load system profile for different machines and work config
(dolist (config '("~/.config/doom/local.el"))
  (let ((config-file (file-truename config)))
    (when (file-exists-p config-file)
      (load-file config-file))))

