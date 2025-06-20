;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Berkeley Mono Variable"
                          :size 20
                          :weight 'normal
                          :width 'normal))

;; Set the big font variant
(setq doom-big-font (font-spec :family "Berkeley Mono Variable"
                               :size 36
                               :weight 'bold))

;; Set the bold font variant
(setq doom-font-bold (font-spec :family "Berkeley Mono Variable"
                               :size 20
                               :weight 'bold
                               :width 'normal))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Set up project directories
(after! projectile
  ;; Add your main projects directory
  (projectile-add-known-project "~/.local/share/chezmoi/")
  (setq projectile-project-search-path '("~/workspace/www/" "/Volumes/develop/workspace/www"))

  ;; Discover projects recursively (up to 3 levels deep)
  (setq projectile-project-search-path-function
        (lambda (dir)
          (let ((depth 3))
            (mapcar (lambda (x) (cons x depth))
                    (list dir)))))

  ;; Force projectile to use the native indexing method
  (setq projectile-indexing-method 'native)

  ;; Enable caching for better performance
  (setq projectile-enable-caching t)

  ;; ;; Remove Makefile from projectile-project-root-files-top-down-recurring
  ;; (when (boundp 'projectile-project-root-files-top-down-recurring)
  ;;   (setq projectile-project-root-files-top-down-recurring
  ;;         (remove "Makefile" projectile-project-root-files-top-down-recurring)))
)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
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

;; ===========================================================================
;; Load external configuration files
(load! "keymaps")
