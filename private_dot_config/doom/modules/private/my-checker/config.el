;;; private/my-checker/config.el -*- lexical-binding: t; -*-

;; Disable warning and note left fringe indicator
(setq flymake-margin-indicators-string '((error "!" compilation-error)
                                         (warning "" compilation-warning)
                                         (note "" compilation-info)))

(when (modulep! :checkers syntax))
(map! :leader
        (:prefix-map ("e" . "error")
        :desc "Cspell check all changed files" "c" #'cspell-check-diff-from-HEAD
        :desc "Cspell check buffer"    "C" #'cspell-check-buffer
        :desc "Next error"      "n" #'flymake-goto-next-error
        :desc "Previous error"  "p" #'flymake-goto-prev-error
        :desc "Explain error"   "e" #'flymake-show-diagnostic
        :desc "List errors"     "l" #'flymake-show-buffer-diagnostics
        :desc "Lsp list errors" "L" #'consult-flymake
        :desc "List project error" "P" #'flymake-show-project-diagnostics
        :desc "Verify setup"    "v" #'flymake-running-backends))

;; Disable flymake by default - runs after Doom's flymake configuration
(after! flymake
  (remove-hook 'prog-mode-hook #'flymake-mode)
  (remove-hook 'text-mode-hook #'flymake-mode)
  ;; Add our own hook to ensure flymake stays disabled by default
  ;; Flymake can still be manually enabled via SPC t f (doom/toggle-flymake)
  (add-hook 'prog-mode-hook (lambda () (flymake-mode -1)))
  (add-hook 'text-mode-hook (lambda () (flymake-mode -1))))

(use-package flymake-cspell
  :when (executable-find "cspell")
  :hook (prog-mode . flymake-cspell-setup))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cspell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar cspell-base-program "cspell")
(defvar cspell-config-file-path (concat "'" (expand-file-name  "~/.config/cspell/cspell.json") "'"))
(defvar cspell-args (string-join `("--config" ,cspell-config-file-path) " "))
(defun cspell-check-buffer ()
  (interactive)
  (if cspell-base-program
      (let* ((file-name (concat "'" (file-name-nondirectory (buffer-file-name)) "'"))
             (command (string-join `(,cspell-base-program ,cspell-args ,file-name) " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g cspell`")
    ))

(defun cspell-check-diff-from-HEAD ()
  (interactive)
  (if cspell-base-program
      (let* ((default-directory (doom-project-root))
             (command (string-join `("git diff --name-only HEAD | xargs -I{}" ,cspell-base-program ,cspell-args "'{}'") " ")))
        (compilation-start command 'grep-mode))
    (message "Cannot find cspell, please install with `npm install -g cspell`")))
