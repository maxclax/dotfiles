;;; +prog.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! compilation-mode #'visual-line-mode)

;; Fix for makefile compilation buffer name error
(after! compile
  ;; Ensure compilation-buffer-name-function handles nil gracefully
  (setq compilation-buffer-name-function
        (lambda (name-of-mode)
          (if (stringp name-of-mode)
              (format "*%s*" name-of-mode)
            "*compilation*"))))

;; Fix for makefile-executor compilation issues
(after! makefile-executor
  ;; Override the problematic buffer naming function
  (defun makefile-executor--compilation-buffer-name (target)
    "Generate compilation buffer name for TARGET."
    (if (and target (stringp target))
        (format "*makefile: %s*" target)
      "*makefile*")))

(use-package! format-all
  ;; :hook (emacs-lisp-mode . format-all-mode)
  :defer t)


(use-package! which-func
  :defer t
  :commands which-function)

(use-package restclient
  :defer t
  :mode (("\\.http\\'" . restclient-mode)))

;; Corfu completion configuration (replacing company)
(after! corfu
  ;; Enable corfu globally
  (global-corfu-mode)
  ;; Ensure LSP completion works with corfu
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-separator ?_))


(use-package! graphql-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.graphqls\\'" . graphql-mode)))


(use-package! protobuf-mode
  :defer t)

(use-package! jinja2-mode
  :mode "steps\\.txt\\'"
  :mode "preflight_steps\\.txt\\'"
  :defer t)

(use-package! gn-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.gni?\\'" . gn-mode)))

(add-hook! 'go-mode-hook (setq-local format-all-formatters '(("Go" gofmt))))

(add-hook! 'json-mode-hook (setq-local format-all-formatters '(("JSON" prettier))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JS, WEB
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook! '(web-mode-hook html-mode-hook) (setq-local format-all-formatters '(("HTML" prettier))))
(add-hook! 'typescript-mode-hook (setq-local format-all-formatters '(("TypeScript" prettier))))
(add-hook! 'rjsx-mode-hook (setq-local format-all-formatters '(("JavaScript" prettier))))

(after! web-mode
  (web-mode-toggle-current-element-highlight)
  (web-mode-dom-errors-show))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; JAVA
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG & RUN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! quickrun
  (quickrun-add-command "c++/c1z"
    '((:command . "clang++")
      (:exec    . ("%c -std=c++1z %o -o %e %s"
                   "%e %a"))
      (:remove  . ("%e")))
    :default "c++"))

(when (modulep! :tools debugger)
  (defun +my/dape-breakpoint-toggle ()
    (interactive)
    (require 'dape)
    (dape-breakpoint-toggle)
    (+go/write-project-breakpoints))

  (defun +my/dape-breakpoint-remove-all ()
    (interactive)
    (require 'dape)
    (dape-breakpoint-remove-all)
    (+go/write-project-breakpoints))

  (map! :leader
        (:prefix ("d" . "debug")
         :desc "dape breakpoint toggle" "b" #'+my/dape-breakpoint-toggle
         :desc "dape breakpoint remove all" "B" #'+my/dape-breakpoint-remove-all
         ))

  (after! dape
    (setq dape-configs (assq-delete-all 'dlv dape-configs))
    (add-to-list 'dape-configs
                 `(delve
                   modes (go-mode go-ts-mode)
                   ensure dape-ensure-command
                   fn (dape-config-autoport dape-config-tramp)
                   command "dlv"
                   command-args ("dap" "--listen" "127.0.0.1::autoport")
                   command-insert-stderr t
                   command-cwd (lambda()(if (string-suffix-p "_test.go" (buffer-name))
                                            default-directory (dape-cwd)))
                   port :autoport
                   :type "debug"
                   :request "launch"
                   :mode (lambda() (if (string-suffix-p "_test.go" (buffer-name)) "test" "debug"))
                   :program "."
                   :cwd "."
                   :args (lambda()
                           (if (string-suffix-p "_test.go" (buffer-name))
                               (save-excursion
                                 (when (re-search-backward "^func[ \t]+\\(\\(\\w\\|\\s_\\)+\\)" nil t)
                                   (let* ((test-name (match-string 1))
                                          (test-regexp (concat "^" test-name "$")))
                                     `["-test.run" ,test-regexp])))
                             []))))
    ))


(add-hook! 'go-mode-hook (add-hook! 'after-save-hook :local #'+go/write-project-breakpoints))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LANGUAGE CUSTOMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-generic-mode sxhkd-mode
  '(?#)
  '("alt" "Escape" "super" "bspc" "ctrl" "space" "shift") nil
  '("sxhkdrc") nil
  "Simple mode for sxhkdrc files.")
