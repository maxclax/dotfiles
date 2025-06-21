;;; +misc.el -*- lexical-binding: t; -*-

(use-package! screenshot
  :defer t)

(after! centaur-tabs
  (centaur-tabs-group-by-projectile-project))

(use-package consult-todo
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LOG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! keycast
  :defer t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This package provides the g~ operator to transform CamelCase words into snake_case. You can customize the binding.
;; Try using g~io
(use-package! evil-string-inflection :after evil)


(use-package! tmux-pane
  :unless (display-graphic-p)
  :defer t
  :commands (tmux-pane--windmove)
  :hook (after-init . my-tmux-pane-mode)
  :init
  (defvar my-tmux-pane-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-t k")
                  (lambda () (interactive) (tmux-pane--windmove "up"  "-U")))
      (define-key map (kbd "C-t j")
                  (lambda () (interactive) (tmux-pane--windmove "down"  "-D")))
      (define-key map (kbd "C-t h")
                  (lambda () (interactive) (tmux-pane--windmove "left" "-L")))
      (define-key map (kbd "C-t l")
                  (lambda () (interactive) (tmux-pane--windmove "right" "-R")))
      (define-key map (kbd "C-t C-k")
                  (lambda () (interactive) (tmux-pane--windmove "up"  "-U")))
      (define-key map (kbd "C-t C-j")
                  (lambda () (interactive) (tmux-pane--windmove "down"  "-D")))
      (define-key map (kbd "C-t C-h")
                  (lambda () (interactive) (tmux-pane--windmove "left" "-L")))
      (define-key map (kbd "C-t C-l")
                  (lambda () (interactive) (tmux-pane--windmove "right" "-R")))
      map))

  (define-minor-mode my-tmux-pane-mode
    "Seamlessly navigate between tmux pane and emacs window"
    :init-value nil
    :global t
    :keymap 'my-tmux-pane-mode-map))


(use-package! imenu-list
  :defer t
  :config
  (set-popup-rules! '(("^\\*Ilist\\*" :side right :size 40 :select t))))


(add-hook! 'better-jumper-post-jump-hook #'recenter)

(after! nav-flash
  (defun +advice/nav-flash-show (orig-fn &rest args)
    (ignore-errors (apply orig-fn args)))
  (advice-add 'nav-flash-show :around #'+advice/nav-flash-show))

(after! dirvish
  (setq dirvish-attributes
        '(vc-state file-size nerd-icons collapse subtree-state file-time))
  (setq dirvish-quick-access-entries
        `(("h" "~/" "Home")
          ("c" "~/.config" "config")
          ("D" "~/Downloads" "Downloads")
          ("e" ,doom-user-dir "Doom directory")
          ("E" ,doom-emacs-dir "Emacs directory")
          ))

  (setq dirvish-hide-details '(dired dirvish dirvish-side)
        dirvish-hide-cursor '(dired dirvish dirvish-side))

  (when (executable-find "lla")
    (dirvish-define-preview lla (file)
      "Use `lla' to generate directory preview."
      :require ("lla")                  ; tell Dirvish to check if we have the executable
      (when (file-directory-p file)     ; we only interest in directories here
        `(shell . ("lla" "-l" "--icons" ,file))))

    (add-to-list 'dirvish-preview-dispatchers 'lla))

  (defun dirvish-copy-file-relative-path (&optional multi-line)
    "Copy filepath of marked files.
If MULTI-LINE, make every path occupy a new line."
    (interactive "P")
    (let* ((files (mapcar (lambda (file)
                            (file-relative-name (file-local-name file)))
                          (dired-get-marked-files)))
           (names (mapconcat #'concat files (if multi-line "\n" " "))))
      (dirvish--kill-and-echo (if multi-line (concat "\n" names) names)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRANSLATE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package! go-translate
  :defer t
  :config
  (setq gts-translate-list '(("en" "zh-CN")))
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-google-engine) (gts-google-rpc-engine))
         :render (gts-buffer-render)))
  ;; For China user
  ;; (setq go-translate-base-url "https://translate.google.cn")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPLETION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! corfu
  (setq +corfu-want-ret-to-confirm "both")
  )

(when (modulep! :completion vertico)
  (setq vertico-posframe-border-width 3)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-bottom-center)

  ;; Fix jump issue for vertico, https://github.com/hlissner/doom-emacs/issues/5386
  (dolist (func '(+default/search-project))
    (advice-add func :around #'doom-set-jump-a)))

(when (modulep! :completion ivy)
  (after! (:and ivy ivy-prescient)
    (setq ivy-prescient-retain-classic-highlighting t))

  (after! ivy-posframe
    (setq ivy-posframe-border-width 3)

    ;; Use minibuffer to display ivy functions
    (dolist (fn '(+ivy/switch-workspace-buffer
                  ivy-switch-buffer))
      (setf (alist-get fn ivy-posframe-display-functions-alist) #'ivy-display-function-fallback)))

  (after! ivy-rich
    (plist-put! ivy-rich-display-transformers-list
                'ivy-switch-buffer
                '(:columns
                  ((ivy-switch-buffer-transformer (:width 60))
                   (ivy-rich-switch-buffer-size (:width 7))
                   (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
                   (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
                   (ivy-rich-switch-buffer-project (:width 15 :face success))
                   (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
                  :predicate
                  (lambda (cand) (get-buffer cand)))))

  (after! counsel
    ;; counsel-rg-base-command is configurable
    (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
          counsel-describe-function-function 'helpful-callable
          counsel-describe-variable-function 'helpful-variable)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TERM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! eshell
  ;; eshell-mode imenu index
  (add-hook! 'eshell-mode-hook (setq-local imenu-generic-expression '(("Prompt" " λ \\(.*\\)" 1))))

  (defun eshell/l (&rest args) (eshell/ls "-l" args))
  (defun eshell/e (file) (find-file file))
  (defun eshell/md (dir) (eshell/mkdir dir) (eshell/cd dir))
  (defun eshell/ft (&optional arg) (treemacs arg))

  (defun eshell/up (&optional pattern)
    (let ((p (locate-dominating-file
              (f-parent default-directory)
              (lambda (p)
                (if pattern
                    (string-match-p pattern (f-base p))
                  t)))
             ))
      (eshell/pushd p)))
  )


(after! term
  ;; term-mode imenu index
  (add-hook! 'term-mode-hook (setq-local imenu-generic-expression '(("Prompt" "➜\\(.*\\)" 1)))))
