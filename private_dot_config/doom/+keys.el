;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'meta
        mac-command-modifier 'super))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +my/setup-clean-c-c-bindings ()
  ;; Also clear in override map to prevent conflicts
  (when (boundp 'general-override-mode-map)
    (define-key general-override-mode-map (kbd "C-c n") (make-sparse-keymap)))

  ;; Apply your custom C-c bindings with highest priority
  (map!
   (:map override
         "C-c a"     #'org-agenda
         "C-c x"     #'org-capture

         "C-c b f"   #'format-all-buffer

         "C-c F j"   #'dirvish-dwim
         "C-c F n"   #'+default/yank-filename

         "C-c g A"   #'+my/smart-commit
         "C-c g a"   #'+my/commit-with-ai

         "C-c i t D" #'org-download-delete
         "C-c i t d" #'my/open-templates-directory
         "C-c i t f" #'my/org-attach-file-and-insert-link
         "C-c i t F" #'my/org-attach-multiple-files-and-insert-links
         "C-c i t i" #'my/insert-template
         "C-c i t s" #'org-download-screenshot
         "C-c i t u" #'org-download-image
         "C-c i t y" #'org-download-clipboard

         "C-c l p l" #'list-processes
         "C-c l p p" #'prodigy
         "C-c l s c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply"))
         "C-c l t"   #'hl-todo-insert
         "C-c l x"   #'align-regexp

         "C-c n b"   #'denote-backlinks
         "C-c n d"   #'denote-dired
         "C-c n g"   #'denote-grep
         "C-c n j"   #'denote-journal-new-or-existing-entry
         "C-c n l"   #'denote-link
         "C-c n n"   #'denote
         "C-c n r"   #'denote-rename-file
         "C-c n S"   #'consult-notes-search-in-all-notes
         "C-c n s"   #'consult-notes

         ;; "C-c o c"   #'org-clock-in-last
         ;; "C-c o i"   #'org-clock-in
         ;; "C-c o j"   #'org-clock-goto
         ;; "C-c o o"   #'org-clock-out

         "C-c O x"   #'link-hint-open-link
         "C-c O X"   #'link-hint-open-link-at-point

         "C-c p t i" #'hl-todo-insert
         "C-c p t l" #'magit-todos-list
         "C-c p t t" (lambda () (interactive)
                        (if (projectile-project-p)
                            (consult-ripgrep (projectile-project-root) "TODO")
                          (message "Not in a project")))
         "C-c p t T" (lambda () (interactive)
                        (if (projectile-project-p)
                            (consult-ripgrep (projectile-project-root) "TODO\\|HACK\\|TEMP\\|DONE\\|NOTE\\|DONT\\|DEBUG\\|FAIL\\|FIXME")
                          (message "Not in a project")))
         "C-c p t u" #'magit-todos-update

         "C-c t K"   #'keycast-log-mode
         "C-c t k"   #'keycast-header-line-mode
         "C-c t t p" #'pomm-pause
         "C-c t t r" #'pomm-reset
         "C-c t t s" #'pomm-start
         "C-c t t S" #'pomm-stop
         "C-c t t t" #'pomm)))


;; Apply after Doom finishes loading (for startup)
(add-hook 'doom-first-buffer-hook #'+my/setup-clean-c-c-bindings)
