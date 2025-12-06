;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'super
        mac-command-modifier 'none))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +my/setup-clean-c-c-bindings ()
  ;; Clear specific prefixes in override map
  (when (boundp 'general-override-mode-map)
    (define-key general-override-mode-map (kbd "C-c n") (make-sparse-keymap))
    (define-key general-override-mode-map (kbd "C-c p") (make-sparse-keymap))
    (define-key general-override-mode-map (kbd "C-c o") (make-sparse-keymap)))

  ;; Apply your custom C-c bindings directly to global map
  (map!
   (:map override
         :desc "Org agenda" "C-c a" #'org-agenda
         :desc "Org capture" "C-c c" #'org-capture

         (:prefix ("C-c b" . "Buffers")
          :desc "Format buffer" "f" #'format-all-buffer)

         (:prefix ("C-c F" . "Files")
          :desc "Yank filename" "n" #'+default/yank-filename)

         (:prefix ("C-c g" . "Git")
          :desc "Generate AI commit (text)" "A" #'+my/smart-commit
          :desc "AI commit" "a" #'+my/commit-with-ai)

         (:prefix ("C-c i" . "Insert")
          (:prefix ("t" . "Templates")
           :desc "Delete image" "D" #'org-download-delete
           :desc "Templates directory" "d" #'my/open-templates-directory
           :desc "Attach file" "f" #'my/org-attach-file-and-insert-link
           :desc "Attach multiple files" "F" #'my/org-attach-multiple-files-and-insert-links
           :desc "Insert template" "i" #'my/insert-template
           :desc "Screenshot" "s" #'org-download-screenshot
           :desc "Download image" "u" #'org-download-image
           :desc "Clipboard image" "y" #'org-download-clipboard))

         (:prefix ("C-c l" . "Tools")
          (:prefix ("p" . "Process")
           :desc "List processes" "l" #'list-processes
           :desc "Prodigy" "p" #'prodigy)
          (:prefix ("s" . "Shell")
           :desc "Chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply")))
          :desc "Insert TODO" "t" #'hl-todo-insert
          :desc "Align regexp" "x" #'align-regexp)

         (:prefix ("C-c n" . "Notes")
          :desc "Denote backlinks" "b" #'denote-backlinks
          :desc "Denote link after creating" "c" #'denote-link-after-creating
          :desc "Denote dired" "d" #'denote-dired
          :desc "Denote grep" "g" #'denote-grep
          :desc "Denote journal" "j" #'denote-journal-new-or-existing-entry
          :desc "Open Journelly" "J" #'my/open-journelly
          :desc "Denote link" "l" #'denote-link
          :desc "New denote" "n" #'denote
          :desc "Denote rename" "r" #'denote-rename-file
          :desc "Search all notes" "S" #'consult-notes-search-in-all-notes
          :desc "Consult notes" "s" #'consult-notes)

         (:prefix ("C-c o" . "Open")
          :desc "Dired jump" "-" #'dired-jump
          :desc "Dirvish" "/" #'dirvish
          :desc "Dirvish sidebar" "p" #'dirvish-side
          :desc "Reveal in Finder" "O" #'+macos/reveal-project-in-finder
          :desc "Open link at point" "x" #'link-hint-open-link-at-point)

         (:prefix ("C-c p" . "Project")
          :desc "Search project" "s" #'+default/search-project
          :desc "Search symbol in notes" "." #'+default/search-notes-for-symbol-at-point
          :desc "Switch project" "p" #'projectile-switch-project
          :desc "Recent files" "e" #'projectile-recentf
          :desc "Find file" "f" #'projectile-find-file
          :desc "Find file other project" "F" #'doom/find-file-in-other-project
          :desc "Kill project buffers" "k" #'projectile-kill-buffers
          (:prefix ("t" . "TODOs")
           :desc "Insert TODO comment" "i" #'hl-todo-insert
           :desc "Search TODOs" "t" (lambda () (interactive)
                                       (if (projectile-project-p)
                                           (consult-ripgrep (projectile-project-root) "TODO:")
                                         (message "Not in a project")))))

         (:prefix ("C-c t" . "Toggles/Timing")
          (:prefix ("t" . "Timer")
           :desc "Pause timer" "p" #'pomm-pause
           :desc "Reset timer" "r" #'pomm-reset
           :desc "Start timer" "s" #'pomm-start
           :desc "Stop timer" "S" #'pomm-stop
           :desc "Pomm timer" "t" #'pomm)))))


;; Apply after Doom finishes loading (for startup)
(add-hook 'doom-first-buffer-hook #'+my/setup-clean-c-c-bindings)

;; Apply very late to override any packages that load after us
(add-hook 'window-setup-hook #'+my/setup-clean-c-c-bindings)

;; Also apply when switching buffers (in case mode-specific bindings interfere)
(add-hook 'buffer-list-update-functions
          (lambda () (+my/setup-clean-c-c-bindings)))
