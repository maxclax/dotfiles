;; +keys.el -*- lexical-binding: t; -*-


(when IS-MAC
    (setq mac-option-modifier 'meta
          mac-right-option-modifier 'meta
          mac-command-modifier 'super))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Denote commands under C-c d prefix
(global-set-key (kbd "C-c d n") #'denote)                           ; Create note
(global-set-key (kbd "C-c d r") #'denote-rename-file)               ; Rename file
(global-set-key (kbd "C-c d l") #'denote-link)                      ; Link
(global-set-key (kbd "C-c d b") #'denote-backlinks)                 ; Backlinks
(global-set-key (kbd "C-c d d") #'denote-dired)                     ; Dired
(global-set-key (kbd "C-c d g") #'denote-grep)                      ; Grep
(global-set-key (kbd "C-c d s") #'consult-notes)                    ; Search notes
(global-set-key (kbd "C-c d S") #'consult-notes-search-in-all-notes) ; Search in all notes

;; Journal commands
(global-set-key (kbd "C-c d j") #'denote-journal-new-or-existing-entry) ; Today's journal

;; Template commands under C-c i t prefix
(global-set-key (kbd "C-c i t i") #'my/insert-template)           ; Insert template
(global-set-key (kbd "C-c i t d") #'my/open-templates-directory)  ; Open templates dir

;; ;; Buffers
;; (map! :leader
;;       :prefix "b"
;;       :desc "Format-all buffer"      "f" #'format-all-buffer)



;; ;; Files
;; (map! :leader
;;       :prefix "f"
;;       :desc "Yank filename" "n" #'+default/yank-filename
;;       :desc "Dirvish" "j" #'dirvish-dwim)


;; ;;
;; (map! :leader
;;       :prefix "g"
;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ;; GIT/MAGIT KEYBINDINGS - extends existing "g" git prefix
;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       (:prefix ("c" . "Create")
;;        :desc "Generate AI Commit Message" "A" #'+my/smart-commit
;;        :desc "Commit with AI Message" "a" #'+my/commit-with-ai))

;; ;; Open
;; (map! :leader
;;       :prefix "o"
;;       :desc "Open link" "x" #'link-hint-open-link
;;       :desc "Open link at point" "X" #'link-hint-open-link-at-point)

;; ;; (map! :leader
;; ;;       :prefix "i"
;; ;;       (:prefix ("t" . "templates")
;; ;;        :desc "Insert template" "i" #'my/insert-template
;; ;;        :desc "Open templates dir" "d" #'my/open-templates-directory))

;; ;; Project
;; (map! :leader
;;       :prefix "p"
;;       "n" #'+default/yank-project-name
;;       "*" (+my/prefix-M-x "projectile-")
;;       :desc "Update projectile list" "u" #'update-projectile-known-projects)

;; ;; Project TODOs - separate map block
;; (map! :leader
;;       :prefix "p"
;;       (:prefix ("t" . "TODOs")
;;        :desc "Search TODOs only" "t" (lambda () (interactive)
;;                                        (if (projectile-project-p)
;;                                            (consult-ripgrep (projectile-project-root) "TODO")
;;                                          (message "Not in a project")))
;;        :desc "Search all TODO types" "T" (lambda () (interactive)
;;                                             (if (projectile-project-p)
;;                                                 (consult-ripgrep (projectile-project-root) "TODO\\|HACK\\|TEMP\\|DONE\\|NOTE\\|DONT\\|DEBUG\\|FAIL\\|FIXME")
;;                                               (message "Not in a project")))
;;        :desc "Magit TODOs list" "l" #'magit-todos-list
;;        :desc "Update magit TODOs" "u" #'magit-todos-update
;;        :desc "Insert TODO comment" "i" #'hl-todo-insert))



;; ;; Toggles
;; (map! :leader
;;       :prefix "t"
;;        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        ;; TIMING KEYBINDINGS (from init-timing.el)
;;        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       (:prefix ("t" . "timing")
;;        :desc "Start timer" "s" #'pomm-start
;;        :desc "Stop timer" "S" #'pomm-stop
;;        :desc "Pause/Resume timer" "p" #'pomm-pause
;;        :desc "Pomm timer" "t" #'pomm
;;        :desc "Reset timer" "r" #'pomm-reset)
;;       "k"                          #'keycast-header-line-mode
;;       "K"                          #'keycast-log-mode)

;; ;; Tools
;; (map! :leader
;;       :prefix "l"
;;       "t" #' hl-todo-insert ;; Add a TODO comment
;;       ("x" #'align-regexp

;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         ;; AI KEYBINDINGS (from init-ai.el)
;;         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         (:prefix ("a" . "AI")
;;         :desc "GPTel" "g" #'gptel
;;         :desc "GPTel add" "a" #'gptel-add
;;         :desc "GPTel rewrite" "r" #'gptel-rewrite
;;         :desc "GPTel send" "s" #'gptel-send
;;         :desc "GPTel menu" "m" #'gptel-menu
;;         :desc "GPTel tools" "t" #'gptel-tools
;;         :desc "GPTel add file" "f" #'gptel-add-file
;;         :desc "Use Ollama Qwen" "q" #'my/gptel-use-ollama-qwen
;;         :desc "Use OpenAI" "o" #'my/gptel-use-openai
;;         :desc "Use Anthropic" "A" #'my/gptel-use-anthropic)

;;        (:prefix ("p" . "Process Management")
;;                 "p" #'prodigy
;;                 "l" #'list-processes)

;;        (:prefix ("s" . "Shell Commands")
;;         :desc "podman machine start" "p" (lambda () (interactive) (compile "cd ~/ && podman machine start"))
;;         :desc "podman machine stop" "P" (lambda () (interactive) (compile "cd ~/ && podman machine stop"))
;;         :desc "make backup_create" "b" (lambda () (interactive) (compile "cd ~/ && make backup_create"))
;;         :desc "make update_macos" "u" (lambda () (interactive) (compile "cd ~/ && make update_apps && make update_macos"))
;;         :desc "chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply --force")))

;;        (:prefix ("r" . "RSS Reader")
;;         :desc "Newsticker" "n" #'newsticker-show-news
;;         :desc "Newsticker get all" "u" #'newsticker-get-all-news
;;         :desc "Elfeed" "e" #'elfeed
;;         :desc "Elfeed get all" "U" #'elfeed-update)))


;; ;; Jumps
;; (map! :leader
;;       :prefix "j"
;;       "j" #'avy-goto-char-timer
;;       "l" #'avy-goto-line
;;       "b" #'avy-pop-mark
;;       "t" #'yas-describe-tables)


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; ORG-MODE LOCAL KEYBINDINGS
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Org agenda focus keys
;; (map! :after org-agenda
;;       :map org-agenda-mode-map
;;       :localleader
;;       (:prefix ("f" . "focus")
;;        :desc "Focus on primary cloud" "p" #'org-focus-primary
;;        :desc "Focus on workspace" "w" #'org-focus-workspace
;;        :desc "Focus on extra workspace" "e" #'org-focus-workspace-extra
;;        :desc "Focus on all org files" "a" #'org-focus-all))

;; ;; Org review keybindings (global)
;; (global-set-key (kbd "C-c r p") #'my/set-review-period)
;; (global-set-key (kbd "C-c r m") #'my/mark-reviewed)
;; (global-set-key (kbd "C-c f") #'my/weekly-outcomes-overview)

;; ;; Org utilities
;; (global-set-key (kbd "C-c o r") #'org-reload)

;; ;; Custom file attachment functions
;; (map! :after org
;;       :map org-mode-map
;;       :localleader
;;       (:prefix ("i" . "insert")

;;       (:prefix ("t" . "templates")
;;        :desc "Insert template" "i" #'my/insert-template
;;        :desc "Open templates dir" "d" #'my/open-templates-directory)

;;        :desc "Download screenshot" "s" #'org-download-screenshot
;;        :desc "Download image from URL" "u" #'org-download-image
;;        :desc "Download from clipboard" "y" #'org-download-clipboard
;;        :desc "Choose file from disk" "f" #'my/org-attach-file-and-insert-link
;;        :desc "Choose multiple files" "F" #'my/org-attach-multiple-files-and-insert-links
;;        :desc "Delete image" "D" #'org-download-delete))
