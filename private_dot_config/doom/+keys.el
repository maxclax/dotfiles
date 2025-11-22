;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (map! :leader
;;       :desc "Fix too many open files" "K" #'file-notify-rm-all-watches
;;       :desc "Remove perspective by name"       "-" #'persp-remove-by-name)


;; ;; Buffers
;; (map! :leader
;;       :prefix "b"
;;       :desc "Make buffer empty" "e" #'+my/erase-buffer-no-yank
;;       :desc "Revert buffer" "r" #'revert-buffer-no-confirm
;;       ;; :desc "Reload buffer" "R" #'reload-buffer-no-confirm
;;       :desc "Format-all buffer"      "f" #'format-all-buffer)


;; ;; Code
;; (map! :leader
;;       :prefix "c"
;;       :desc "Format-all buffer"      "f" #'format-all-buffer
;;       "F" #'+my/untabify-buffer
;;       :desc "Check grammar"          "g" #'langtool-check-buffer
;;       :desc "Done Check grammar"     "G" #'langtool-check-done
;;       (:when (modulep! :tools lsp +eglot)
;;         :desc "Eglot organize imports"   "I" #'eglot-code-action-organize-imports
;;         :desc "Eglot workspace restart"  "R" #'eglot-reconnect
;;         :desc "Eglot quickfix" "q" #'eglot-code-action-quickfix
;;         )
;;       (:when (not (modulep! :tools lsp +eglot))
;;         :desc "LSP organize imports"   "I" #'lsp-organize-imports
;;         :desc "LSP workspace restart"  "R" #'lsp-workspace-restart
;;         :desc "Treemacs references"    "D" #'lsp-treemacs-references))

;; ;; Debug
;; (map! :leader
;;       (:prefix ("d" . "debug")
;;        :desc "dape breakpoint toggle" "b" #'+my/dape-breakpoint-toggle
;;        :desc "dape breakpoint remove all" "B" #'+my/dape-breakpoint-remove-all
;;        ))


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

;; ;; Main denote keybindings
;; (map! :leader
;;       :prefix "n"
;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       ;; DENOTE KEYBINDINGS (from init-denote.el)
;;       ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;       (:prefix ("d" . "denote")
;;        :desc "Create typed note" "t" #'my/denote-create-typed-note
;;        :desc "Find note" "f" #'denote-open-or-create
;;        :desc "Create note" "n" #'denote
;;        :desc "Create note in extra" "N" #'my/denote-create-in-extra
;;        :desc "Link to note" "l" #'denote-link
;;        :desc "Backlinks" "b" #'denote-find-backlink
;;        :desc "Rename file" "r" #'denote-rename-file
;;        :desc "Keywords" "k" #'denote-keywords-add
;;        :desc "Remove keywords" "K" #'denote-keywords-remove
;;        :desc "Link to heading" "h" #'denote-org-extras-link-to-heading
;;        :desc "Search notes" "s" #'consult-notes
;;        :desc "Search in notes" "S" #'consult-notes-search-in-all-notes
;;        :desc "Debug directories" "D" #'my/debug-denote-dirs
;;        :desc "Explore network" "e" #'denote-explore-network
;;        :desc "Refresh search sources" "g" #'my/refresh-consult-notes-sources

;;        ;; Lifecycle management
;;        :desc "Complete project" "C" #'my/denote-complete-project
;;        :desc "Archive note" "A" #'my/denote-archive-note
;;        :desc "Find active projects" "p" #'my/denote-find-active-projects
;;        :desc "Find completed projects" "P" #'my/denote-find-completed-projects
;;        :desc "Find archived notes" "a" #'my/denote-find-archived

;;        ;; Additional denote functions
;;        :desc "Date" "T" #'denote-date
;;        :desc "Link or create" "i" #'denote-link-or-create
;;        :desc "Find link" "L" #'denote-find-link
;;        :desc "Rename keywords" "m" #'denote-rename-file-keywords
;;        :desc "Rename using front matter" "M" #'denote-rename-file-using-front-matter

;;        ;; PARA method
;;        :desc "Assign PARA category" "=" #'my/denote-assign-para
;;        :desc "Find PARA notes" "/" #'my/denote-find-para

;;        ;; Explore functions
;;        (:prefix ("x" . "explore")
;;         :desc "Count notes" "c" #'denote-explore-count-notes
;;         :desc "Count keywords" "C" #'denote-explore-count-keywords
;;         :desc "Barchart keywords" "b" #'denote-explore-barchart-keywords
;;         :desc "Barchart filetypes" "e" #'denote-explore-barchart-filetypes
;;         :desc "Random note" "r" #'denote-explore-random-note
;;         :desc "Random link" "l" #'denote-explore-random-link
;;         :desc "Random keyword" "k" #'denote-explore-random-keyword
;;         :desc "Random regex" "x" #'denote-explore-random-regex
;;         :desc "Duplicate notes" "d" #'denote-explore-identify-duplicate-notes
;;         :desc "Zero keywords" "z" #'denote-explore-zero-keywords
;;         :desc "Single keywords" "s" #'denote-explore-single-keywords
;;         :desc "Sort keywords" "o" #'denote-explore-sort-keywords
;;         :desc "Rename keyword" "w" #'denote-explore-rename-keyword
;;         :desc "Network" "n" #'denote-explore-network
;;         :desc "Network regenerate" "v" #'denote-explore-network-regenerate
;;         :desc "Barchart degree" "D" #'denote-explore-barchart-degree)

;;        ;; Citar-denote functions
;;        (:prefix ("c" . "citar")
;;         :desc "Create citation note" "c" #'citar-create-note
;;         :desc "Open note" "n" #'citar-denote-open-note
;;         :desc "No cite" "x" #'citar-denote-nocite
;;         :desc "Add citekey" "k" #'citar-denote-add-citekey
;;         :desc "Remove citekey" "K" #'citar-denote-remove-citekey
;;         :desc "DWIM" "d" #'citar-denote-dwim
;;         :desc "Open reference" "e" #'citar-denote-open-reference-entry))

;;       ;; Journal keybindings
;;       (:prefix ("j" . "journal")
;;        :desc "Today's journal" "j" #'denote-journal-new-or-existing-entry
;;        :desc "New journal entry" "n" #'denote-journal-new-entry
;;        :desc "Journal in extra" "J" #'my/denote-journal-extra
;;        :desc "Link to journal" "l" #'denote-journal-link-or-create-entry
;;        :desc "Open calendar" "c" #'calendar
;;        :desc "Journal from calendar" "C" #'denote-journal-calendar-new-or-existing
;;        :desc "Find entry from calendar" "f" #'denote-journal-calendar-find-file
;;        :desc "Insert template" "t" #'my/insert-template))

;; ;; Registers
;; (map! :leader
;;       (:prefix ("r" . "registers")
;;        :desc "Copy to register" "c"                 #'copy-to-register
;;        :desc "Frameset to register" "f"             #'frameset-to-register
;;        :desc "Insert contents of register" "i"      #'insert-register
;;        :desc "Jump to register" "j"                 #'jump-to-register
;;        :desc "List registers" "l"                   #'list-registers
;;        :desc "Number to register" "n"               #'number-to-register
;;        :desc "Interactively choose a register" "r"  #'counsel-register
;;        :desc "View a register" "v"                  #'view-register
;;        :desc "Window configuration to register" "w" #'window-configuration-to-register
;;        :desc "Increment register" "+"               #'increment-register
;;        :desc "Point to register" "SPC"              #'point-to-register))

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
