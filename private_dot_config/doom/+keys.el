;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :leader
      :desc "Fix too many open files" "K" #'file-notify-rm-all-watches
      :desc "Remove perspective by name"       "-" #'persp-remove-by-name)


;; Buffers
(map! :leader
      :prefix "b"
      :desc "Make buffer empty" "e" #'+my/erase-buffer-no-yank
      :desc "Revert buffer" "r" #'revert-buffer-no-confirm
      ;; :desc "Reload buffer" "R" #'reload-buffer-no-confirm
      :desc "Format-all buffer"      "f" #'format-all-buffer)


;; Code
(map! :leader
      :prefix "c"
      :desc "Format-all buffer"      "f" #'format-all-buffer
      "F" #'+my/untabify-buffer
      :desc "Check grammar"          "g" #'langtool-check-buffer
      :desc "Done Check grammar"     "G" #'langtool-check-done
      (:when (modulep! :tools lsp +eglot)
        :desc "Eglot organize imports"   "I" #'eglot-code-action-organize-imports
        :desc "Eglot workspace restart"  "R" #'eglot-reconnect
        :desc "Eglot quickfix" "q" #'eglot-code-action-quickfix
        )
      (:when (not (modulep! :tools lsp +eglot))
        :desc "LSP organize imports"   "I" #'lsp-organize-imports
        :desc "LSP workspace restart"  "R" #'lsp-workspace-restart
        :desc "Treemacs references"    "D" #'lsp-treemacs-references))

;; Debug
(map! :leader
      (:prefix ("d" . "debug")
       :desc "dape breakpoint toggle" "b" #'+my/dape-breakpoint-toggle
       :desc "dape breakpoint remove all" "B" #'+my/dape-breakpoint-remove-all
       ))


;; Files
(map! :leader
      :prefix "f"
      :desc "Yank filename" "n" #'+default/yank-filename
      :desc "Dirvish" "j" #'dirvish-dwim)


;; Open
(map! :leader
      :prefix "o"
      :desc "Open link" "x" #'link-hint-open-link
      :desc "Open link at point" "X" #'link-hint-open-link-at-point)

;; Project
(map! :leader
      :prefix "p"
      "n" #'+default/yank-project-name
      ;; Use the working method for SPC p t
      :desc "Search project TODOs" "T" (lambda () (interactive)
                                         (if (projectile-project-p)
                                             (consult-ripgrep (projectile-project-root) "TODO:")
                                           (message "Not in a project")))
      ;; Keep the original broken one for debugging
      :desc "Magit todo list" "t" #'magit-todos-list
      "*" (+my/prefix-M-x "projectile-")
      :desc "Update projectile list" "u" #'update-projectile-known-projects)

;; Registers
(map! :leader
      (:prefix ("r" . "registers")
       :desc "Copy to register" "c"                 #'copy-to-register
       :desc "Frameset to register" "f"             #'frameset-to-register
       :desc "Insert contents of register" "i"      #'insert-register
       :desc "Jump to register" "j"                 #'jump-to-register
       :desc "List registers" "l"                   #'list-registers
       :desc "Number to register" "n"               #'number-to-register
       :desc "Interactively choose a register" "r"  #'counsel-register
       :desc "View a register" "v"                  #'view-register
       :desc "Window configuration to register" "w" #'window-configuration-to-register
       :desc "Increment register" "+"               #'increment-register
       :desc "Point to register" "SPC"              #'point-to-register))

;; Toogles
(map! :leader
      :prefix "t"
      :desc "Pomm-third timer" "T" #'pomm-third-time
      :desc "Pomm-third" "t"       #'pomm
      "k"                          #'keycast-header-line-mode
      "K"                          #'keycast-log-mode)

(map! :leader
      :prefix "l"
      "t" #' hl-todo-insert ;; Add a TODO comment
      ("x" #'align-regexp
       :desc "Aidermacs" "a" #'aidermacs-transient-menu
       :desc "Aider" "A" #'aider-transient-menu
       (:prefix ("p" . "Process Management")
                "p" #'prodigy
                "l" #'list-processes)
       (:prefix ("c" . "Claude")
                "a" #'ai-code-menu
                "c" #'claude-code-transient
                "C" #'claude-code-ide-menu)
       (:prefix ("g" . "GPTel & Git AI")
        "g" #'gptel
        "a" #'gptel-add
        "r" #'gptel-rewrite
        "s" #'gptel-send
        "m" #'gptel-menu
        "M" #'mcp-hub
        "S" #'mcp-hub-start-server
        "t" #'gptel-tools
        "f" #'gptel-add-file
        ;; AI Git helper
        ;;
        :desc "🤖 Generate AI Commit Message" "m" #'+my/smart-commit
        :desc "🤖 Commit with AI Message" "C" #'+my/commit-with-ai)
       (:prefix ("s" . "Shell Commands")
        :desc "podman machine start" "p" (lambda () (interactive) (compile "cd ~/ && podman machine start"))
        :desc "podman machine stop" "P" (lambda () (interactive) (compile "cd ~/ && podman machine stop"))
        :desc "make backup_create" "b" (lambda () (interactive) (compile "cd ~/ && make backup_create"))
        :desc "make update_macos" "u" (lambda () (interactive) (compile "cd ~/ && make update_apps && make update_macos"))
        :desc "chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply --force")))))


;; Jumps
(map! :leader
      :prefix "j"
      "j" #'avy-goto-char-timer
      "l" #'avy-goto-line
      "b" #'avy-pop-mark
      "t" #'yas-describe-tables)
