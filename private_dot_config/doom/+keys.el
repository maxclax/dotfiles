;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'super
        mac-command-modifier 'hyper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/setup-clean-c-c-bindings ()
  (map!
   (:map override
    :desc "Org agenda" "C-c a"           #'org-agenda
    :desc "Org capture" "C-c x"          #'org-capture
    :desc "Evaluate line/region" "C-c e" #'+eval/line-or-region
    :desc "Winner undo" "C-c <left>"     #'winner-undo
    :desc "Winner redo" "C-c <right>"    #'winner-redo

    ;; (:prefix ("C-c C-f" . "Fold")
    ;;  :desc "Toggle fold" "C-f"         #'+fold/toggle
    ;;  :desc "Open fold" "C-u"           #'+fold/open
    ;;  :desc "Close fold" "C-c"          #'+fold/close
    ;;  :desc "Open all folds" "C-a C-u"  #'+fold/open-all
    ;;  :desc "Close all folds" "C-a C-c" #'+fold/close-all
    ;;  :desc "Next fold" "C-n"           #'+fold/next
    ;;  :desc "Previous fold" "C-p"       #'+fold/previous)

    (:prefix ("C-c c" . "Code")
     :desc "Jump to definition" "d"     #'+lookup/definition
     :desc "Jump to references" "D"     #'+lookup/references
     :desc "Evaluate buffer/region" "e" #'+eval/buffer-or-region
     :desc "Format buffer" "f"          #'format-all-buffer
     :desc "Find implementations" "i"   #'+lookup/implementations
     :desc "Find type definition" "t"   #'+lookup/type-definition)

    (:prefix ("C-c f" . "Files")
     :desc "Copy this file" "C"           #'doom/copy-this-file
     :desc "Delete this file" "d"         #'doom/delete-this-file
     :desc "Kill (copy) filename" "k"     #'my/kill-buffer-filename
     :desc "Kill (copy) full path" "K"    #'my/kill-buffer-filepath
     :desc "Rename/move this file" "r"    #'doom/move-this-file
     :desc "Recent files" "r"             #'consult-recent-file
     :desc "Recent project file" "R"      #'projectile-recentf
     :desc "Sudo this file" "u"           #'doom/sudo-this-file
     :desc "Sudo find file" "U"           #'doom/sudo-find-file
     :desc "Open scratch buffer" "x"      #'doom/open-scratch-buffer
     :desc "Switch to scratch buffer" "X" #'doom/switch-to-scratch-buffer)

    (:prefix ("C-c i" . "Insert")
     :desc "Emoji" "e"                        #'emoji-insert
     :desc "Current file name" "f"            #'+default/insert-filename
     :desc "Current file path" "F"            #'+default/insert-filepath
     :desc "Insert shell link" "l"           #'my/insert-shell-link
     :desc "Current time [H:M]" "t"           #'my/insert-current-time
     (:prefix ("j" . "Journal")
      :desc "Start journal log entry" "s"     #'my/journal-log-start
      :desc "Finish journal log entry" "f"    #'my/journal-log-finish)
     (:prefix ("i" . "Images")
      :desc "Attach file" "a"                 #'my/org-attach-file-and-insert-link
      :desc "Attach multiple files" "A"       #'my/org-attach-multiple-files-and-insert-links
      :desc "Insert image from clipboard" "c" #'org-download-clipboard
      :desc "Delete image" "d"                #'org-download-delete
      :desc "Insert image from file" "f"      #'org-download-image
      :desc "Insert screenshot" "s"           #'org-download-screenshot
      :desc "Insert image from URL" "u"       #'org-download-image)
     (:prefix ("o" . "Org")
      :desc "org-timestamp []" "t"            #'org-timestamp-inactive
      :desc "org-timestamp <>" "T"            #'org-timestamp))

    (:prefix ("C-c l" . "Tools")
             (:prefix ("e" . "ERC IRC")
              :desc "Show all buffers" "a"    #'my/erc-show-all-buffers
              :desc "Connect to IRC" "c"      #'my/erc-connect
              :desc "Join extra channels" "j" #'my/erc-join-extra)
             :desc "Org store link" "l"       #'org-store-link
             (:prefix ("p" . "Process")
              :desc "List processes" "l"      #'list-processes
              :desc "Prodigy" "p"             #'prodigy)
             (:prefix ("s" . "Shell")
              :desc "Chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply")))
             :desc "Insert TODO" "t"          #'hl-todo-insert
             :desc "Align regexp" "x"         #'align-regexp)

    (:prefix ("C-c n" . "Notes")
     :desc "Denote backlinks" "b"           #'denote-backlinks
     :desc "Denote link after creating" "c" #'denote-link-after-creating
     :desc "Denote dired" "d"               #'denote-dired
     :desc "Denote find" "f"                #'consult-denote-find
     :desc "Denote grep" "g"                #'denote-grep
     :desc "Open inbox" "i"                 #'my/open-inbox
     (:prefix ("j" . "Journal")
      :desc "Today's journal" "j"           #'denote-journal-new-or-existing-entry
      :desc "Tomorrow's journal" "t"        #'my/open-tomorrow-journal
      :desc "Yesterday's journal" "y"       #'my/open-yesterday-journal)
     :desc "Open Journelly" "J"             #'my/open-journelly
     :desc "Denote link" "l"                #'denote-link
     :desc "New denote" "n"                 #'denote
     :desc "Denote rename" "r"              #'denote-rename-file
     :desc "Consult notes" "s"              #'consult-notes
     :desc "Search all notes" "S"           #'consult-notes-search-in-all-notes)

    (:prefix ("C-c o" . "Open")
     :desc "Dired jump" "-"         #'dired-jump
     :desc "Dirvish" "/"            #'dirvish
     :desc "Docker" "D"             #'docker
     :desc "Reveal in Finder" "O"   #'+macos/reveal-project-in-finder
     :desc "Dirvish sidebar" "p"    #'dirvish-side
     :desc "REPL" "r"               #'+eval/open-repl-other-window
     :desc "REPL" "R"               #'+eval/open-repl-same-window
     :desc "Open link at point" "x" #'link-hint-open-link-at-point)

    (:prefix ("C-c p" . "Project")
     :desc "Recent files" "e"            #'projectile-recentf
     :desc "Find file" "f"               #'projectile-find-file
     :desc "Find file other project" "F" #'doom/find-file-in-other-project
     :desc "Kill project buffers" "k"    #'projectile-kill-buffers
     :desc "Switch project" "p"          #'projectile-switch-project
     :desc "Search project" "s"          #'+default/search-project
     (:prefix ("t" . "TODOs")
      :desc "Insert TODO comment" "i"    #'hl-todo-insert
      :desc "Search TODOs" "t" (lambda () (interactive)
                                 (if (projectile-project-p)
                                     (consult-ripgrep (projectile-project-root) "TODO:")
                                   (message "Not in a project")))))

    (:prefix ("C-c v" . "Versioning")
     :desc "AI commit" "a"                 #'my/commit-with-ai
     :desc "Generate AI commit (text)" "A" #'my/smart-commit
     :desc "Magit blame" "B"               #'magit-blame
     :desc "Kill link to remote" "k"       #'+vc/browse-at-remote-kill
     :desc "Kill link to homepage" "K"     #'+vc/browse-at-remote-kill-homepage
     :desc "Magit buffer log" "L"          #'magit-log-buffer-file
     :desc "Jump to next hunk" "n"         #'+vc-gutter/next-hunk
     :desc "Jump to previous hunk" "p"     #'+vc-gutter/previous-hunk
     :desc "Git time machine" "t"          #'git-timemachine-toggle)

    (:prefix ("C-c t" . "Toggles")
     :desc "Fill column" "c"           #'global-display-fill-column-indicator-mode
     :desc "Eat other window" "e"      #'eat-other-window
     :desc "Flymake" "f"               #'flymake-mode
     :desc "Indent guides" "i"         #'indent-bars-mode
     :desc "Indent style" "I"          #'doom/toggle-indent-style
     :desc "Line numbers" "l"          #'doom/toggle-line-numbers
     :desc "Read-only" "r"             #'read-only-mode
     :desc "Soft line wrapping" "w"    #'+word-wrap-mode
     :desc "Zen mode" "z"              #'+zen/toggle
     :desc "Zen mode (fullscreen)" "Z" #'+zen/toggle-fullscreen
     (:prefix ("t" . "Timer")
      :desc "Switch timer" "p"         #'pomm-third-time-switch
      :desc "Start timer" "s"          #'pomm-third-time-start
      :desc "Stop timer" "S"           #'pomm-third-time-stop
      :desc "Pomm third-time" "t"      #'pomm-third-time)))))


;; Setup C-x bindings
(defun my/setup-c-x-bindings ()
  (map! :desc "ibuffer" "C-x C-b"                  #'ibuffer
        :desc "Split horizontally instead" "C-x |" #'my/split-window-horizontally-instead
        :desc "Split vertically instead" "C-x _"   #'my/split-window-vertically-instead))

;; Apply early to ensure our bindings take precedence
(add-hook 'doom-first-buffer-hook #'my/setup-clean-c-c-bindings)
(add-hook 'doom-first-buffer-hook #'my/setup-c-x-bindings)

;; Mac-style Cmd shortcuts for GUI Emacs
(defun my/setup-mac-cmd-shortcuts ()
  "Setup Mac-style Cmd+C/V/X/Z shortcuts for GUI Emacs."
  (when (display-graphic-p)
    (map!
     (:map override
      :desc "Copy (Cmd+C)" "H-c" #'kill-ring-save
      :desc "Paste (Cmd+V)" "H-v" #'yank
      :desc "Cut (Cmd+X)" "H-x" #'kill-region
      :desc "Undo (Cmd+Z)" "H-z" #'undo
      :desc "Redo (Shift+Cmd+Z)" "H-Z" #'redo
      :desc "Paste Pop (Shift+Cmd+V)" "H-V" #'yank-pop))))

;; Apply early to ensure our bindings take precedence
(add-hook 'doom-first-buffer-hook #'my/setup-mac-cmd-shortcuts)

;; Apply very late to override any packages that load after us
(add-hook 'window-setup-hook #'my/setup-clean-c-c-bindings)
(add-hook 'window-setup-hook #'my/setup-c-x-bindings)
(add-hook 'window-setup-hook #'my/setup-mac-cmd-shortcuts)
