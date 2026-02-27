;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'super
        mac-command-modifier 'hyper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default doom ~/.config/emacs/modules/config/default/+evil-bindings.el
(defun my/setup-clean-c-c-bindings ()
  (map!
   (:map override
    :desc "Org agenda" "C-c a"           #'org-agenda
    :desc "Org capture" "C-c x"          #'org-capture
    :desc "Evaluate line/region" "C-c e" #'+eval/line-or-region
    :desc "Winner undo" "C-c <left>"     #'winner-undo
    :desc "Winner redo" "C-c <right>"    #'winner-redo
    :desc "Open scratch buffer" "C-c X"      #'doom/switch-to-scratch-buffer

    (:prefix ("C-c C-f" . "Fold")
     :desc "Toggle fold" "C-f"         #'my/fold-toggle
     :desc "Open fold" "C-u"           #'+fold/open
     :desc "Close fold" "C-c"          #'+fold/close
     :desc "Open all folds" "C-a C-u"  #'+fold/open-all
     :desc "Close all folds" "C-a C-c" #'+fold/close-all
     :desc "Next fold" "C-n"           #'+fold/next
     :desc "Previous fold" "C-p"       #'+fold/previous)

    (:prefix ("C-c s" . "Syntax/Diagnostics")
     :desc "Next error"           "n" #'flymake-goto-next-error
     :desc "Previous error"       "p" #'flymake-goto-prev-error
     :desc "Explain error"        "e" #'flymake-show-diagnostic
     :desc "List buffer errors"   "l" #'flymake-show-buffer-diagnostics
     :desc "List project errors"  "L" #'flymake-show-project-diagnostics
     :desc "Consult flymake"      "d" #'consult-flymake
     :desc "Cspell buffer"        "c" #'my/cspell-check-buffer
     :desc "Cspell changed files" "C" #'my/cspell-check-diff)

    (:prefix ("C-c d" . "Debugger")
     :desc "dape"                  "d" #'dape
     :desc "Pause"                 "p" #'dape-pause
     :desc "Continue"              "c" #'dape-continue
     :desc "Next"                  "n" #'dape-next
     :desc "Step in"               "s" #'dape-step-in
     :desc "Step out"              "o" #'dape-step-out
     :desc "Restart"               "r" #'dape-restart
     :desc "Info"                  "i" #'dape-info
     :desc "REPL"                  "R" #'dape-repl
     :desc "Memory"                "m" #'dape-memory
     :desc "Disassemble"           "M" #'dape-disassemble
     :desc "Breakpoint log"        "l" #'dape-breakpoint-log
     :desc "Breakpoint expression" "e" #'dape-breakpoint-expression
     :desc "Breakpoint hits"       "h" #'dape-breakpoint-hits
     :desc "Breakpoint toggle"     "b" #'dape-breakpoint-toggle
     :desc "Breakpoint remove all" "B" #'dape-breakpoint-remove-all
     :desc "Select thread"         "t" #'dape-select-thread
     :desc "Select stack"          "S" #'dape-select-stack
     :desc "Stack down"            ">" #'dape-stack-select-down
     :desc "Stack up"              "<" #'dape-stack-select-up
     :desc "Evaluate expression"   "x" #'dape-evaluate-expression
     :desc "Watch dwim"            "w" #'dape-watch-dwim
     :desc "Disconnect"            "D" #'dape-disconnect-quit
     :desc "Quit"                  "q" #'dape-quit)

    (:prefix ("C-c c" . "Code")
     :desc "Format buffer" "f"           #'format-all-buffer
     :desc "Kill inside pair" "k"        #'my/kill-inside-pair
     :desc "LSP: Code action" "a"        #'lsp-execute-code-action
     :desc "LSP: Definition" "g"         #'+lookup/definition
     :desc "LSP: Implementations" "i"    #'+lookup/implementations
     :desc "LSP: Organize imports" "o"   #'lsp-organize-imports
     :desc "LSP: References" "G"         #'+lookup/references
     :desc "LSP: Rename symbol" "r"      #'lsp-rename
     :desc "LSP: Restart workspace" "R"  #'lsp-workspace-restart
     :desc "LSP: Type definition" "t"    #'+lookup/type-definition
     :desc "Sort lines"             "s"  #'sort-lines
     (:prefix ("p" . "Python")
      :desc "Copy python cmd"      "p" #'my/python-copy-python-cmd
      :desc "Copy pytest cmd"      "y" #'my/python-copy-pytest-cmd
      :desc "Yank module import"   "i" #'my/python-yank-module-import
      :desc "Insert yanked import" "I" #'my/python-insert-temp-import))

    (:prefix ("C-c f" . "Files")
     :desc "Copy this file" "C"           #'doom/copy-this-file
     :desc "Delete this file" "d"         #'doom/delete-this-file
     :desc "Kill (copy) filename" "k"     #'my/kill-buffer-filename
     :desc "Kill (copy) full path" "K"    #'my/kill-buffer-filepath
     :desc "Rename/move this file" "m"    #'doom/move-this-file
     :desc "Recent files" "r"             #'consult-recent-file
     :desc "Open file at point" "p"       #'ffap
     :desc "Sudo this file" "u"           #'doom/sudo-this-file
     :desc "Sudo find file" "U"           #'doom/sudo-find-file)

    (:prefix ("C-c i" . "Insert")
     :desc "Emoji" "e"                        #'emoji-insert
     :desc "Current file name" "f"            #'+default/insert-filename
     :desc "Current file path" "F"            #'+default/insert-filepath
     :desc "Insert link from clipboard" "l"  #'org-cliplink
     :desc "Insert shell link" "L"           #'my/insert-shell-link
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
              :desc "Chezmoi apply" "c" (lambda () (interactive) (let ((compilation-buffer-name-function nil)) (compile "chezmoi apply"))))
             (:prefix ("d" . "Database")
              :desc "pgmacs browser" "m"         #'my/pgmacs-connect-uri)
             :desc "Reset checklist" "r"       #'my/org-reset-checklist
             :desc "Insert TODO" "t"          #'hl-todo-insert
             :desc "Align regexp" "x"         #'align-regexp)

    (:prefix ("C-c n" . "Notes")
     :desc "Denote backlinks" "b"           #'denote-backlinks
     :desc "Denote link after creating" "c" #'denote-link-after-creating
     :desc "Denote dired" "d"               #'my/denote-dired
     :desc "Denote feed" "e"                #'my/denote-feed
     :desc "Denote find" "f"                #'my/denote-find
     :desc "Denote grep" "g"                #'denote-grep
     :desc "Open inbox" "i"                 #'my/open-inbox
     (:prefix ("j" . "Journal")
      :desc "Today's journal" "j"           #'denote-journal-new-or-existing-entry
      :desc "Tomorrow's journal" "t"        #'my/open-tomorrow-journal
      :desc "Yesterday's journal" "y"       #'my/open-yesterday-journal
      :desc "Refile to today" "r"           #'my/refile-to-today-journal)
     :desc "Open Journelly" "J"             #'my/open-journelly
     :desc "Journelly by tag" "T"            #'my/journelly-search-tag
     :desc "Denote link" "l"                #'denote-link
     :desc "Denote add links" "L"            #'denote-add-links
     :desc "New denote" "n"                 #'denote
     :desc "New denote (extra)" "N"          #'my/denote-create-in-extra
     :desc "Denote rename" "r"              #'denote-rename-file
     :desc "Consult notes" "s"              #'consult-notes
     :desc "Search all notes" "S"           #'consult-notes-search-in-all-notes)

    (:prefix ("C-c o" . "Open")
     :desc "Dired jump" "-"         #'dired-jump
     :desc "Docker" "D"             #'docker
     :desc "Reveal in Finder" "O"   #'+macos/reveal-project-in-finder
     :desc "Dirvish sidebar" "p"    #'dirvish-side
     :desc "REPL" "r"               #'+eval/open-repl-other-window
     :desc "REPL" "R"               #'+eval/open-repl-same-window
     :desc "Open link at point" "x" #'link-hint-open-link-at-point)

    (:prefix ("C-c p" . "Project")
     :desc "Find file" "f"               #'projectile-find-file
     :desc "Find file other project" "F" #'doom/find-file-in-other-project
     :desc "Kill project buffers" "k"    #'projectile-kill-buffers
     :desc "Switch project" "p"          #'projectile-switch-project
     :desc "Recent files" "r"            #'projectile-recentf
     :desc "Search project" "s"          #'+default/search-project
     (:prefix ("t" . "TODOs")
      :desc "Insert TODO comment" "i"    #'hl-todo-insert
      :desc "Search TODOs" "t" (lambda () (interactive)
                                 (if (projectile-project-p)
                                     (consult-ripgrep (projectile-project-root) "TODO:")
                                   (message "Not in a project")))))

    (:prefix ("C-c v" . "Versioning")
     :desc "Magit blame" "B"               #'magit-blame
     :desc "Kill link to remote" "k"       #'+vc/browse-at-remote-kill
     :desc "Kill link to homepage" "K"     #'+vc/browse-at-remote-kill-homepage
     :desc "Magit buffer log" "L"          #'magit-log-buffer-file
     :desc "Jump to next hunk" "n"         #'+vc-gutter/next-hunk
     :desc "Jump to previous hunk" "p"     #'+vc-gutter/previous-hunk
     :desc "Git time machine" "t"          #'git-timemachine-toggle)

    (:prefix ("C-c A" . "AI")
     :desc "gptel: rewrite region"  "r" #'gptel-rewrite
     :desc "aider: project session" "a" #'my/aider-project
     :desc "aider: eat terminal"    "e" #'my/aider-eat
     :desc "aider: code change"     "c" #'aider-function-or-region-change
     :desc "aider: ask about code"  "q" #'aider-ask-question
     :desc "aider: commit message"    "C" #'my/aider-commit-message
     :desc "aider: fix flymake errors" "F" #'my/aider-flymake-fix-errors
     :desc "aider: TDD cycle"          "t" #'aider-tdd-cycle)

    (:prefix ("C-c t" . "Toggles")
     :desc "Fill column" "c"           #'global-display-fill-column-indicator-mode
     :desc "Eat other window" "e"      #'eat-other-window
     :desc "Keycast log mode" "K" #'keycast-log-mode
     :desc "Keycast header mode" "k" #'keycast-header-line-mode
     :desc "Flymake" "f"               #'flymake-mode
     :desc "Indent guides" "i"         #'indent-bars-mode
     :desc "Indent style" "I"          #'doom/toggle-indent-style
     :desc "Link display" "L"           #'org-toggle-link-display
     :desc "Line numbers" "l"          #'doom/toggle-line-numbers
     :desc "Inline images" "p"          #'org-toggle-inline-images
     :desc "Read-only" "r"             #'read-only-mode
     :desc "Soft line wrapping" "w"    #'+word-wrap-mode
     :desc "Copilot" "a"               #'copilot-mode
     :desc "Minuet"  "A"               #'minuet-auto-suggestion-mode
     :desc "Prose mode" "P"             #'prose-mode
     :desc "Zen mode" "z"              #'+zen/toggle
     :desc "Zen mode (fullscreen)" "Z" #'+zen/toggle-fullscreen
     (:prefix ("t" . "Timing")
      :desc "Start timer" "t"          #'tmr
      :desc "Start with details" "T"   #'tmr-with-details
      :desc "Toggle ticking sound" "s" #'my/tick-toggle
      :desc "Cancel timer" "x"         #'tmr-cancel
      :desc "Reschedule timer" "r"     #'tmr-reschedule
      :desc "Pause/Resume" "p"         #'tmr-toggle-pause
      :desc "List timers" "l"          #'tmr-tabulated-view
      :desc "List timers (full)" "L"   #'tmr-tabulated-mode)))))


;; Setup C-x bindings
(defun my/setup-c-x-bindings ()
  (map! :desc "Project ibuffer" "C-x C-b"          #'projectile-ibuffer
        :desc "All ibuffer" "C-x C-B"             #'ibuffer
        :desc "Dirvish" "C-x d"                    #'dirvish
        :desc "Project buffers" "C-x b"            #'projectile-switch-to-buffer
        :desc "All buffers" "C-x B"                #'switch-to-buffer
        :desc "Split horizontally instead" "C-x |" #'my/split-window-horizontally-instead
        :desc "Split vertically instead" "C-x _"   #'my/split-window-vertically-instead))

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

;; Setup M bindings
(defun my/setup-meta-bindings ()
  (map! :desc "Other window"    "M-o" #'other-window
        :desc "imenu"           "M-i" #'imenu
        ;; Workspace switching — M-1..9 overrides magit's M-1..4 (show-level-all),
        ;; which is acceptable since plain 1/2/3/4 still works in magit.
        :desc "Workspace 1"    "M-1" #'+workspace/switch-to-0
        :desc "Workspace 2"    "M-2" #'+workspace/switch-to-1
        :desc "Workspace 3"    "M-3" #'+workspace/switch-to-2
        :desc "Workspace 4"    "M-4" #'+workspace/switch-to-3
        :desc "Workspace 5"    "M-5" #'+workspace/switch-to-4
        :desc "Workspace 6"    "M-6" #'+workspace/switch-to-5
        :desc "Workspace 7"    "M-7" #'+workspace/switch-to-6
        :desc "Workspace 8"    "M-8" #'+workspace/switch-to-7
        :desc "Workspace 9"    "M-9" #'+workspace/switch-to-8
        :desc "Prev workspace" "M-[" #'+workspace/switch-left
        :desc "Next workspace" "M-]" #'+workspace/switch-right
        :desc "Last workspace" "M-`" #'+workspace/other
        :desc "New workspace"      "M-N" #'+workspace/new
        :desc "Kill workspace"     "M-D" #'+workspace/kill
        :desc "Display workspaces" "M-W" #'+workspace/display
        :desc "Switch workspace"   "M-S" #'+workspace/switch-to))

;; Prevent projectile and persp-mode from binding their keymaps to C-c p.
;; Must be set before these modes activate.
(setq projectile-keymap-prefix nil
      persp-keymap-prefix (kbd "C-c P"))

;; Apply early to ensure our bindings take precedence
(add-hook 'doom-first-buffer-hook #'my/setup-clean-c-c-bindings)
(add-hook 'doom-first-buffer-hook #'my/setup-c-x-bindings)
(add-hook 'doom-first-buffer-hook #'my/setup-mac-cmd-shortcuts)
(add-hook 'doom-first-buffer-hook #'my/setup-meta-bindings)

;; Apply very late to override any packages that load after us
(add-hook 'window-setup-hook #'my/setup-clean-c-c-bindings)
(add-hook 'window-setup-hook #'my/setup-c-x-bindings)
(add-hook 'window-setup-hook #'my/setup-mac-cmd-shortcuts)
(add-hook 'window-setup-hook #'my/setup-meta-bindings)

;; ── Global bindings ───────────────────────────────────────────────────────────

(map! "M-+" #'tempel-complete            ; Complete snippet at point
      "M-*" #'tempel-insert             ; Insert snippet by name
      "C-o" #'my/casual-open            ; Context-aware transient menu
      "C-=" #'er/expand-region          ; Expand selection: word → line → block → ...
      "C--" #'er/contract-region)       ; Shrink selection back

;; ── Mode-specific bindings ────────────────────────────────────────────────────

(after! symbol-overlay
  (map! :map symbol-overlay-mode-map
        "C-c h h" #'symbol-overlay-put
        "C-c h c" #'symbol-overlay-remove-all
        "C-c h n" #'symbol-overlay-jump-next
        "C-c h p" #'symbol-overlay-jump-prev))

(after! elisp-mode
  (map! :map emacs-lisp-mode-map
        "C-c m" #'macrostep-expand))

(after! casual
  (map! :map isearch-mode-map "C-o" #'casual-isearch-tmenu)
  (with-eval-after-load 'calc-alg
    (when (boundp 'calc-alg-map)
      (map! :map calc-alg-map "C-o" #'casual-calc-tmenu)))
  (with-eval-after-load 'ediff
    (map! :map ediff-mode-map "C-o" #'casual-ediff-tmenu)))

(after! dirvish
  (map! :map dirvish-mode-map
        "`"     #'dirvish-quick-access
        "C-o"   #'my/casual-open        ; casual-dired-tmenu (overrides dired-display-file)
        "C-f"   #'dired-find-file
        "C-b"   #'dired-up-directory
        "TAB"   #'dirvish-subtree-toggle
        "M-TAB" #'dirvish-layout-toggle))


(after! verb
  (map! :map org-mode-map "C-c C-r" verb-command-map))

(after! copilot
  (map! :map copilot-completion-map
        "<tab>"   #'copilot-accept-completion
        "TAB"     #'copilot-accept-completion
        "C-TAB"   #'copilot-accept-completion-by-word
        "C-<tab>" #'copilot-accept-completion-by-word
        "M-n"     #'copilot-next-completion
        "M-p"     #'copilot-previous-completion))

;; Minuet keybindings (copilot-style inline completions)
(after! minuet
  (map! :map minuet-active-mode-map
        "<tab>"   #'minuet-accept-suggestion
        "TAB"     #'minuet-accept-suggestion
        "C-TAB"   #'minuet-accept-suggestion-line
        "C-<tab>" #'minuet-accept-suggestion-line
        "M-n"     #'minuet-next-suggestion
        "M-p"     #'minuet-previous-suggestion
        "M-e"     #'minuet-dismiss-suggestion))
