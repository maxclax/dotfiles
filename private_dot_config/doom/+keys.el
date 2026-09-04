;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC
  (setq mac-option-modifier 'meta
        mac-right-option-modifier 'super
        mac-command-modifier 'hyper))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; surround.el — add/change/delete surrounding pairs
;; H-' as prefix (Hyper+quote), with which-key integration
(use-package! surround
  :bind-keymap ("M-'" . surround-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; default doom ~/.config/emacs/modules/config/default/+evil-bindings.el
(defun my/setup-clean-c-c-bindings ()
  (map!
   (:map override
    :desc "Org agenda" "C-c a"           #'org-agenda
    :desc "Org capture" "C-c x"          #'org-capture
    :desc "Org-ql search" "C-c q"        #'org-ql-search
    :desc "Org-ql views" "C-c Q"         #'org-ql-view
    :desc "Evaluate line/region" "C-c e" #'+eval/line-or-region
    :desc "Winner undo" "C-c <left>"     #'winner-undo
    :desc "Winner redo" "C-c <right>"    #'winner-redo
    :desc "Open scratch buffer" "C-c X"      #'doom/switch-to-scratch-buffer
    ;; Copy (non-destructive). `w' mirrors Emacs's own copy key, M-w.
    (:prefix ("C-c w" . "Copy")
     :desc "Copy line" "l"                    #'my/copy-line
     :desc "Copy to end of line" "e"          #'my/copy-to-end-of-line
     :desc "Copy value (: - = |)" "v"         #'my/copy-value
     :desc "Copy whole buffer" "b"            #'my/copy-buffer
     :desc "Copy org block contents" "B"      #'my/org-copy-block
     :desc "Copy note title" "t"              #'my/denote-copy-title
     :desc "Copy inside pair" "p"             #'my/copy-inside-pair
     :desc "Copy link at point" "L"           #'my/copy-link
     (:prefix ("f" . "File")
      :desc "Copy file name" "n"              #'my/kill-buffer-filename
      :desc "Copy file path" "p"              #'my/kill-buffer-filepath))

    ;; Kill / cut (deletes + saves to the ring). `k' mirrors C-k.
    (:prefix ("C-c k" . "Kill (cut)")
     :desc "Kill whole line" "l"              #'kill-whole-line
     :desc "Kill to end of line" "e"          #'kill-line
     :desc "Kill inside pair" "p"             #'my/kill-inside-pair)

    ;; Shifted F: plain C-c C-f must stay free for major modes (org: forward
    ;; heading same level); org folding itself is TAB, this is for other langs
    (:prefix ("C-c F" . "Fold")
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
     :desc "Copy inside pair" "y"        #'my/copy-inside-pair
     :desc "LSP: Code action" "a"        #'lsp-execute-code-action
     :desc "LSP: Rename symbol" "r"      #'lsp-rename
     :desc "LSP: Restart workspace" "R"  #'lsp-workspace-restart
     :desc "LSP: Type definition" "t"    #'+lookup/type-definition
     :desc "LSP: Doc glance" "h"         #'lsp-ui-doc-glance
     :desc "LSP: Peek definition" "d"    #'lsp-ui-peek-find-definitions
     :desc "LSP: Peek references" "e"    #'lsp-ui-peek-find-references
     :desc "Sort lines"             "s"  #'sort-lines
     :desc "Align regexp"           "x"  #'align-regexp
     (:prefix ("p" . "Python")
      :desc "Copy python cmd"      "p" #'my/python-copy-python-cmd
      :desc "Copy pytest cmd"      "y" #'my/python-copy-pytest-cmd
      :desc "Yank module import"   "i" #'my/python-yank-module-import
      :desc "Insert yanked import" "I" #'my/python-insert-temp-import))

    (:prefix ("C-c f" . "Files")
     :desc "Copy this file" "C"           #'doom/copy-this-file
     :desc "Delete this file" "d"         #'doom/delete-this-file
     :desc "Rename/move this file" "m"    #'doom/move-this-file
     :desc "Recent files" "r"             #'consult-recent-file
     :desc "Open file at point" "p"       #'ffap
     :desc "Sudo this file" "u"           #'doom/sudo-this-file
     :desc "Sudo find file" "U"           #'doom/sudo-find-file)

    ;; Date/time stamps: use org natives — C-c ! for [] (C-u adds time),
    ;; C-c . for <>
    (:prefix ("C-c i" . "Insert")
     :desc "TODO comment" "t"                 #'hl-todo-insert
     :desc "Emoji" "e"                        #'emoji-insert
     :desc "Current file name" "f"            #'+default/insert-filename
     :desc "Current file path" "F"            #'+default/insert-filepath
     :desc "Insert link from clipboard" "l"  #'org-cliplink
     :desc "Insert shell link" "L"           #'my/insert-shell-link
     (:prefix ("j" . "Journal")
      :desc "Start journal log entry" "s"     #'my/journal-log-start
      :desc "Finish journal log entry" "f"    #'my/journal-log-finish)
     (:prefix ("i" . "Images")
      :desc "Paste from clipboard" "c" #'org-download-clipboard
      :desc "Insert from file" "f"     #'org-download-image
      :desc "Insert from URL" "u"      #'org-download-yank
      :desc "Take screenshot" "s"      #'org-download-screenshot
      :desc "Delete image" "d"         #'org-download-delete))

    (:prefix ("C-c l" . "Tools")
             ;; gptel for one-shot questions and region rewrites; agentic work
             ;; is pi in a terminal. `a' is DWIM context — region, dired marks,
             ;; else the buffer.
             ;; Not `C-c C-a': that is org-attach in org-mode-map.
             (:prefix ("a" . "AI")
              :desc "gptel: chat buffer" "c"         #'gptel
              :desc "gptel: send" "s"                #'gptel-send
              :desc "gptel: menu (all options)" "m"  #'gptel-menu
              :desc "gptel: rewrite region" "r"      #'gptel-rewrite
              :desc "gptel: add to context" "a"      #'gptel-add
              :desc "gptel: add file to context" "f" #'gptel-add-file
              :desc "gptel: abort request" "k"       #'gptel-abort)
             (:prefix ("e" . "ERC IRC")
              :desc "Show all buffers" "a"    #'my/erc-show-all-buffers
              :desc "Connect to IRC" "c"      #'my/erc-connect
              :desc "Join extra channels" "j" #'my/erc-join-extra)
             (:prefix ("f" . "Frames")
              :desc "Close others, main on 5" "0" #'my/close-other-frames
              :desc "Create 1 frame"  "1" #'my/make-1-frame
              :desc "Create 2 frames" "2" #'my/make-2-frames
              :desc "Create 3 frames" "3" #'my/make-3-frames)
             :desc "Org store link" "l"       #'org-store-link
             (:prefix ("p" . "Process")
              :desc "List processes" "l"      #'list-processes
              :desc "Prodigy" "p"             #'prodigy)
             (:prefix ("s" . "Shell")
              :desc "Chezmoi apply" "c" (lambda () (interactive) (let ((compilation-buffer-name-function nil)) (compile "chezmoi apply"))))
             (:prefix ("d" . "Database")
              :desc "Connect (URI)"   "d"  #'my/pgmacs-connect-uri
              :desc "Connect (ask)"   "c"  #'my/pgmacs-connect
              :desc "Run SQL"         "e"  #'pgmacs-run-sql
              :desc "Table list"      "t"  #'pgmacs--switch-to-database-buffer)
             :desc "Reset checklist" "r"       #'my/org-reset-checklist
             :desc "X (Twitter)" "T"           #'my/x
             )

    (:prefix ("C-c m" . "Multiple cursors")
     :desc "Edit lines" "l" #'mc/edit-lines
     :desc "Mark next" "n" #'mc/mark-next-like-this
     :desc "Mark previous" "N" #'mc/mark-previous-like-this
     :desc "Mark next word" "w" #'mc/mark-next-like-this-word
     :desc "Mark previous word" "W" #'mc/mark-previous-like-this-word
     :desc "Mark all" "a" #'mc/mark-all-like-this
     :desc "Edit ends of lines" "e" #'mc/edit-ends-of-lines
     :desc "Edit beginnings of lines" "b" #'mc/edit-beginnings-of-lines)

    (:prefix ("C-c n" . "Notes")
     :desc "Archive note" "A"               #'my/denote-archive-note
     :desc "Denote backlinks (xref)" "b"    #'denote-backlinks
     :desc "Backlinks + preview" "B"        #'my/denote-insert-backlinks-context
     :desc "Backlinks preview (virtual)" "v" #'my/denote-backlinks-preview
     :desc "Denote link after creating" "c" #'denote-link-after-creating
     :desc "Denote dired" "d"               #'my/denote-dired
     :desc "Denote feed" "e"                #'my/denote-feed
     :desc "Denote find" "f"                #'my/denote-find
     :desc "Denote grep" "g"                #'denote-grep
     (:prefix ("G" . "Graph")
      :desc "Notes graph (browser)" "g"     #'my/denote-graph-notes
      :desc "Graph (choose type)" "G"       #'denote-explore-network
      :desc "Regenerate graph" "r"          #'denote-explore-network-regenerate
      :desc "Orphan notes" "o"              #'denote-explore-isolated-files
      :desc "Keyword barchart" "k"          #'denote-explore-barchart-keywords
      :desc "Random note" "n"               #'denote-explore-random-note)
     :desc "Open inbox" "i"                 #'my/open-inbox
     :desc "Sticky note (quick lookup)" "q" #'my/denote-sticky
     :desc "Insert tag index" "#"           #'my/denote-insert-tag-index
     (:prefix ("j" . "Journal")
      :desc "Today's journal" "j"           #'denote-journal-new-or-existing-entry
      :desc "Tomorrow's journal" "t"        #'my/open-tomorrow-journal
      :desc "Yesterday's journal" "y"       #'my/open-yesterday-journal)
     :desc "Open Journelly" "J"             #'my/open-journelly
     :desc "Journelly by tag" "T"            #'my/journelly-search-tag
     :desc "Denote link" "l"                #'denote-link
     :desc "Denote add links" "L"            #'denote-add-links
     :desc "New denote" "n"                 #'denote
     :desc "New denote (extra)" "N"          #'my/denote-create-in-extra
     :desc "Denote rename" "r"              #'denote-rename-file
     :desc "Consult notes" "s"              #'consult-notes
     :desc "Search all notes" "S"           #'consult-notes-search-in-all-notes
     :desc "Copy as rich text (HTML)" "m"   #'my/org-copy-rich-html)

    (:prefix ("C-c o" . "Open")
     :desc "Dired jump" "-"         #'dired-jump
     :desc "Dual panel file manager" "2" #'my/dirvish-dual-panel
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
     :desc "Search project for word at point" "S" (lambda () (interactive)
                                                    (consult-ripgrep (projectile-project-root)
                                                                     (thing-at-point 'symbol t)))
     (:prefix ("t" . "TODOs")
      :desc "Next TODO in buffer" "n"    #'hl-todo-next
      :desc "Prev TODO in buffer" "p"    #'hl-todo-previous
      :desc "Search TODOs" "s" (lambda () (interactive)
                                 (if (projectile-project-p)
                                     (consult-ripgrep
                                      (projectile-project-root)
                                      ;; every hl-todo keyword, |-joined as
                                      ;; an rg alternation pattern
                                      (mapconcat (lambda (k) (concat (car k) ":"))
                                                 hl-todo-keyword-faces "|"))
                                   (message "Not in a project")))))

    (:prefix ("C-c v" . "Versioning")
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
     :desc "Keycast log mode" "K" #'keycast-log-mode
     :desc "Keycast header mode" "k" #'keycast-header-line-mode
     :desc "Flymake" "f"               #'flymake-mode
     :desc "Indent guides" "i"         #'indent-bars-mode
     :desc "Indent style" "I"          #'doom/toggle-indent-style
     :desc "Link display" "l"           #'org-toggle-link-display
     :desc "Line numbers" "L"          #'doom/toggle-line-numbers
     :desc "Inline images" "p"          #'org-toggle-inline-images
     :desc "Read-only" "r"             #'read-only-mode
     :desc "Soft line wrapping" "w"    #'+word-wrap-mode
     :desc "Copilot" "a"               #'copilot-mode
     :desc "Prose mode" "P"             #'prose-mode
     :desc "Truncate lines" "t"        #'toggle-truncate-lines
     :desc "Zoom window" "z"           #'my/toggle-window-zoom
     :desc "Zen mode (fullscreen)" "Z" #'+zen/toggle-fullscreen
     (:prefix ("T" . "Timing")
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
        :desc "imenu"           "M-i" #'imenu)
  ;; Uses :map override to take priority over magit-section's M-1..4
  ;; (show-level-all). Use plain 1/2/3/4 in magit.
  ;; TEMPORARY: M-1..9 select TABS while testing the submodules-as-tabs
  ;; flow; to revert, point the digits back at +workspace/switch-to-N.
  (map! :map override
        :desc "Tab 1"          "M-1" (cmd! (tab-bar-select-tab 1))
        :desc "Tab 2"          "M-2" (cmd! (tab-bar-select-tab 2))
        :desc "Tab 3"          "M-3" (cmd! (tab-bar-select-tab 3))
        :desc "Tab 4"          "M-4" (cmd! (tab-bar-select-tab 4))
        :desc "Tab 5"          "M-5" (cmd! (tab-bar-select-tab 5))
        :desc "Tab 6"          "M-6" (cmd! (tab-bar-select-tab 6))
        :desc "Tab 7"          "M-7" (cmd! (tab-bar-select-tab 7))
        :desc "Tab 8"          "M-8" (cmd! (tab-bar-select-tab 8))
        :desc "Tab 9"          "M-9" (cmd! (tab-bar-select-tab 9))
        :desc "Last workspace" "M-`" #'+workspace/other
        :desc "Kill workspace"     "M-D" #'+workspace/kill
        :desc "Display workspaces" "M-W" #'+workspace/display
        :desc "Switch workspace"   "M-S" #'+workspace/switch-to))

  ;; ── Consult (M-s prefix) — better search / navigation ────────────────────────
  (map! :map override
        :desc "consult-line (current buffer)"      "M-s l" #'consult-line
        :desc "consult-line-multi (all buffers)"   "M-s L" #'consult-line-multi
        :desc "consult-ripgrep (dir/project)"      "M-s r" #'consult-ripgrep
        :desc "consult-outline (headings)"         "M-s O" #'consult-outline
        :desc "consult-buffer"                     "M-s b" #'consult-buffer
        :desc "consult-imenu (symbols in buffer)"  "M-s i" #'consult-imenu
        :desc "consult-imenu-multi"                "M-s I" #'consult-imenu-multi
        :desc "consult-mark"                       "M-s m" #'consult-mark
        :desc "consult-global-mark"                "M-s M" #'consult-global-mark
        :desc "consult-kmacro"                     "M-s k" #'consult-kmacro
        :desc "consult-find / fd"                  "M-s f" #'consult-find
        :desc "consult-recent-file"                "M-s R" #'consult-recent-file)

;; C-x C-a: sessions on plain letters (lowercase = quick, capitals = named
;; files); workspace commands ALL Ctrl-held, so the whole chord flows
;; without releasing Ctrl (C-x C-a C-s = save workspace, etc.)
(map! "C-x C-a s"   #'doom/quicksave-session
      "C-x C-a l"   #'doom/quickload-session
      "C-x C-a S"   #'doom/save-session
      "C-x C-a L"   #'doom/load-session
      "C-x C-a C-n" #'+workspace/new
      "C-x C-a C-r" #'+workspace/rename
      "C-x C-a C-k" #'+workspace/kill
      "C-x C-a C-1" #'my/workspace-kill-others
      "C-x C-a C-s" #'+workspace/save
      "C-x C-a C-l" #'+workspace/load)

;; Prevent projectile and persp-mode from binding their keymaps to C-c p.
;; Prevent lsp-mode from binding its keymap to C-c l (we use that for Tools).
;; Must be set before these modes activate.
(setq projectile-keymap-prefix nil
      persp-keymap-prefix (kbd "C-c P")
      lsp-keymap-prefix "C-c L")

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

;; Explicitly register sub-prefix group names for C-c l (which-key doesn't
;; always pick these up when map! nests them inside :map override)
(after! which-key
  (which-key-add-key-based-replacements
    "C-c i j" "Journal"
    "C-c i i" "Images"
    "C-c i o" "Org"
    "C-c l e" "ERC IRC"
    "C-c l f" "Frames"
    "C-c l p" "Process"
    "C-c l s" "Shell"
    "C-c l d" "Database"))

;; ── Disable arrow keys ────────────────────────────────────────────────────────
(map! "<left>"  #'ignore
      "<right>" #'ignore
      "<up>"    #'ignore
      "<down>"  #'ignore)

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


;; (after! elisp-mode
;;   (map! :map emacs-lisp-mode-map
;;         "C-c m" #'macrostep-expand))

(after! casual
  (map! :map isearch-mode-map "C-o" #'casual-isearch-tmenu)
  (with-eval-after-load 'calc-alg
    (when (boundp 'calc-alg-map)
      (map! :map calc-alg-map "C-o" #'casual-calc-tmenu)))
  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (define-key ediff-mode-map (kbd "C-o") #'casual-ediff-tmenu))))

(after! dirvish
  (map! :map dirvish-mode-map
        "`"     #'dirvish-quick-access
        "C-o"   #'my/casual-open        ; casual-dired-tmenu (overrides dired-display-file)
        "C-f"   #'dired-find-file
        "C-b"   #'dired-up-directory
        "TAB"   #'dirvish-subtree-toggle
        "M-TAB" #'dirvish-layout-toggle
        "2"     #'my/dirvish-dual-panel
        "V"     #'my/dired-toggle-details
        "H"     #'my/dired-toggle-hidden)
  (map! :map dired-mode-map
        "2"     #'my/dirvish-dual-panel
        "H"     #'my/dired-toggle-hidden))

(after! dired
  (map! :map dired-mode-map
        "V" #'my/dired-toggle-details))


;; After one C-c p t n/p, keep jumping between TODOs with bare n/p (repeat-mode)
(defvar-keymap my/hl-todo-repeat-map
  :repeat t
  "n" #'hl-todo-next
  "p" #'hl-todo-previous)

(after! verb
  (map! :map org-mode-map "C-c C-r" verb-command-map))

;; Refile prefix (C-c r) — in org buffers AND the agenda. The refile commands are
;; state-agnostic (work on TODO, NEXT, WAIT, plain headings — anything at point).
(map! :map org-mode-map
      (:prefix ("C-c r" . "Refile")
       :desc "Refile to today" "t"      #'my/refile-to-today-journal
       :desc "Refile ALL to today" "T"  #'my/refile-all-to-today-journal))

(after! org-agenda
  (map! :map org-agenda-mode-map
        (:prefix ("C-c r" . "Refile")
         :desc "Refile to today" "t"      #'my/refile-to-today-journal
         :desc "Refile ALL to today" "T"  #'my/refile-all-to-today-journal)))

(after! copilot
  (map! :map copilot-completion-map
        "<tab>"   #'copilot-accept-completion
        "TAB"     #'copilot-accept-completion
        "C-TAB"   #'copilot-accept-completion-by-word
        "C-<tab>" #'copilot-accept-completion-by-word
        "M-n"     #'copilot-next-completion
        "M-p"     #'copilot-previous-completion))


;; With a daemon, C-x C-c and ⌘Q kill the whole session — and
;; `confirm-kill-emacs' is nil, so there is no prompt. Close the frame instead;
;; `M-x kill-emacs' still ends the daemon deliberately.
(defun my/close-frame-or-kill-emacs ()
  "Delete this frame; kill Emacs only if it is the last one and no daemon."
  (interactive)
  (if (or (daemonp) (cdr (frame-list)))
      (delete-frame)
    (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") #'my/close-frame-or-kill-emacs)
(global-set-key (kbd "s-q")     #'my/close-frame-or-kill-emacs)

;; A frameless daemon keeps its app presence — focused, menu bar, no window.
;; Hide the app when the last GUI frame closes; ec brings it back.
(defun my/ns-hide-when-frameless (frame)
  (when (and (fboundp 'ns-hide-emacs)
             (not (seq-some (lambda (f)
                              (and (not (eq f frame)) (display-graphic-p f)))
                            (frame-list))))
    (ns-hide-emacs t)))
(add-hook 'delete-frame-functions #'my/ns-hide-when-frameless)
