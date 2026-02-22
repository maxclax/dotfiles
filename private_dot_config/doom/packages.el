;; LSP-related packages

;; disabled packages
(disable-packages! solaire-mode
                   writegood-mode
                   dired-x
                   flymake-popon
                   pyimport)

(package! tldr)
(package! lorem-ipsum)
(package! vundo)
(package! undo-tree)

;; =====================================================================
;; AI ASSISTANCE & CODE COMPLETION
;; =====================================================================
;; AI tools and LLM integration for enhanced coding assistance
;; → lisp/init-ai.el

(package! copilot)                    ; GitHub Copilot AI code completion

;; =====================================================================
;; NOTE-TAKING & KNOWLEDGE MANAGEMENT
;; =====================================================================
;; Comprehensive note-taking system with organization and search
;; → lisp/init-denote.el

(package! denote)                     ; Simple and powerful note-taking system
(package! denote-journal)             ; Journal entries integration with denote
(package! denote-explore)             ; Explore and navigate between notes
(package! denote-markdown)            ; Markdown support for denote
(package! denote-org)                 ; Org-mode support for denote
(package! denote-sequence)            ; Sequential note-taking workflows
(package! denote-menu)                ; Menu interface for note operations
(package! consult-denote)             ; Consult integration for fast note search
(package! citar-denote)               ; Bibliography integration with denote
(package! consult-notes)              ; General note searching with consult

(package! tempel)
;; (package! tempel-collection)
(package! eglot-tempel)

;; =====================================================================
;; BIBLIOGRAPHY & ACADEMIC TOOLS
;; =====================================================================
;; Research paper and citation management system
;; → lisp/init-bibliography.el

(package! citar)                   ; Citation management with Helm/Consult (disabled)
(package! biblio)                  ; Academic paper lookup and management (disabled)

;; =====================================================================
;; ORG-MODE ENHANCEMENTS
;; =====================================================================
;; Extended functionality for Emacs Org-mode
;; → lisp/init-org.el

(package! org-appear)                 ; Auto-hide/show markup in Org-mode
(package! org-ql)                     ; Query language for Org-mode files
(package! org-super-agenda)           ; Enhanced agenda views and grouping
(package! org-download)               ; Drag-and-drop image/link insertion

;; =====================================================================
;; TIME MANAGEMENT & PRODUCTIVITY
;; =====================================================================
;; Time tracking and productivity enhancement tools
;; → lisp/init-timing.el

(package! pomm)                       ; Pomodoro timer and productivity tracker
(package! tmr)                        ; TMR May Ring (tmr)

;; =====================================================================
;; CASUAL — TRANSIENT MENUS FOR BUILT-IN MODES
;; =====================================================================
;; C-o opens contextual menu in: dired, ibuffer, isearch, agenda, calc, info
;; → lisp/init-casual.el

(package! casual)                     ; Keyboard-driven menus via Transient

;; =====================================================================
;; UI ENHANCEMENT & VISUAL TOOLS
;; =====================================================================

(package! circadian)                  ; Automatic theme switching (day/night modes)
(package! beacon)                     ; Highlight cursor position after scrolling/jumping
(package! keycast)                    ; Display current command/key sequence in mode line
(package! page-break-lines)           ; Display ^L characters as horizontal lines

;; =====================================================================
;; GIT INTEGRATION & VERSION CONTROL
;; =====================================================================

(package! git-link)                   ; Generate GitHub/GitLab URLs for current file/line

;; =====================================================================
;; PRODUCTIVITY & WORKFLOW AUTOMATION
;; =====================================================================

(package! makefile-executor)          ; Execute Makefile targets with ease
(package! prodigy)                    ; Process management for development servers/services
(package! format-all)                 ; Universal code formatter supporting many languages
(package! consult-todo)               ; Browse and jump to TODO items using consult

;; =====================================================================
;; DATABASE TOOLS
;; =====================================================================
;; Database clients and utilities for various database systems
;; → lisp/init-pgmacs.el

(package! pg)                         ; PostgreSQL library for Emacs
(package! pgmacs
  :recipe (:host github :repo "emarsden/pgmacs")) ; PostgreSQL client for Emacs

;; =====================================================================
;; DEVELOPMENT TOOLS & TERMINAL
;; =====================================================================

(package! restclient)                 ; HTTP requests testing and API exploration
(package! verb)                       ; HTTP client integrated with org-mode
(package! eat)                        ; Terminal emulator package for running shells
(package! magit-delta)                ; Delta syntax highlighting in Magit diffs
(package! browse-kill-ring)           ; Visual kill ring browser
