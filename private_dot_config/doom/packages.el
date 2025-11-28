;; LSP-related packages

;; ;; disabled packages
;; (disable-packages! solaire-mode
;;                    osx-trash
;;                    realgud
;;                    realgud-trepan-ni
;;                    ccls
;;                    tide
;;                    swiper
;;                    forge
;;                    code-review
;;                    writegood-mode
;;                    dired-x
;;                    flymake-popon
;;                    lsp-python-ms
;;                    pyimport)

;; =====================================================================
;; AI ASSISTANCE & CODE COMPLETION
;; =====================================================================
;; AI tools and LLM integration for enhanced coding assistance
;; → lisp/init-ai.el

(package! copilot)                    ; GitHub Copilot AI code completion
(package! gptel :recipe (:nonrecursive t)) ; ChatGPT/LLM interface in Emacs
(package! aidermacs)               ; AI coding assistant (disabled)
(package! aider :recipe (:host github :repo "tninja/aider.el")) ; Alternative AI assistant

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

;; =====================================================================
;; UI ENHANCEMENT & VISUAL TOOLS
;; =====================================================================

(package! circadian)                  ; Automatic theme switching (day/night modes)
(package! beacon)                     ; Highlight cursor position after scrolling/jumping
(package! keycast)                    ; Display current command/key sequence in mode line
;;(package! all-the-icons-ibuffer)      ; Add file type icons to buffer list

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
;; DEVELOPMENT TOOLS & TERMINAL
;; =====================================================================

(package! restclient)                 ; HTTP requests testing and API exploration
(package! eat)                        ; Terminal emulator package for running shells
