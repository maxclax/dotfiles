;; LSP-related packages

;; disabled packages
(disable-packages! solaire-mode
                   osx-trash
                   realgud
                   realgud-trepan-ni
                   ccls
                   tide
                   swiper
                   forge
                   code-review
                   writegood-mode
                   dired-x
                   flymake-popon
                   lsp-python-ms
                   pyimport)


;; AI
(package! mcp)
(package! copilot)
(package! gptel :recipe (:nonrecursive t))
(package! claude-code-ide
  :recipe (:host github :repo "manzaltu/claude-code-ide.el"))

;; text
(package! adoc-mode)
(package! tldr)
(package! pomm)
(package! symbol-overlay)
(package! org-appear)
(package! nov)      ; EPUB reader
(package! djvu)     ; DjVu document viewer

;; denote - note-taking system
(package! denote)
(package! denote-explore)
(package! consult-notes)

;; misc
(package! makefile-executor)
(package! circadian)
(package! prodigy)
(package! beacon)
(package! format-all)
(package! keycast)
(package! key-chord)
(package! evil-string-inflection)
(package! all-the-icons-ibuffer)
(package! git-link)
(package! magit-delta)
(package! magit-todos)
(package! imenu-list)
(package! tmux-pane)
(package! gt)
(package! simpleclip)  ; Better clipboard integration
(package! consult-todo)
(package! restclient)
(package! vimrc-mode)
(package! eat)         ; Terminal emulator package
(package! vdiff)

;; programming
(package! jinja2-mode)
(package! graphql-mode)

(if (modulep! :tools lsp +eglot)
    (progn
      (package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb"))
      (package! eglot-java))
  (progn
    (package! lsp-docker)))

;; PostgreSQL-related packages
(package! pgmacs
  :recipe (:host github :repo "emarsden/pgmacs"))
