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



;; AI → lisp/init-ai.el
(package! copilot)
(package! gptel :recipe (:nonrecursive t))
(package! aidermacs)
(package! aider :recipe (:host github :repo "tninja/aider.el"))

;; denote - note-taking system → lisp/init-denote.el
(package! denote)
(package! denote-journal)
(package! denote-explore)
(package! denote-silo)
(package! denote-markdown)
(package! denote-org)
(package! denote-sequence)
(package! denote-menu)
(package! consult-denote)
(package! citar-denote)
(package! consult-notes)

;; bibliography management → lisp/init-bibliography.el
(package! citar)
(package! biblio)

;; org enhancements → lisp/init-org.el
(package! org-appear)
(package! org-ql)
(package! org-super-agenda)
(package! org-download)
(package! calfw)
(package! calfw-org)

;; timing → lisp/init-timing.el
(package! pomm)

;; text
(package! adoc-mode)
(package! tldr)
(package! symbol-overlay)
(package! literate-calc-mode)
(package! nov)      ; EPUB reader
(package! djvu)     ; DjVu document viewer
(package! elfeed)   ; RSS reader
(package! elfeed-org) ; Org-mode integration for elfeed



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
