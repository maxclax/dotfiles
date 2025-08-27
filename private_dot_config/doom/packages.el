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

(package! lsp-pyright)  ; Python LSP client using Pyright
(package! lsp-ui)       ; UI enhancements for LSP
(package! vue-mode)     ; Vue.js mode

;; AI and LSP tools
(package! aidermacs)
(package! aider)
(package! copilot)
(package! mcp)
(package! gptel :recipe (:nonrecursive t))

;; text
(package! adoc-mode)
(package! tldr)
(package! pomm)
(package! symbol-overlay)
(package! org-appear)

;; misc
(package! circadian)
(package! beacon)
(package! clippy)
(package! rainbow-mode)
(package! format-all)
(package! keycast)
(package! key-chord)
(package! evil-string-inflection)
(package! all-the-icons-ibuffer)
(package! git-link)
(package! magit-delta)
(package! imenu-list)
(package! tmux-pane)
(package! gt)
(package! xclip)
(package! simpleclip)  ; Better clipboard integration
(package! consult-todo)
(package! restclient)
(package! vimrc-mode)

;; programming
(package! jinja2-mode)
(if (modulep! :tools lsp +eglot)
    (progn
      (package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb"))
      (package! eglot-java))
  (progn
    (package! lsp-docker)))

;; PostgreSQL-related packages
(package! pgmacs
  :recipe (:host github :repo "emarsden/pgmacs"))
