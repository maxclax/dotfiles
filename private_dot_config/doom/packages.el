;; LSP-related packages
(package! lsp-pyright)  ; Python LSP client using Pyright
(package! lsp-ui)       ; UI enhancements for LSP
(package! vue-mode)     ; Vue.js mode

;; text
(package! adoc-mode)
(package! tldr)
(package! pomm)
(package! symbol-overlay)
(package! org-appear)

;; misc
(package! beacon)
(package! clippy)
(package! rainbow-mode)
(package! format-all)
(package! keycast)
(package! all-the-icons-ibuffer)
(package! git-link)
(package! magit-delta)
(package! imenu-list)
(package! tmux-pane)
(package! xclip)
(package! consult-todo)
(package! restclient)

;; programming
(package! jinja2-mode)
(package! aider)
(package! copilot)
(package! gptel :recipe (:nonrecursive t))

(if (modulep! :tools lsp +eglot)
    (progn
      (package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb")))
  (progn
    (package! lsp-docker)))
