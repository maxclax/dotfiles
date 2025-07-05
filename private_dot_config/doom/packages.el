;; LSP-related packages
(package! lsp-pyright)  ; Python LSP client using Pyright
(package! lsp-ui)       ; UI enhancements for LSP
(package! vue-mode)     ; Vue.js mode
(package! company-box)  ; Better company UI


(package! format-all)
(package! tldr)
(package! keycast)
(package! pomm)

(package! aider)
(package! copilot)
(package! gptel :recipe (:nonrecursive t))

(if (modulep! :tools lsp +eglot)
    (progn
      (package! breadcrumb :recipe (:host github :repo "joaotavora/breadcrumb")))
  (progn
    (package! lsp-docker)))
