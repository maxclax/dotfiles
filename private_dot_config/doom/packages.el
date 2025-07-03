;; LSP-related packages
(package! lsp-pyright)  ; Python LSP client using Pyright
(package! lsp-ui)       ; UI enhancements for LSP
(package! vue-mode)     ; Vue.js mode
(package! company-box)  ; Better company UI


(package! tldr)
(package! keycast)
(package! pomm)

(package! aider)
(package! gptel :recipe (:nonrecursive t))
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
