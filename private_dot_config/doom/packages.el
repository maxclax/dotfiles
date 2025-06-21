;; -*- no-byte-compile: t; -*-

;;; packages.el

;; disabled packages
;; (disable-packages! solaire-mode
;;                    osx-trash)

;; text
(package! adoc-mode)
(package! tldr)

;; (package! youdao-dictionary)
(package! symbol-overlay)
(package! pomm)
(package! org-appear)

;; misc
(package! format-all)
(package! keycast)
(package! evil-string-inflection)
(package! all-the-icons-ibuffer)
(package! atomic-chrome)
(package! git-link)
(package! magit-delta)
(package! imenu-list)
(package! tmux-pane)
(package! go-translate)
(package! xclip)
(package! consult-todo)

;; programming
(package! bazel)
(package! jinja2-mode)
(package! vue-mode)
(package! django-mode)
(package! protobuf-mode)
(package! gn-mode)
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("aider.el" "aider-doom.el")))
