;;; +os.el -*- lexical-binding: t; -*-

(xterm-mouse-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
;; (setq persp-auto-save-opt 1)  ; Auto-save workspaces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOCKER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure docker package to use podman instead of docker

(setq docker-command "podman")
(setq docker-compose-command "podman-compose")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRASH
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delete to trash
(setq delete-by-moving-to-trash t)

;; using trash over rm
(when (executable-find "trash")
  (os--trash-setup))
