;;; +os.el -*- lexical-binding: t; -*-

(setq persp-auto-save-opt 1)  ; Auto-save workspaces

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DOCKER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure docker package to use podman instead of docker

(setq docker-command "podman")
(setq docker-compose-command "podman-compose")

