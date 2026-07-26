;;; init-docker.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Configure docker package to use podman instead of docker

(setq docker-command "podman")
(setq docker-compose-command "podman-compose")

(provide 'init-docker)
