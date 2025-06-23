;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "os")
(load! "git")
(load! "misc")
(load! "text")
(load! "prog")
(load! "ui")
(load! "keys")

(setq org-directory "~/org/")

;; Load system profile for different machines and work config
(dolist (config '("~/.config/doom/local.el"))
  (let ((config-file (file-truename config)))
    (when (file-exists-p config-file)
      (load-file config-file))))
