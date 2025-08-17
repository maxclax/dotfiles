;;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :leader
      :desc "Switch to perspective NAME"       "DEL" #'persp-switch
      :desc "Switch to buffer in perspective"  "," #'persp-switch-to-buffer
      :desc "Remove perspective by name"       "-" #'persp-remove-by-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REGISTERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :leader
      (:prefix ("r" . "registers")
       :desc "Copy to register" "c" #'copy-to-register
       :desc "Frameset to register" "f" #'frameset-to-register
       :desc "Insert contents of register" "i" #'insert-register
       :desc "Jump to register" "j" #'jump-to-register
       :desc "List registers" "l" #'list-registers
       :desc "Number to register" "n" #'number-to-register
       :desc "Interactively choose a register" "r" #'counsel-register
       :desc "View a register" "v" #'view-register
       :desc "Window configuration to register" "w" #'window-configuration-to-register
       :desc "Increment register" "+" #'increment-register
       :desc "Point to register" "SPC" #'point-to-register))

;; Jumps
(map! :leader
      :prefix "j"
      "j" #'avy-goto-char-timer
      "l" #'avy-goto-line
      "b" #'avy-pop-mark
      "t" #'yas-describe-tables)

;; Files
(map! :leader
      :prefix "f"
      :desc "Find file in project" "f" #'projectile-find-file
      :desc "Yank filename" "n" #'+default/yank-filename
      :desc "Dirvish" "j" #'dirvish-dwim)

;; Buffers
(map! :leader
      :prefix "b"
      :desc "Select all in buffer" "a" #'(lambda () (interactive) (mark-whole-buffer))
      :desc "Make buffer empty without yank" "e" #'(lambda () (interactive) (let ((inhibit-read-only t)) (erase-buffer))))

;; Open
(map! :leader
      :prefix "o"
      :desc "Open link" "x" #'link-hint-open-link
      :desc "Open link at point" "X" #'link-hint-open-link-at-point)

;; Toogles
(map! :leader
      :prefix "t"
      :desc "Pomm-third timer" "t" #'pomm-third-time
      "k" #'keycast-log-mode
      "R" #'rainbow-mode)

(map! :leader
      :prefix "l"
      ("x" #'align-regexp
       (:prefix ("p" . "Process Management")
        "p" #'prodigy
        "l" #'list-processes
        :desc "podman machine start" "s" (lambda () (interactive) (compile "cd ~/ && podman machine start"))
        :desc "podman machine stop" "S" (lambda () (interactive) (compile "cd ~/ && podman machine stop")))
       (:prefix ("a" . "Aider") "a" #'aider-transient-menu)
       (:prefix ("g" . "GPTel")
                "a" #'gptel-add
                "g" #'gptel
                "r" #'gptel-rewrite
                "s" #'gptel-send
                "m" #'gptel-menu
                "M" #'mcp-hub
                "S" #'mcp-hub-start-server
                "t" #'gptel-tools
                "f" #'gptel-add-file)
       (:prefix ("s" . "System Management")
        :desc "make backup_create" "b" (lambda () (interactive) (compile "cd ~/ && make backup_create"))
        :desc "make update_macos" "u" (lambda () (interactive) (compile "cd ~/ && make update_apps && make update_macos"))
        :desc "chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply")))))
