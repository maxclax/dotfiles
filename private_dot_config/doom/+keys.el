;;; +keys.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

(map! :leader
      :desc "Select all in buffer" "ba" #'(lambda () (interactive) (mark-whole-buffer))
      :desc "Make buffer empty without yank" "be" #'(lambda () (interactive) (let ((inhibit-read-only t)) (erase-buffer)))
      :desc "Open Dirfish (Project Directory)" "e" #'dirvish-side)
      ;; :desc "Open Treemacs" "e" #'treemacs)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map! :leader
      :desc "Switch to perspective NAME"       "DEL" #'persp-switch
      :desc "Switch to buffer in perspective"  "," #'persp-switch-to-buffer
      :desc "Switch to next perspective"       "]" #'persp-next
      :desc "Switch to previous perspective"   "[" #'persp-prev
      :desc "Add a buffer current perspective" "+" #'persp-add-buffer
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

;; Toogles
(map! :leader
      :prefix "t"
      :desc "Pomm-third timer" "t" #'pomm-third-time
      "k" #'keycast-log-mode
      "R" #'rainbow-mode)

;; Own commands
(map! :leader
      (:prefix ("lp" . "Podman")
       :desc "Podman machine start" "s" (lambda () (interactive) (compile "cd ~/ && podman machine start"))
       :desc "Podman machine stop" "S" (lambda () (interactive) (compile "cd ~/ && podman machine stop")))
      (:prefix ("la" . "AI")
       :desc "Aider menu" "a" #'aider-transient-menu
       :desc "GPTel" "g" #'gptel)
      :prefix "l"
      :desc "Services (prodigy)" "s" #'prodigy
      :desc "List process" "t" #'list-processes
      :desc "Open LINK under cursor" "l" #'browse-url-at-point
      :desc "Start backup" "b" (lambda () (interactive) (compile "cd ~/ && make backup_create"))
      :desc "Update all" "u" (lambda () (interactive) (compile "cd ~/ && make update_apps && make update_macos"))
      :desc "Chezmoi apply" "c" (lambda () (interactive) (compile "cd ~/ && chezmoi apply"))
       "x" #'align-regexp)
