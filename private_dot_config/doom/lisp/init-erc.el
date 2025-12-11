;;; lisp/init-erc.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:
;; ERC IRC client configuration - uses private variables for credentials

;; Quick connect function (available immediately)
(defun my/erc-connect ()
  "Connect to Libera Chat with SSL and auto-join channels."
  (interactive)
  (require 'erc)
  (let ((nick (if (boundp 'my/irc-nick)
                  my/irc-nick
                (format "guest-%04d" (random 9999)))))
    (erc-tls :server "irc.libera.chat"
             :port 6697
             :nick nick)))

;; Join additional channels manually
(defun my/erc-join-extra ()
  "Join additional channels not in auto-join list."
  (interactive)
  (require 'erc)
  (dolist (channel '("#nix" "#nixos" "#linux"))
    (erc-cmd-JOIN channel)))

;; Open all ERC buffers in side-by-side windows
(defun my/erc-show-all-buffers ()
  "Open all ERC channel buffers (not server) in side-by-side windows in current frame."
  (interactive)
  (require 'erc)
  (let ((erc-buffers (seq-filter (lambda (buf)
                                   (with-current-buffer buf
                                     (and (erc-channel-p (erc-default-target))
                                          (not (erc-server-buffer-p)))))
                                 (erc-buffer-list))))
    (if (null erc-buffers)
        (message "No ERC buffers found")
      ;; Delete other windows first
      (delete-other-windows)
      ;; Show first buffer in current window
      (switch-to-buffer (car erc-buffers))
      ;; Split for remaining buffers
      (dolist (buffer (cdr erc-buffers))
        (split-window-right)
        (other-window 1)
        (switch-to-buffer buffer))
      ;; Balance windows to equal sizes
      (balance-windows)
      ;; Ensure equal width distribution
      (let ((window-count (length erc-buffers))
            (frame-width (frame-width)))
        (when (> window-count 1)
          (dotimes (i window-count)
            (select-window (nth i (window-list)))
            (window-resize (selected-window)
                          (- (/ frame-width window-count) (window-width))
                          t))))
      (message "Opened %d ERC buffers with equal sizes" (length erc-buffers)))))


(after! erc
  ;; Basic server settings
  (setq erc-server "irc.libera.chat"
        erc-port 6697
        erc-use-tls t)

  ;; Use private nick from private-vars.el if available
  (when (boundp 'my/irc-nick)
    (setq erc-nick my/irc-nick))

  ;; Generate random nick if private nick not set
  (unless (boundp 'my/irc-nick)
    (setq erc-nick (format "guest-%04d" (random 9999))))

  ;; Auto-join favorite channels after connect
  (setq erc-autojoin-channels-alist
        '(("libera.chat" "#emacs" "#org-mode")))

  ;; Logging to home directory
  (setq erc-log-channels-directory (expand-file-name "erc-logs" (getenv "HOME")))
  (setq erc-save-buffer-on-part nil
        erc-save-queries-on-quit t
        erc-log-write-after-send t
        erc-log-write-after-insert t)

  ;; Create log directory if it doesn't exist
  (unless (file-exists-p erc-log-channels-directory)
    (make-directory erc-log-channels-directory t))

  ;; Enable logging
  (erc-log-mode 1)

  ;; Hide join/part noise
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))

  ;; Channel list configuration
  (setq erc-channel-list-default-sort-function 'erc-channel-list-sort-by-users)

  ;; Custom /commands
  (defun erc-cmd-POPULAR (&rest ignore)
    "List channels with 100+ users."
    (erc-cmd-LIST ">100")))

(provide 'init-erc)
