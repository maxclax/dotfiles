;;; lisp/init-chess.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;; Chess in Emacs (using the classic emacs-chess package)
;; Main package: https://github.com/jwiegley/emacs-chess
;; Blog post: https://en.andros.dev/blog/d8b3a759/playing-chess-online-with-emacs/
;;
;; Ways to play online:
;; - Internet Chess Servers (ICS): M-x chess-ics
;; - Local network: C-u M-x chess RET network RET
;; - IRC: M-x chess-irc

(use-package! chess
  :config
  (setq chess-images-separate-frame nil   ; Show board in the current frame
        chess-images-default-size 100))

(provide 'init-chess)
