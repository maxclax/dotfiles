;;; lisp/init-windows.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:
;; Window management configuration including winner mode and split functions

(use-package! winner
  :init
  (winner-mode 1)
  :config
  ;; Make winner keys repeatable like in Purcell
  (defvar my/winner-repeat-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<left>") #'winner-undo)
      (define-key map (kbd "<right>") #'winner-redo)
      map)
    "Keymap for repeating winner commands.")

  ;; Enable repeat mode for winner commands
  (when (fboundp 'repeat-mode)
    (put 'winner-undo 'repeat-map 'my/winner-repeat-map)
    (put 'winner-redo 'repeat-map 'my/winner-repeat-map)))

;; Rearrange split windows
(defun my/split-window-horizontally-instead ()
  "Kill any other windows and re-split such that the current window is on the top half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-horizontally)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))

(defun my/split-window-vertically-instead ()
  "Kill any other windows and re-split such that the current window is on the left half of the frame."
  (interactive)
  (let ((other-buffer (and (next-window) (window-buffer (next-window)))))
    (delete-other-windows)
    (split-window-vertically)
    (when other-buffer
      (set-window-buffer (next-window) other-buffer))))


(provide 'init-windows)