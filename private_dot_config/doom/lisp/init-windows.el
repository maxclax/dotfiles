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

;; ── Zoom one window to the whole frame, and back ────────────────────────────
;; tmux's `prefix z'. `delete-other-windows' alone throws the layout away and
;; leaves you relying on `winner-undo', which walks a general history and so
;; does not necessarily land back where you were. This stashes the exact
;; window configuration on the frame and restores it verbatim.

(defun my/toggle-window-zoom ()
  "Maximise the selected window, or restore the layout it replaced.
Nothing is killed: buffers, point and scroll positions all come back.
Splitting again while zoomed discards the stashed layout, so the next
invocation zooms the new arrangement rather than jumping back to a
layout that no longer makes sense."
  (interactive)
  (let ((wconf (frame-parameter nil 'my/window-zoom-wconf)))
    (if (and wconf (one-window-p))
        (progn
          (set-frame-parameter nil 'my/window-zoom-wconf nil)
          (set-window-configuration wconf)
          (message "Layout restored"))
      (if (one-window-p)
          (message "Only one window — nothing to zoom")
        (set-frame-parameter nil 'my/window-zoom-wconf
                             (current-window-configuration))
        (delete-other-windows)
        (message "Zoomed — press again to restore")))))

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



;; Horizontal trackpad scrolling: mwheel's left/right branches are gated on
;; this, nil by default. Only visible where lines are truncated.
(setq mouse-wheel-tilt-scroll t)

(provide 'init-windows)
