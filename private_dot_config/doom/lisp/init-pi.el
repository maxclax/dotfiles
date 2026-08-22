;;; lisp/init-pi.el -*- mode: emacs-lisp; lexical-binding: t; -*-

;;; Commentary:
;; Emacs frontend for the pi coding agent (https://github.com/dnouri/pi-coding-agent).
;;
;; Unlike gptel, this drives the `pi' CLI as an agent: it edits files, runs
;; commands, and keeps sessions. The CLI is installed by
;; .chezmoiscripts/darwin/run_onchange_after_11-ai-tools.sh, so the package is
;; guarded on its presence rather than failing loudly on a machine without it.
;;
;; Providers come from pi's own config (~/.pi/agent/), not from here — including
;; the local llama.cpp router that shell/exports.sh points at.
;;
;; No evil integration: pi-coding-agent-evil.el exists but this config is
;; non-evil, and every documented binding is already plain Emacs (C-c C-c to
;; send, C-c C-p for the transient menu, C-c C-r to resume).

;;; Code:

(use-package! pi-coding-agent
  :when (executable-find "pi")
  :commands (pi-coding-agent pi-coding-agent-open-input)
  :init
  ;; `pi' is a much better name to type than the full command.
  (defalias 'pi #'pi-coding-agent)
  :config
  ;; Completed reasoning collapses; live thinking still streams. Keeps long
  ;; sessions readable without losing the ability to expand a block with TAB.
  (setopt pi-coding-agent-thinking-display 'hidden)

  ;; Keep the input pane visible (the package default). `on-demand' and
  ;; `hidden' both hide it after every send, which means reaching for
  ;; `pi-coding-agent-open-input' before each message.
  (setopt pi-coding-agent-input-window-display 'always
          pi-coding-agent-input-window-height 0.25)

  ;; Yank the underlying Markdown rather than the rendered text, so code blocks
  ;; survive a copy out of the chat buffer.
  (setopt pi-coding-agent-copy-raw-markdown t)

  ;; Open on the right (40%) instead of taking over the current window; pi
  ;; still stacks its input pane below the chat inside that column.
  (defun my/pi-display-on-right (orig chat-buf input-buf &optional chat-only)
    (if (or (get-buffer-window chat-buf) (get-buffer-window input-buf))
        (funcall orig chat-buf input-buf chat-only)
      (let ((win (split-window (frame-root-window)
                               (- (round (* 0.4 (frame-width)))) 'right)))
        (select-window win)
        (funcall orig chat-buf input-buf chat-only))))
  (advice-add 'pi-coding-agent--display-buffers :around #'my/pi-display-on-right))

(provide 'init-pi)
;;; init-pi.el ends here
