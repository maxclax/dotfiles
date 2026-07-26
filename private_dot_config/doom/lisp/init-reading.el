;;; init-reading -*- mode: emacs-lisp; lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NEWSTICKER - RSS Feed Reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configure Newsticker immediately
(setq newsticker-url-list '(
  ("Emacs Rocks" "https://emacsrocks.com/atom.xml")
  ("Planet Emacslife" "https://planet.emacslife.com/atom.xml")
  ("Ruslan's Tech Blog" "https://codelearn.me/feed.xml")))

;; Custom function to close Newsticker buffers
(defun my/close-newsticker ()
  "Kill all tree-view related buffers."
  (ignore-errors
    (kill-buffer "*Newsticker List*"))
  (ignore-errors
    (kill-buffer "*Newsticker Item*"))
  (ignore-errors
    (kill-buffer "*Newsticker Tree*")))

(use-package! newsticker
  :defer t
  :config
  ;; Advice to automatically close buffers when quitting Newsticker
  (advice-add 'newsticker-treeview-quit :after 'my/close-newsticker))

(provide 'init-reading)
