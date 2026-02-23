;;; autoload/python.el -*- lexical-binding: t; -*-

;;
;; ── Breakpoint highlight ──────────────────────────────────────────────────────

;;;###autoload
(defun my/python-annotate-pdb ()
  "Highlight breakpoint lines in the current buffer."
  (interactive)
  (highlight-lines-matching-regexp "breakpoint()" 'hi-yellow)
  (highlight-lines-matching-regexp "import \\(pdb\\|ipdb\\|pudb\\)" 'hi-yellow)
  (highlight-lines-matching-regexp "\\(pdb\\|ipdb\\|pudb\\).set_trace()" 'hi-yellow))

;;
;; ── Run commands ──────────────────────────────────────────────────────────────

;;;###autoload
(defun my/python-copy-python-cmd ()
  "Copy `python3 <file>' command to kill ring."
  (interactive)
  (kill-new (concat "python3 " (file-relative-name (buffer-file-name) (doom-project-root))))
  (message "Copied python cmd"))

;;;###autoload
(defun my/python-copy-pytest-cmd ()
  "Copy `pytest file::function' command to kill ring."
  (interactive)
  (kill-new (concat "pytest "
                    (file-relative-name (buffer-file-name) (doom-project-root))
                    "::" (which-function)))
  (message "Copied pytest cmd"))

;;
;; ── Imports ───────────────────────────────────────────────────────────────────

(defvar my/python-temp-import nil "Temporary import for my/python-insert-temp-import.")

;;;###autoload
(defun my/python-yank-module-import ()
  "Copy `from module import function' for current position to kill ring."
  (interactive)
  (let ((import (string-join
                 (list "from"
                       (replace-regexp-in-string
                        "/" "."
                        (file-relative-name
                         (file-name-sans-extension (buffer-file-name))
                         (doom-project-root)))
                       "import"
                       (replace-regexp-in-string "\\..*" "" (or (which-function) "")))
                 " ")))
    (setq my/python-temp-import import)
    (message import)))

;;;###autoload
(defun my/python-insert-temp-import ()
  "Insert the last yanked module import (see `my/python-yank-module-import')."
  (interactive)
  (if my/python-temp-import
      (save-excursion
        (goto-char (point-min))
        (forward-line 1)
        (insert my/python-temp-import "\n"))
    (message "No import copied — run my/python-yank-module-import first")))
