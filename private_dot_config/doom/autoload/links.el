;;; autoload/links.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/insert-shell-link ()
  "Insert a shell:open link for any app URL (alfred, drafts, etc)."
  (interactive)
  (let* ((url (read-string "App URL: "))
         (desc (read-string "Description (optional): " url)))
    (insert (format "[[shell:open \"%s\"][%s]]" url desc))))
