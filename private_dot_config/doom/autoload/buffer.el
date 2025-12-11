;;; autoload/buffer.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/kill-buffer-filename  ()
  "Copy and show the file name of the current buffer."
  (interactive)
  (if-let (file-name (file-name-nondirectory (buffer-file-name)))
      (progn
        (kill-new file-name)
        (message "%s" file-name))
    (message "WARNING: Current buffer is not attached to a file!")))

;;;###autoload
(defun my/kill-buffer-filepath ()
  "Copy and show the full file path of the current buffer."
  (interactive)
  (if-let (file-path (buffer-file-name))
      (progn
        (kill-new file-path)
        (message "%s" file-path))
    (message "WARNING: Current buffer is not attached to a file!")))


;;;###autoload
(defun my/delete-file-and-kill-buffer ()
  "Delete the current file and kill the buffer, or just kill buffer if no file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        ;; Buffer has a file - ask to delete it
        (when (y-or-n-p (format "Really delete file %s? " (file-name-nondirectory filename)))
          (delete-file filename t)
          (kill-buffer (current-buffer))
          (message "File %s deleted" (file-name-nondirectory filename)))
      ;; No file - just kill the buffer
      (kill-buffer (current-buffer))
      (message "Buffer killed"))))


;;;###autoload
(defun wait/indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))


;;;###autoload
(defun wait/indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (indent-buffer)
        (message "Indented buffer.")))))


;;;###autoload
(defun wait/untabify-buffer ()
  (interactive)
  (save-excursion
    (untabify (point-min) (point-max)) nil))


;;;###autoload
(defun wait/check-large-buffer ()
  "Check if the buffer is large."
  (when (> (buffer-size) 1048576)       ; 1MB
    t))
