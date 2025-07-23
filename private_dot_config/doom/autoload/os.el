;;; autoload/os.el -*- lexical-binding: t; -*-

;; Copied from https://github.com/emacsorphanage/osx-trash/blob/master/osx-trash.el
(defconst os--trash-pkg-file
  (expand-file-name (if load-in-progress load-file-name (buffer-file-name)))
  "The absolute path to this file.")

(defconst os--trash-pkg-dir
  (file-name-directory os--trash-pkg-file)
  "The absolute path to the directory of this package.")

(defun os--trash-move-file-to-trash (file-name)
  "Move FILE-NAME to trash.

Try to call the `trash' utility first"
  (let ((file-name (expand-file-name file-name)))
    (with-temp-buffer
      (let ((retcode (call-process "trash" nil t nil file-name)))
        (unless (equal retcode 0)
          (error "Failed to trash %S: %S" file-name (buffer-string)))))))

;;;###autoload
(defun os--trash-setup ()
  "Provide trash support for OS X.

Provide `system-move-file-to-trash' as an alias for
`os--trash-move-file-to-trash'.

Note that you still need to set `delete-by-moving-to-trash' to a
non-nil value to enable trashing for file operations."
  (when (not (fboundp 'system-move-file-to-trash))
    (defalias 'system-move-file-to-trash
      'os--trash-move-file-to-trash)))

;; ------------------------------------------------------------------------
