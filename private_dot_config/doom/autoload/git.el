;;; autoload/git.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/fix-git-lock ()
  "Remove git index.lock files in current git repository and submodules."
  (interactive)
  (let ((git-root (or (magit-toplevel)
                      (projectile-project-root)
                      default-directory))
        (removed-count 0))

    ;; Find and remove all index.lock files
    (let ((lock-files (split-string
                      (shell-command-to-string
                       (format "find %s -name 'index.lock' -type f 2>/dev/null"
                               (shell-quote-argument git-root)))
                      "\n" t)))

      (dolist (lock-file lock-files)
        (when (file-exists-p lock-file)
          (delete-file lock-file)
          (setq removed-count (1+ removed-count))
          (message "Removed: %s" lock-file)))

      (if (> removed-count 0)
          (progn
            (message "✅ Removed %d git lock file(s)" removed-count)
            ;; Refresh magit if it's running
            (when (and (featurep 'magit) (magit-toplevel))
              (magit-refresh)))
        (message "No git lock files found")))))