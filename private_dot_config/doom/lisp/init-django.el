;;; init-django.el -*- lexical-binding: t; -*-

(defun my/django-open-template-at-point ()
  "Open the Django template file referenced by the string at point.
Resolves relative to the project's templates/ directory."
  (interactive)
  (let* ((name (thing-at-point 'filename t))
         (root (projectile-project-root))
         (path (expand-file-name name (concat (string-remove-suffix "/" root) "/templates/"))))
    (if (file-exists-p path)
        (find-file path)
      (user-error "Template not found: %s" path))))

(defun my/django-open-static-at-point ()
  "Open the Django static file referenced by the string at point.
Resolves relative to the project's static/ directory."
  (interactive)
  (let* ((name (thing-at-point 'filename t))
         (root (projectile-project-root))
         (path (expand-file-name name (concat (string-remove-suffix "/" root) "/static/"))))
    (if (file-exists-p path)
        (find-file path)
      (user-error "Static file not found: %s" path))))

(defun my/django-open-url-at-point ()
  "Jump to the Django URL pattern definition referenced at point.
Handles namespaced ('app:name') and plain ('name') URL references.
With namespace, finds the urls.py with matching app_name first."
  (interactive)
  (let* ((raw (save-excursion
                (when (re-search-backward "[\"']" (line-beginning-position) t)
                  (let ((start (1+ (point))))
                    (goto-char start) ; move past opening quote so re-search-forward finds closing one
                    (when (re-search-forward "[\"']" (line-end-position) t)
                      (buffer-substring-no-properties start (1- (point))))))))
         (parts    (when raw (split-string raw ":")))
         (namespace (when (> (length parts) 1) (car parts)))
         (url-name  (car (last parts)))
         (root      (projectile-project-root)))
    (if (not url-name)
        (user-error "No URL name at point")
      (let ((urls-file
             (if namespace
                 ;; Find the urls.py that declares this namespace via app_name
                 (let* ((cmd (format "rg -l -g 'urls.py' %s %s 2>/dev/null"
                                     (shell-quote-argument (concat "app_name.*" namespace))
                                     (shell-quote-argument root)))
                        (result (string-trim (shell-command-to-string cmd))))
                   (car (split-string result "\n" t)))
               nil)))
        (if (and namespace (not urls-file))
            (user-error "No urls.py found for namespace '%s'" namespace)
          (let* ((cmd (if urls-file
                          ;; Search only in the namespaced urls.py — output: LINE:content
                          (format "rg -n %s %s 2>/dev/null"
                                  (shell-quote-argument (concat "name=.*" url-name))
                                  (shell-quote-argument urls-file))
                        ;; No namespace — search all urls.py — output: FILE:LINE:content
                        (format "rg -n -g 'urls.py' %s %s 2>/dev/null"
                                (shell-quote-argument (concat "name=.*" url-name))
                                (shell-quote-argument root))))
                 (results (string-trim (shell-command-to-string cmd)))
                 (first   (car (split-string results "\n" t))))
            (if (not first)
                (user-error "URL pattern not found: %s" url-name)
              (if urls-file
                  ;; Single file search: output is LINE:content
                  (progn
                    (string-match "^\\([0-9]+\\):" first)
                    (let ((line (string-to-number (match-string 1 first))))
                      (find-file urls-file)
                      (goto-char (point-min))
                      (forward-line (1- line))))
                ;; Multi-file search: output is FILE:LINE:content
                (string-match "^\\(.+\\):\\([0-9]+\\):" first)
                (let ((file (match-string 1 first))
                      (line (string-to-number (match-string 2 first))))
                  (find-file file)
                  (goto-char (point-min))
                  (forward-line (1- line)))))))))))



(defun my/django-open-module-at-point ()
  "Open the Python module file at point."
  (interactive)
  (let* ((module (save-excursion
                   (skip-chars-backward "a-zA-Z0-9_.")
                   (let ((start (point)))
                     (skip-chars-forward "a-zA-Z0-9_.")
                     (buffer-substring-no-properties start (point)))))
         (root (string-remove-suffix "/" (projectile-project-root)))
         (path (concat (replace-regexp-in-string "\\." "/" module) ".py"))
         (full-path (expand-file-name path root)))
    (if (file-exists-p full-path)
        (find-file full-path)
      (user-error "Module not found: %s" full-path))))

(defun my/django-open-at-point ()
  "Smart open: detect what's at point and navigate to the right Django resource."
  (interactive)
  (let ((thing (or (thing-at-point 'filename t)
                   (save-excursion
                     (skip-chars-backward "a-zA-Z0-9_.")
                     (let ((start (point)))
                       (skip-chars-forward "a-zA-Z0-9_.")
                       (buffer-substring-no-properties start (point)))))))
    (cond
     ((string-match-p "\\.html?\\'" thing)        (my/django-open-template-at-point))
     ((string-match-p "\\.(png\\|jpg\\|jpeg\\|gif\\|svg\\|css\\|js\\|ico\\|woff2?\\|ttf)\\'" thing)
                                                   (my/django-open-static-at-point))
     ((and (string-match-p "\\." thing)
           (not (string-match-p "/" thing)))       (my/django-open-module-at-point))
     (t                                            (my/django-open-url-at-point)))))

(after! dape
  (add-to-list 'dape-configs
    '(django-attach
      modes (python-mode python-ts-mode)
      host "localhost"
      port 5678
      :request "attach"
      :type "python"
      :pathMappings [(:localRoot dape-cwd :remoteRoot "/usr/src/app")])))

(defun my/django-project-p ()
  "Return non-nil if the current buffer is inside a Django project.
Detected by the presence of manage.py at the project root."
  (when-let ((root (projectile-project-root)))
    (file-exists-p (expand-file-name "manage.py" root))))

(defun my/django-setup ()
  "Apply personal Django settings for the current buffer.
Activated automatically in any project containing manage.py."
  (when (my/django-project-p)
    (let ((root (string-remove-suffix "/" (projectile-project-root))))
      ;; Coding style
      (setq-local tab-width 4
                  fill-column 88
                  indent-tabs-mode nil
                  require-final-newline t
                  show-trailing-whitespace t)
      (display-fill-column-indicator-mode 1)
      ;; Python-specific
      (when (derived-mode-p 'python-mode 'python-ts-mode)
        (setq-local python-shell-interpreter
                    (expand-file-name ".venv/bin/python" root))
        (setq-local python-shell-interpreter-args "-i")
        (setq-local python-black-command
                    (expand-file-name ".venv/bin/black" root))
        (setq-local lsp-diagnostics-provider   :flymake
                    lsp-pyright-venv-path       root
                    lsp-pyright-venv-directory  ".venv"
                    lsp-pyright-python-executable-cmd
                    (expand-file-name ".venv/bin/python" root)
                    lsp-pyright-multi-root      nil
                    lsp-pyright-diagnostic-mode "workspace"))
      ;; Navigation keys
      (local-set-key (kbd "C-c c o o") #'my/django-open-at-point)
      (local-set-key (kbd "C-c c o t") #'my/django-open-template-at-point)
      (local-set-key (kbd "C-c c o s") #'my/django-open-static-at-point)
      (local-set-key (kbd "C-c c o u") #'my/django-open-url-at-point)
      (local-set-key (kbd "C-c c o m") #'my/django-open-module-at-point))))

(dolist (hook '(python-mode-hook
                python-ts-mode-hook
                web-mode-hook
                html-mode-hook
                html-ts-mode-hook))
  (add-hook hook #'my/django-setup))
