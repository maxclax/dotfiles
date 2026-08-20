;;; init-auth.el -*- lexical-binding: t; -*-
;; Central credential lookup (auth-source) for ALL of Emacs — gptel, TRAMP,
;; forge, smtp, … 1Password answers first via the `op' CLI; secrets never
;; touch disk. Layout: the "Automation" vault, one item per host, field
;; named after the auth-source login — e.g. item "api.anthropic.com" with
;; field "apikey" -> op://Automation/api.anthropic.com/apikey.
;;
;; The vault itself is the source of truth: its item titles are listed once
;; per session, and only those hosts are asked of 1Password — new items are
;; picked up with M-x my/auth-1password-refresh (or next Emacs start), no
;; config changes. Everything else falls through to ~/.authinfo.

(use-package! auth-source-1password
  :config
  (setq auth-source-1password-vault "Automation")

  (defvar my/auth-1password--vault-hosts 'unset
    "Cached item titles of the vault, or `unset' before first listing.")

  (defun my/auth-1password-vault-hosts ()
    "Item titles in `auth-source-1password-vault', cached per session.
A failed listing (1Password locked, op missing) caches as empty so
lookups quietly fall back to ~/.authinfo; refresh to retry."
    (when (eq my/auth-1password--vault-hosts 'unset)
      (setq my/auth-1password--vault-hosts
            (or (ignore-errors
                  (let ((json (shell-command-to-string
                               (format "%s item list --vault %s --format json"
                                       auth-source-1password-executable
                                       (shell-quote-argument
                                        auth-source-1password-vault)))))
                    (mapcar (lambda (item) (alist-get 'title item))
                            (append (json-parse-string json
                                                       :object-type 'alist)
                                    nil))))
                nil)))
    my/auth-1password--vault-hosts)

  (defun my/auth-1password-refresh ()
    "Re-list the 1Password vault (run after adding a new item)."
    (interactive)
    (setq my/auth-1password--vault-hosts 'unset)
    (message "1Password vault hosts: %s"
             (string-join (my/auth-1password-vault-hosts) ", ")))

  ;; Ask 1Password only for hosts that have an item in the vault, and treat
  ;; an empty `op' result as a miss — returning nil lets auth-source
  ;; continue to ~/.authinfo.
  (defadvice! my/auth-1password-vault-hosts-only-a (fn &rest spec)
    :around #'auth-source-1password-search
    (let* ((host (plist-get spec :host))
           (hosts (if (listp host) host (list host))))
      (when (seq-some (lambda (h) (and (stringp h)
                                       (member h (my/auth-1password-vault-hosts))))
                      hosts)
        (let* ((result (apply fn spec))
               (secret (and result (plist-get (car result) :secret))))
          (when (and (stringp secret) (not (string-empty-p secret)))
            result)))))

  (auth-source-1password-enable))

;; sops-encrypted project files edit transparently: find-file shows the
;; decrypted buffer, C-x C-s re-encrypts on disk. The age key comes from the
;; login Keychain via SOPS_AGE_KEY_CMD (GUI Emacs doesn't inherit zsh session
;; vars, so set it here too), seeded from 1Password by chezmoi. Guarded so a
;; machine without the sops binary (pre-rebuild, no-op setups) is untouched.
(use-package! sops
  :when (executable-find "sops")
  :config
  (setenv "SOPS_AGE_KEY_CMD" "security find-generic-password -a sops -s sops-age-key -w")
  ;; default prefilter only matches trailing .env — also catch .env.prod etc.
  (setq sops-prefilter-regex
        "\\.\\(ya?ml\\|json\\|ini\\|txt\\)\\'\\|\\.env\\(\\.[A-Za-z0-9_-]+\\)?\\'")
  ;; sops--run's wait loop ends only when its sentinel fires — no timeout, no
  ;; liveness check — so a missed sentinel spins at 100% CPU and wedges the
  ;; whole daemon. Seen repeatedly on file open; bound it.
  (define-advice sops--run (:around (fn &rest args) my/sops-run-timeout)
    (with-timeout (15 (user-error "sops: timed out, skipping"))
      (apply fn args)))

  ;; doom-modeline hides minor-mode lighters — show our own encrypted badge
  (add-to-list 'mode-line-misc-info '(sops-mode " 🔒sops "))

  ;; Guardrail: a buffer opened BEFORE a file was encrypted (or any non-sops
  ;; buffer) would overwrite the ciphertext with plaintext on C-x C-s.
  ;; Detect encrypted-on-disk + no sops-mode and make the save opt-in.
  (defun my/sops-guard-plaintext-save-h ()
    (when (and buffer-file-name
               (not (bound-and-true-p sops-mode))
               (string-match-p sops-prefilter-regex buffer-file-name)
               (file-exists-p buffer-file-name)
               (with-temp-buffer
                 (ignore-errors
                   (insert-file-contents-literally buffer-file-name nil 0 4096))
                 (goto-char (point-min))
                 (re-search-forward
                  "ENC\[AES256_GCM\|^sops:\|"sops":\|^sops_" nil t)))
      (unless (yes-or-no-p
               "File on disk is sops-ENCRYPTED; saving would write PLAINTEXT over it. Really save? ")
        (user-error "Save aborted — use M-x revert-buffer to load the encrypted version"))))
  (add-hook 'before-save-hook #'my/sops-guard-plaintext-save-h)

  (global-sops-mode 1))

(provide 'init-auth)
