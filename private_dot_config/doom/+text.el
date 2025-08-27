;;; +text.el -*- lexical-binding: t; -*-

(use-package! pomm
  :defer t
  :commands (pomm pomm-third-time)
  :config
  (setq pomm-work-period 40
        pomm-long-break-period 25
        pomm-short-break-period 5
        alert-default-style (if IS-MAC 'osx-notifier 'libnotify)
        pomm-audio-enabled t)
  (pomm-mode-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package! org-roam
;;   :custom
;;   (org-roam-directory
;;    (expand-file-name "~/Library/Mobile Documents/iCloud~com~logseq~logseq/Documents/Org"))
;;   (org-roam-dailies-directory "journals/")
;;   (org-roam-capture-templates
;;    '(("d" "default" plain
;;       "%?" :target
;;       (file+head "pages/${slug}.org" "#+title: ${title}\n")
;;       :unnarrowed t))))

(setq org-directory (expand-file-name "~/.org")
      org-roam-directory (expand-file-name "roam" org-directory)
      org-agenda-files (list org-directory)
      org-ellipsis " ▼ "
      org-hide-emphasis-markers t
      org-babel-python-command "python3"
      ;; The standard unicode characters are usually misaligned depending on the
      ;; font. This bugs me. Markdown #-marks for headlines are more elegant.
      org-bullets-bullet-list '("#"))

(after! org-roam
  (make-directory (concat org-directory "/roam") 'parents))

;; (use-package! websocket
;;   :after org-roam)

;; (use-package! org-roam-ui
;;   :after org-roam ;; or :after org
;;   ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;   ;;         a hookable mode anymore, you're advised to pick something yourself
;;   ;;         if you don't care about startup time, use
;;   ;; :hook (after-init . org-roam-ui-mode)
;;   :config
;;   (setq org-roam-ui-sync-theme t
;;         org-roam-ui-follow t
;;         org-roam-ui-update-on-save t
;;         org-roam-ui-open-on-start t))

(after! (:and org-agenda all-the-icons)
  ;; https://old.reddit.com/r/emacs/comments/hnf3cw/my_orgmode_agenda_much_better_now_with_category/
  (setq org-agenda-category-icon-alist
        `(("work" ,(list (all-the-icons-material "work")) nil nil :ascent center)
          ("chore" ,(list (all-the-icons-material "home")) nil nil :ascent center)
          ("events" ,(list (all-the-icons-material "event")) nil nil :ascent center)
          ("todo" ,(list (all-the-icons-material "check_box")) nil nil :ascent center)
          ("solution" ,(list (all-the-icons-material "done")) nil nil :ascent center)
          ("birthday" ,(list (all-the-icons-material "cake")) nil nil :ascent center)
          ("anniversary" ,(list (all-the-icons-material "favorite")) nil nil :ascent center))))

(after! org
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ("p" "Templates for projects")
          ("pt" "Project todo" entry       ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry      ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i" :prepend t :kill-buffer t)))

  (setq org-log-into-drawer "LOGBOOK")

  ;; Schedule/deadline popup with default time
  (defvar org-default-time "10:30"
    "The default time for deadlines.")

  (defun advise-org-default-time (func arg &optional time)
    (let ((old-time (symbol-function #'org-read-date)))
      (cl-letf (((symbol-function #'org-read-date)
                 #'(lambda (&optional a b c d default-time f g)
                     (let ((default-time (or default-time
                                             org-default-time)))
                       (apply old-time a b c d f default-time g)
                       ))))
        (apply func arg time))))

  (advice-add #'org-deadline :around #'advise-org-default-time)
  (advice-add #'org-schedule :around #'advise-org-default-time))

(after! ox-pandoc
  (setq org-pandoc-options-for-revealjs '((variable . "highlight-theme=github")
                                          (variable . "theme=white"))))
(use-package! org-appear
  :defer t
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-delay 0.3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MARKDOWN
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq markdown-fontify-code-blocks-natively t)

(remove-hook 'text-mode-hook #'auto-fill-mode)

(use-package! edit-indirect :defer t)

(add-hook! 'markdown-mode-hook (setq-local format-all-formatters '(("Markdown" prettier))))

(after! markdown-mode
  (advice-add #'markdown-follow-thing-at-point :around #'doom-set-jump-a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHERS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package! tldr
  :defer t
  :config
  (setq tldr-directory-path (concat doom-etc-dir "tldr/"))
  (set-popup-rule! "^\\*tldr\\*" :side 'right :select t :quit t)
  )

(use-package! link-hint :defer t)

(use-package! symbol-overlay :defer t)

(after! so-long
  (setq so-long-target-modes (delete 'text-mode so-long-target-modes)))

(use-package! adoc-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode)))
