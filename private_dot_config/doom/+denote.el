;;; +denote.el --- Denote configuration for PARA system -*- lexical-binding: t; -*-

;;; Commentary:
;; Denote configuration integrated with PARA method
;; - Projects: Active projects with deadlines
;; - Areas: Ongoing responsibilities
;; - Resources: Topics of ongoing interest
;; - Archive: Inactive items

;;; Code:

(use-package! denote
  :config
  ;; PARA directory structure
  (setq denote-directory (expand-file-name "~/PARA"))

  ;; PARA-based keywords
  (setq denote-known-keywords
        '("inbox" "project" "area" "resource" "archive"
          "meeting" "daily" "weekly" "idea" "reference"
          "goal" "habit" "review" "learning" "code"))

  ;; Sort keywords for consistency
  (setq denote-sort-keywords t)

  ;; File type defaults
  (setq denote-file-type 'org)

  ;; Date format (compatible with obsidian)
  (setq denote-date-format "%Y%m%d")

  ;; Rename buffer to show denote title
  (denote-rename-buffer-mode 1)

  ;; PARA-specific subdirectories
  (setq denote-directory-files-matching-regexp
        (concat "\\(?1:[0-9]\\{8\\}\\)\\(?2:T[0-9]\\{6\\}\\)?"
                "\\(?3:--[[:alnum:][:nonascii:]-]*\\)?"
                "\\(?4:__[[:alnum:][:nonascii:]-]*\\)?"
                "\\.\\(?5:org\\|md\\|txt\\)$"))

  ;; Use default front matter (don't override)
  ;; denote provides the right number of arguments automatically

  ;; Template for different PARA categories
  (defun my/denote-para-template (category)
    "Insert PARA-specific template based on CATEGORY."
    (pcase category
      ("project" "* Project Overview\n** Goals\n** Tasks\n** Resources\n** Timeline\n")
      ("area" "* Area Overview\n** Responsibilities\n** Standards\n** Resources\n")
      ("resource" "* Resource Summary\n** Key Points\n** Related Topics\n** References\n")
      ("meeting" "* Meeting Notes\n** Attendees\n** Agenda\n** Action Items\n** Decisions\n")
      (_ "* Notes\n")))

  ;; Custom function to create PARA notes
  (defun my/denote-create-para-note (category title)
    "Create a denote note with PARA CATEGORY and TITLE."
    (interactive
     (list (completing-read "PARA Category: "
                           '("inbox" "project" "area" "resource" "archive"))
           (read-string "Note title: ")))
    (let ((keywords (list category))
          (denote-directory (expand-file-name "~/PARA")))
      (denote title keywords)
      (when (derived-mode-p 'org-mode)
        (goto-char (point-max))
        (insert "\n")
        (insert (my/denote-para-template category))))))

(use-package! denote-explore
  :after denote
  :config
  ;; Network visualization for note connections
  (setq denote-explore-network-filename "denote-network.html")
  (setq denote-explore-network-directory denote-directory))

(use-package! consult-notes
  :config
  ;; Integration with consult for fast note access
  (setq consult-notes-file-dir-sources
        `(("PARA Inbox" ?i ,(expand-file-name "0-Inbox" denote-directory))
          ("PARA Projects" ?p ,(expand-file-name "1-Projects" denote-directory))
          ("PARA Areas" ?a ,(expand-file-name "2-Areas" denote-directory))
          ("PARA Resources" ?r ,(expand-file-name "3-Resources" denote-directory))
          ("PARA Archive" ?x ,(expand-file-name "4-Archive" denote-directory))))

  ;; Enable live preview
  (consult-notes-org-headings-mode 1))

;; Key bindings for PARA workflow
(map! :leader
      (:prefix ("n d" . "denote")
       :desc "Create PARA note" "p" #'my/denote-create-para-note
       :desc "Find note" "f" #'denote-open-or-create
       :desc "Create note" "n" #'denote
       :desc "Link to note" "l" #'denote-link
       :desc "Backlinks" "b" #'denote-find-backlink
       :desc "Rename file" "r" #'denote-rename-file
       :desc "Keywords" "k" #'denote-keywords-add
       :desc "Remove keywords" "K" #'denote-keywords-remove
       :desc "Link to heading" "h" #'denote-org-extras-link-to-heading
       :desc "Search notes" "s" #'consult-notes
       :desc "Search in notes" "S" #'consult-notes-search-in-all-notes
       :desc "Explore network" "e" #'denote-explore-network))

;; PARA-specific functions
(defun my/denote-move-to-para-category (category)
  "Move current denote file to PARA CATEGORY directory."
  (interactive
   (list (completing-read "Move to PARA category: "
                         '("0-Inbox" "1-Projects" "2-Areas" "3-Resources" "4-Archive"))))
  (when (denote-file-is-note-p (buffer-file-name))
    (let* ((old-file (buffer-file-name))
           (new-dir (expand-file-name category denote-directory))
           (new-file (expand-file-name (file-name-nondirectory old-file) new-dir)))
      (unless (file-exists-p new-dir)
        (make-directory new-dir t))
      (rename-file old-file new-file)
      (set-visited-file-name new-file)
      (message "Moved to %s" category))))

(defun my/denote-weekly-review ()
  "Create a weekly review note in Areas."
  (interactive)
  (let ((title (format "Weekly Review %s" (format-time-string "%Y-W%U")))
        (keywords '("area" "review" "weekly")))
    (denote title keywords)
    (when (derived-mode-p 'org-mode)
      (goto-char (point-max))
      (insert "* Previous Week Wins\n\n* Areas Review\n\n* Projects Progress\n\n* Next Week Focus\n\n* Improvements\n\n"))))

(map! :leader
      (:prefix ("n d" . "denote")
       :desc "Move to PARA category" "m" #'my/denote-move-to-para-category
       :desc "Weekly review" "w" #'my/denote-weekly-review))

(provide '+denote)
;;; +denote.el ends here