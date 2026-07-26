;;; lisp/init-pdf.el -*- lexical-binding: t; -*-

;; PDF viewing with pdf-tools.
;;
;; The epdfinfo server (the C backend pdf-tools needs) is provided PREBUILT by
;; Nix — `emacsPackages.pdf-tools' in packages.nix — so it never has to compile
;; inside Emacs (which is fragile on nix-darwin). Here we just locate that
;; Nix-built binary and point pdf-tools at it; Doom's `:tools pdf' module does
;; the rest (activating pdf-view-mode for .pdf files).
(after! pdf-tools
  ;; Nix exposes the prebuilt server as `epdfinfo' on PATH (see packages.nix).
  (let ((server (executable-find "epdfinfo")))
    (if server
        (setq pdf-info-epdfinfo-program server)
      (message "init-pdf: epdfinfo not on PATH — run `make hm_update' then restart")))

  ;; Crisper rendering on HiDPI; fit page width on open.
  (setq pdf-view-use-scaling t
        pdf-view-display-size 'fit-width)

  ;; Continuous scrolling: all pages flow as one strip, so the mouse wheel /
  ;; trackpad and C-n/C-p scroll smoothly through the whole document instead of
  ;; flipping page-by-page.
  (add-hook 'pdf-view-mode-hook #'pdf-view-roll-minor-mode))
