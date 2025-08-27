;;; +ui.el -*- lexical-binding: t; -*-

;; Set fallback theme
(setq fancy-splash-image (concat doom-private-dir "assets/GNUEmacs.png"))
(setq evil-emacs-state-cursor `(box ,(doom-color 'violet)))


(use-package! circadian
  :config
  (setq circadian-themes '(("5:00" . doom-one-light)
                           ("20:00" . doom-one)))

  ;; Determine the correct initial theme based on current time
  (let* ((hour-now (string-to-number (format-time-string "%H")))
         (minute-now (string-to-number (format-time-string "%M")))
         (time-now (+ (* hour-now 60) minute-now))
         (light-start (+ (* 5 60) 0))   ;; 5:00 in minutes
         (dark-start (+ (* 18 60) 30))) ;; 18:30 in minutes

    ;; Set the initial theme based on current time
    (setq doom-theme
          (if (and (>= time-now light-start)
                   (< time-now dark-start))
              'doom-one-light ;; During day (5:00-18:30)
            'doom-one)))      ;; During night

  ;; Setup circadian for automatic switching
  (circadian-setup))

;; Enable rainbow-mode for CSS, HTML, and other files with color codes
(add-hook 'css-mode-hook #'rainbow-mode)
(add-hook 'html-mode-hook #'rainbow-mode)
(add-hook 'web-mode-hook #'rainbow-mode)  ; If using web-mode
(add-hook 'prog-mode-hook #'rainbow-mode) ; For all programming modes (optional)

(beacon-mode 1)

(setq display-line-numbers-type t)
;; (setq display-line-numbers-type 'relative)

;; Set the default font
(set-face-attribute 'default nil
                    :font "Berkeley Mono Variable-18"
                    :weight 'normal
                    :width 'normal)

;; ====================== Fix for emacsclient ======================
;; Reapply GUI settings for new frames (emacsclient)

(add-hook 'server-after-make-frame-hook
          (lambda ()
            (when (and (boundp 'circadian--current-theme)
                       circadian--current-theme)
              (load-theme circadian--current-theme t))
            (set-face-attribute 'default nil
                               :font "Berkeley Mono Variable-18"
                               :weight 'normal
                               :width 'normal)))

;; ====================== End Fix for emacsclient ======================


(set-popup-rules! '(("^\\*helpful" :size 0.35 :modeline nil)
                    ("^\\*Ibuffer\\*$" :size 0.35 :modeline nil)
                    ("^\\*info.*" :size 80 :side right :modeline nil)
                    ("^\\*Man.*" :size 80 :side right :modeline nil)
                    ("^\\*keycast.*" :size 50 :side right :modeline nil)
                    ("^\\*Customize" :actions display-buffer :modeline nil)
                    ("^\\*edit-indirect" :size 0.6 :modeline nil)
                    ("^\\*YASnippet Tables\\*$" :size 0.35 :modeline nil)
                    ("^\\*grep\\*$" :size 0.35 :modeline nil)
                    ("^\\*pytest\\*" :size 0.35 :modeline nil)
                    ;; ("^\\*aider.*$" :size 0.35 :side right :modeline nil)
                    ("^\\*Chat" :size 0.35 :side right :modeline nil)
                    ;; ("^\\*gptel.*" :size 0.35 :side right :modeline nil)
                    ("\\*.*server log\\*$" :side top :size 0.20 :select nil :modeline nil)
                    ((lambda (buf _) (with-current-buffer buf (eq major-mode 'forge-topic-mode))) :size 0.35 :modeline nil)
                    ))

(setq doom-modeline-height 30     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(after! ibuffer
  (setq-hook! 'ibuffer-hook ibuffer-formats
              '((mark modified read-only locked " "
                 (name 50 18 :left :elide)
                 " "
                 (size 9 -1 :right)
                 " "
                 (mode 16 16 :left :elide)
                 " " filename-and-process)
                (mark " "
                      (name 16 -1)
                      " " filename))))

(use-package! all-the-icons-ibuffer
  :after ibuffer
  :init (all-the-icons-ibuffer-mode 1)
  )

(defface breakpoint-enabled '((t)) "Breakpoint face.")

;; Faces need to postpone renderring
;; custom-set-faces! doesn't work properly when you switch doom themes
(custom-theme-set-faces! '(doom-acario-light doom-one-light doom-city-lights)
  `(hl-line :background ,(doom-color 'bg-alt)) ; sometimes ranger doesn't show hl-line color
  `(doom-modeline-debug-visual :background ,(doom-blend 'red 'bg 0.3))
  `(mode-line :background ,(doom-blend 'blue 'bg 0.3))
  `(mode-line-inactive :background ,(doom-color 'bg-alt))
  `(vertical-border :foreground ,(doom-color 'bg-alt))
  `(vertico-posframe-border :background ,(doom-blend 'blue 'bg 0.35))
  '(font-lock-doc-face :italic t)
  '(font-lock-comment-face :italic t)
  '(font-lock-builtin-face :italic t)
  '(font-lock-type-face :italic t)
  '(tide-hl-identifier-face :inherit 'lsp-face-highlight-read)
  `(breakpoint-enabled :background ,(doom-blend 'red 'bg 0.15))
  `(lsp-ui-peek-highlight :foreground ,(doom-color 'blue))
  `(ivy-posframe-border :background ,(doom-color 'blue))
  `(magit-diff-file-heading :background ,(doom-blend 'blue 'bg 0.2))
  `(magit-diff-file-heading-highlight :background ,(doom-blend 'blue 'bg 0.5))
  '(markdown-header-face-1 :inherit 'org-level-1)
  '(markdown-header-face-2 :inherit 'org-level-2)
  '(markdown-header-face-3 :inherit 'org-level-3)
  `(smerge-upper :background ,(doom-blend 'red 'bg 0.2))
  `(smerge-lower :background ,(doom-blend 'green 'bg 0.2))
  `(web-mode-jsx-depth-1-face :background ,(doom-blend 'teal 'fg 0.1))
  `(web-mode-jsx-depth-2-face :background ,(doom-blend 'teal 'fg 0.2))
  `(web-mode-jsx-depth-3-face :background ,(doom-blend 'teal 'fg 0.3))
  `(web-mode-jsx-depth-4-face :background ,(doom-blend 'teal 'fg 0.4))
  `(web-mode-jsx-depth-5-face :background ,(doom-blend 'teal 'fg 0.5))
  `(flyspell-incorrect :underline ,(doom-color 'red))
  `(flyspell-duplicate :underline ,(doom-color 'orange))
  `(flymake-warning :underline (:style wave :color ,(doom-color 'yellow)) :bold t)
  `(flycheck-warning :underline nil :bold t)
  `(flycheck-error :underline (:style wave :color ,(doom-color 'red)))
  `(flycheck-info :underline (:style wave :color ,(doom-color 'green)))
  `(ein:cell-input-area :background ,(doom-blend 'red 'fg 0.15))
  `(ein:cell-input-prompt :background ,(doom-color 'red) :foreground ,(doom-color 'fg) :bold t)
  `(font-lock-comment-face :foreground ,(doom-color 'blue))
  `(font-lock-doc-face :foreground ,(doom-color 'blue)))

(custom-theme-set-faces! 'doom-city-lights
  `(mode-line :background ,(doom-blend 'dark-blue 'bg 0.2))
  `(hl-todo :foreground ,(doom-lighten 'fg 0.7))
  `(region :background ,(doom-color 'base5))
  ;; ediff
  `(ediff-current-diff-A :foreground ,(doom-color 'red)   :background ,(doom-blend 'red 'fg 0.2))
  `(ediff-current-diff-B :foreground ,(doom-color 'green) :background ,(doom-blend 'green 'fg 0.2))
  `(ediff-current-diff-C :foreground ,(doom-color 'blue)  :background ,(doom-blend 'blue 'fg 0.2))
  `(ediff-current-diff-Ancestor :foreground ,(doom-color 'teal)  :background ,(doom-blend 'teal 'fg 0.2))
  )

(custom-theme-set-faces! 'doom-acario-light
  ;; ediff
  `(ediff-current-diff-A :foreground ,(doom-color 'red)   :background ,(doom-blend 'red 'bg 0.2))
  `(ediff-current-diff-B :foreground ,(doom-color 'green) :background ,(doom-blend 'green 'bg 0.2))
  `(ediff-current-diff-C :foreground ,(doom-color 'blue)  :background ,(doom-blend 'blue 'bg 0.2))
  `(ediff-current-diff-Ancestor :foreground ,(doom-color 'teal)  :background ,(doom-blend 'teal 'bg 0.2))
  `(vertico-posframe-border :background ,(doom-blend 'teal 'bg 0.35))
  `(mode-line :background ,(doom-blend 'teal 'bg 0.2))
  `(lazy-highlight :foreground ,(doom-color 'base0) :background ,(doom-color 'teal))
  `(internal-border :background nil)
  `(lsp-face-highlight-textual :foreground ,(doom-color 'fg) :background ,(doom-color 'bg-alt))
  `(wgrep-face :background ,(doom-blend 'grey 'bg 0.2))
  `(markdown-code-face :background ,(doom-color 'bg-alt) :extend t)
  )

;; Light theme for winter
(custom-theme-set-faces! 'doom-winter-is-coming-light
  `(markdown-code-face :background ,(doom-color 'bg-alt) :extend t)
  )



(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))

(after! centered-window
  (setq cwm-centered-window-width 160))
