;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "os")
(load! "git")
(load! "misc")
(load! "text")
(load! "prog")
(load! "ui")
(load! "keys")

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Berkeley Mono Variable"
                          :size 20
                          :weight 'normal
                          :width 'normal))

;; Set the big font variant
(setq doom-big-font (font-spec :family "Berkeley Mono Variable"
                               :size 36
                               :weight 'bold))

;; Set the bold font variant
(setq doom-font-bold (font-spec :family "Berkeley Mono Variable"
                               :size 20
                               :weight 'bold
                               :width 'normal))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Load system profile for different machines and work config
(dolist (config '("~/.config/doom/local.el"))
  (let ((config-file (file-truename config)))
    (when (file-exists-p config-file)
      (load-file config-file))))
