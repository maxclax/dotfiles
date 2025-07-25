;;; +eglot.el -*- lexical-binding: t; -*-

(use-package! breadcrumb
  :defer t
  :when (modulep! :tools lsp +eglot)
  :hook
  (prog-mode . breadcrumb-local-mode)
  (text-mode . breadcrumb-local-mode)
  )

(after! eglot
  ;; JS & TS
  ;; https://www.reddit.com/r/emacs/comments/11bqzvk/emacs29_and_eglot_inlay_hints/
  ;; https://github.com/microsoft/TypeScript/blob/main/src/server/protocol.ts#L3410-L3539
  (set-eglot-client! '(typescript-mode js-mode js-ts-mode tsx-ts-mode typescript-ts-mode)
                     '("typescript-language-server" "--stdio" :initializationOptions
                       (:preferences (:importModuleSpecifierPreference "relative"
                                      :allowRenameOfImportPath t
                                      ))))
  )
