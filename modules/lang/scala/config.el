;;; lang/scala/config.el -*- lexical-binding: t; -*-

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config
  (setq scala-indent:align-parameters t)
  (map! :mode scala-mode
        :niv "TAB" 'ensime-company-complete-or-indent
        :localleader
        :desc "Start ensime"             :n "s" #'ensime
        :desc "Shutdown ensime"          :n "x" #'ensime-shutdown
        :desc "show errors at point"     :n "e" #'ensime-print-errors-at-point
        :desc "Show errors and warnings" :n "E" #'ensime-show-all-errors-and-warnings))

(def-package! sbt-mode :after scala-mode)

(def-package! ensime
  :after scala-mode
  :commands (ensime ensime-mode ensime-scala-mode-hook)
  :config
  (set! :company-backend 'scala-mode '(ensime-company company-yasnippet))

  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints t
        ;; let DOOM handle company setup
        ensime-completion-style nil)

  (add-hook 'scala-mode-hook #'ensime-mode)
  (add-hook 'ensime-mode-hook #'eldoc-mode)

  ;; Fix void-variable imenu-auto-rescan error caused by `ensime--setup-imenu'
  ;; trying to make imenu variables buffer local before imenu is loaded.
  (require 'imenu))
