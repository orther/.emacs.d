;;; lang/scala/config.el -*- lexical-binding: t; -*-

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (when (not (boundp 'imenu-auto-rescan))
      (setq imenu-auto-rescan nil))
  :config
  (add-hook 'scala-mode-hook #'ensime-mode)
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
  :commands (ensime ensime-mode ensime-scala-mode-hook ensime-company-enable)
  :config
  ;; disable ensime startup notification
  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints t)
  (add-hook 'ensime-mode-hook #'eldoc-mode))
