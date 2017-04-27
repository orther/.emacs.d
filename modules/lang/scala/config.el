;;; lang/scala/config.el

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (add-hook 'scala-mode-hook #'eldoc-mode)
  :config
  ;; disable ensime startup notification
  (setq ensime-startup-snapshot-notification nil)
  (setq ensime-startup-notification nil)
  (map! :map scala-mode-map
    :niv "TAB" 'ensime-company-complete-or-indent
    (:leader
      (:desc "mode"
       :prefix "m"
       :desc "Start ensime"       :n "s" 'ensime)))
  (add-hook 'scala-mode-hook 'scala/configure-ensime)
  (add-hook 'ensime-mode-hook 'scala/enable-eldoc))

(def-package! sbt-mode :after scala-mode)

(def-package! ensime
  :commands (ensime ensime-mode ensime-scala-mode-hook ensime-company-enable))
