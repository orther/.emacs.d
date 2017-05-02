;;; lang/scala/config.el

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :init
  (add-hook 'scala-mode-hook 'scala/configure-ensime)
  (add-hook 'ensime-mode-hook 'scala/enable-eldoc)
  :config
  (setq scala-indent:align-parameters t)
  (map! :map scala-mode-map
    :niv "TAB" 'ensime-company-complete-or-indent
    (:leader
      (:desc "mode"
       :prefix "m"
       :desc "Start ensime"       :n "s" 'ensime))))

(def-package! sbt-mode :after scala-mode)

(def-package! ensime
  :commands (ensime ensime-mode ensime-scala-mode-hook ensime-company-enable)
  :config
  ;; disable ensime startup notification
  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil))
