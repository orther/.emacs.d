;;; feature/search/config.el

(def-package! iedit
  :commands (iedit-mode)
  :config
  (map! :map iedit-mode-occurrence-keymap
        :n [escape] 'iedit-quit))
