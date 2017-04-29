;;; feature/editor/config.el

(def-package! clean-aindent-mode :demand t
  :config
  (clean-aindent-mode t)
  (define-key global-map (kbd "RET") 'newline-and-indent))
