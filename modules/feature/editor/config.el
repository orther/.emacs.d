;;; feature/editor/config.el

(def-package! clean-aindent-mode :demand t
  :config
  (clean-aindent-mode t)
  (define-key global-map (kbd "RET") 'newline-and-indent))

(def-package! yafolding :demand t
  :init
  (defvar yafolding-mode-map (make-sparse-keymap))
  :config
  (add-hook 'prog-mode-hook
            (lambda () (yafolding-mode))))
