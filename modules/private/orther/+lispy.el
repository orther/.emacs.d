;;; private/orther/+lispy.el -*- lexical-binding: t; -*-

;; (def-package! aggressive-indent
;;   :commands (aggressive-indent-mode)
;;   :init (add-hook! emacs-lisp-mode #'aggressive-indent-mode))

(def-package! lispy
  :commands lispy-mode
  :init (add-hook! emacs-lisp-mode '(turn-off-smartparens-mode lispy-mode)))

(def-package! lispyville
  :after lispy
  :commands lispyville-mode
  :init
  (add-hook! lispy-mode #'lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(operators
     c-w
     s-operators
     slurp/barf-lispy
     additional-movement
     additional
     escape
     ;; (escape insert)
     ;; (additional-movement normal visual motion)
     )))
