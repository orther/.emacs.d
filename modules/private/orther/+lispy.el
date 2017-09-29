;;; private/orther/+lispy.el -*- lexical-binding: t; -*-

(def-package! aggressive-indent
  :commands (aggressive-indent-mode)
  :init (add-hook! emacs-lisp-mode #'aggressive-indent-mode))

(def-package! lispy
  :commands (lispy-mode)
  :init (add-hook! emacs-lisp-mode #'lispy-mode))

;; (def-package! lispyville
;;   :commands (lispyville-mode)
;;   :init (add-hook! lispy-mode #'lispyville-mode))
