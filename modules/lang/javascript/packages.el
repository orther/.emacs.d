;; -*- no-byte-compile: t; -*-
;;; lang/javascript/packages.el

;; requires node npm tern js-beautify eslint eslint-plugin-react

(package! coffee-mode)
(package! eslintd-fix)
(package! flow-minor-mode)
(package! flycheck-flow)
(package! js2-mode)
(package! js2-refactor)
(package! nodejs-repl)
(package! rjsx-mode)
(package! skewer-mode)
(package! tern)
(package! web-beautify)

(when (featurep! :completion company)
  (package! company-flow)
  (package! company-tern))

(when (featurep! :feature jump)
  (package! xref-js2))
