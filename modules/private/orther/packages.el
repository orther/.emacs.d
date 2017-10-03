;; -*- no-byte-compile: t; -*-
;;; private/orther/packages.el

;; disable smartparens for lispy and friends
(def-package-hook! smartparens :disable)

;; editing & auto-formatting
(package! lispy)
(package! lispyville)
;; (package! aggressive-indent)

;; testing
(package! mocha)

;; ;; requires prettier-js
;; (package! prettier-js)

(package! flow-minor-mode)

