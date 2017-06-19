;;; private/orther/+eslintd.el

;; (after! prettier-js
;;   (add-hook 'web-mode-hook 'prettier-js-mode))

;; (def-package! prettier-js
;;   ;; :commands prettier-js prettier-js-mode
;;   :after
;;   prettier-js
;;   :init (add-hook 'web-mode-hook #'prettier-js-mode)
;;   :config
;;   (setq prettier-js-args
;;         '("--trailing-comma" "all"
;;           "--single-quote" "true")))

(after! js2-mode
  (+orther/eslintd-set-executable))

(def-package! eslintd-fix
  :commands eslintd-fix-mode
  :after eslintd-fix
  :init (add-hook 'js2-mode-hook #'eslintd-fix-mode t))
