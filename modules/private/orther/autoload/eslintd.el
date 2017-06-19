;;; private/orther/autoload/eslintd.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +orther/eslintd-set-executable ()
  (interactive)
  (when-let (eslintd-executable (executable-find "eslint_d"))
    (make-variable-buffer-local 'flycheck-javascript-eslint-executable)
    (setq flycheck-javascript-eslint-executable eslintd-executable)
    (setq eslintd-fix-executable eslintd-executable)))
