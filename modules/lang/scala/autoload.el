;;; lang/scala/autoloads.el

;;;###autoload
(defun scala/configure-ensime ()
  "Ensure the file exists before starting `ensime-mode'."
  (require 'ensime-company)
  (cond
   ((and (buffer-file-name) (file-exists-p (buffer-file-name)))
    (ensime-mode +1))
   ((buffer-file-name)
(add-hook 'after-save-hook (lambda () (ensime-mode +1)) nil t))))

;;;###autoload
(defun scala/enable-eldoc ()
  (setq-local eldoc-documentation-function
              (lambda ()
                (when (ensime-connected-p)
                  (ensime-print-type-at-point))))
  (eldoc-mode +1))
