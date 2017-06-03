;;; private/gilbertw1/autoload

;;;###autoload
(defun +gilbertw1/delete-git-index-lock ()
  (interactive)
  (let ((git-index-lock-file (concat (magit-git-dir) "index.lock")))
    (when (file-exists-p git-index-lock-file)
      (delete-file git-index-lock-file))))
