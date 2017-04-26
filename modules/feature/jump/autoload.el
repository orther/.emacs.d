;;; feature/jump/autoload.el

;;;###autoload
(defun +jump/online (where &optional search)
  "TODO"
  (interactive
   (list (completing-read "Search on: "
                          (mapcar #'car +lookup-search-url-alist)
                          nil t)))
  (let ((url (cdr (assoc where +lookup-search-url-alist)))
        (search (or search (read-string "Query: "))))
    (browse-url (format url (url-encode-url search)))))
