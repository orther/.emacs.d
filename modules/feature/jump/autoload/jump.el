;;; feature/jump/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +jump/definition (&optional other-window)
  "Jump to the definition of the symbol at point using `dumb-jump'"
  (interactive)
  (evil--jumps-push)
  (if other-window
      (dumb-jump-go-other-window)
    (dumb-jump-go)))

;;;###autoload
(defun +jump/definition-other-window ()
  (interactive)
  (+jump/definition t))

(defvar +search--online-last nil)

;;;###autoload
(defun +search/online (&optional where search)
  (interactive)
  (let ((provider (cond (where where)
                        (t (completing-read (format "Search on (%s): " (thing-at-point 'symbol t))
                                            (mapcar #'car +jump-search-url-alist)
                                            nil t))))
        (query (cond (search search)
                     ((region-active-p)
                      (buffer-substring-no-properties (region-beginning) (region-end)))
                     (t (read-string "Query: ")))))
    (condition-case _ex
        (let ((url (cdr (assoc provider +jump-search-url-alist))))
          (unless url
            (error "'%s' is an invalid search engine" provider))
          (when (or (functionp url) (symbolp url))
            (setq url (funcall url)))
          (cl-assert (and (stringp url) (not (string-empty-p url))))
          (when (string-empty-p query)
            (user-error "The search query is empty"))
          (setq +search--online-last provider)
          (browse-url (format url (url-encode-url query))))
      ('error (setq +search--online-last nil)))))

;;;###autoload
(defun +search/online-use-last (&optional search)
  (interactive)
  (+search/online +search--online-last search))

;;;###autoload
(defun +jump/online (&optional where)
  "Looks up SEARCH online, in you browser, as dictated by WHERE.

Interactively, you are prompted to choose a source from
`+jump-search-url-alist'."
  (interactive)
  (+search/online where (thing-at-point 'symbol t)))

;;;###autoload
(defun +jump/online-use-last ()
  (interactive)
  (+jump/online +search--online-last))
