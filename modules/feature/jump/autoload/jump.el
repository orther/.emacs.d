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

(defvar +jump--online-last nil)

;;;###autoload
(defun +jump/online (where search)
  "Looks up SEARCH online, in you browser, as dictated by WHERE.

Interactively, you are prompted to choose a source from
`+jump-search-url-alist'."
  (interactive
   (list (or (and (not current-prefix-arg)
                  +jump--online-last)
             (completing-read (format "Search on (%s): " (thing-at-point 'symbol t))
                              (mapcar #'car +jump-search-url-alist)
                              nil t))
         (thing-at-point 'symbol t)))
  (condition-case _ex
      (let ((url (cdr (assoc where +jump-search-url-alist))))
        (unless url
          (error "'%s' is an invalid search engine" where))
        (when (or (functionp url) (symbolp url))
          (setq url (funcall url)))
        (cl-assert (and (stringp url) (not (string-empty-p url))))
        (when (string-empty-p search)
          (user-error "The search query is empty"))
        (setq +jump--online-last where)
        (browse-url (format url (url-encode-url search))))
    ('error (setq +jump--online-last nil))))
