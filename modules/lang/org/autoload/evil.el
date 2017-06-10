;;; lang/org/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+org:capture "lang/org/autoload/evil" nil t)
(evil-define-operator +org:capture (&optional beg end bang)
  "Send a selection to `doom/org-capture'."
  :move-point nil :type inclusive
  (interactive "<r><!>")
  (org-capture-string
   (when (and (evil-visual-state-p) beg end)
     (buffer-substring beg end))))

;;;###autoload (autoload '+org:attach "lang/org/autoload/evil" nil t)
(evil-define-command +org:attach (&optional uri)
  (interactive "<a>")
  (unless (eq major-mode 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (if uri
      (let* ((rel-path (org-download--fullname uri))
             (new-path (expand-file-name rel-path))
             (image-p (image-type-from-file-name uri)))
        (cond ((string-match-p (concat "^" (regexp-opt '("http" "https" "nfs" "ftp" "file")) ":/") uri)
               (url-copy-file uri new-path))
              (t (copy-file uri new-path)))
        (unless new-path
          (user-error "No file was provided"))
        (if (evil-visual-state-p)
            (org-insert-link nil (format "./%s" rel-path)
                             (concat (buffer-substring-no-properties (region-beginning) (region-end))
                                     " " (doom--org-attach-icon rel-path)))

          (insert (if image-p
                      (format "[[./%s]] " rel-path)
                    (format "%s [[./%s][%s]] "
                            (doom--org-attach-icon rel-path)
                            rel-path (file-name-nondirectory (directory-file-name rel-path))))))
        (when (string-match-p (regexp-opt '("jpg" "jpeg" "gif" "png")) (file-name-extension rel-path))
          (org-redisplay-inline-images)))
    (let ((default-directory ".attach/"))
      (if (file-exists-p default-directory)
          (call-interactively 'find-file)
        (user-error "No attachments")))))
