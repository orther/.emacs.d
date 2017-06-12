;;; private/orther/autoload/orther.el

;;;###autoload
(defun +orther/install-snippets ()
  "Install my snippets from https://github.com/orther/emacs-snippets into
private/orther/snippets."
  (interactive)
  (doom-fetch :github "orther/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private 'orther))))

;; ;;;###autoload
;; (defun +orther/C-u-M-x ()
;;   "Invokes `execute-extended-command' with the universal argument."
;;   (interactive)
;;   (let ((current-prefix-arg 4))
;;     (call-interactively #'execute-extended-command)))

;;;###autoload
(defun +orther/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(defmacro +orther-def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "+orther/find-in-%s" name)) ()
       ,(format "Find a file in %s" (abbreviate-file-name (eval dir)))
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "+orther/browse-%s" name)) ()
       ,(format "Browse files starting from %s" (abbreviate-file-name (eval dir)))
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload '+orther/find-in-templates "private/orther/autoload/orther" nil t)
;;;###autoload (autoload '+orther/browse-templates "private/orther/autoload/orther" nil t)
(+orther-def-finder! templates +file-templates-dir)

;;;###autoload (autoload '+orther/find-in-snippets "private/orther/autoload/orther" nil t)
;;;###autoload (autoload '+orther/browse-snippets "private/orther/autoload/orther" nil t)
(+orther-def-finder! snippets +orther-snippets-dir)

;;;###autoload (autoload '+orther/find-in-dotfiles "private/orther/autoload/orther" nil t)
;;;###autoload (autoload '+orther/browse-dotfiles "private/orther/autoload/orther" nil t)
(+orther-def-finder! dotfiles (expand-file-name ".dotfiles" "~"))

;;;###autoload (autoload '+orther/find-in-emacsd "private/orther/autoload/orther" nil t)
;;;###autoload (autoload '+orther/browse-emacsd "private/orther/autoload/orther" nil t)
(+orther-def-finder! emacsd doom-emacs-dir)

;;;###autoload (autoload '+orther/find-in-notes "private/orther/autoload/orther" nil t)
;;;###autoload (autoload '+orther/browse-notes "private/orther/autoload/orther" nil t)
(+orther-def-finder! notes +org-dir)
