;; completion/ivy/autoload/evil.el -*- lexical-binding: t; -*-
;;;###if (featurep! :feature evil)

;;;###autoload (autoload '+ivy:swiper "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:swiper (&optional search)
  "Invoke `swiper' with SEARCH, otherwise with the symbol at point."
  (interactive "<a>")
  (swiper search))

;;;###autoload (autoload '+ivy:todo "completion/ivy/autoload/evil" nil t)
(evil-define-command +ivy:todo (&optional bang)
  "An ex wrapper around `+ivy/tasks'."
  (interactive "<!>")
  (+ivy/tasks bang))


;; --- file searching ---------------------

(defvar +ivy--file-last-search nil)
(defvar +ivy--file-search-recursion-p t)
(defvar +ivy--file-search-all-files-p nil)

(defun +ivy--file-search (engine beg end query &optional directory)
  (let* ((project-root (doom-project-root))
         (directory (or directory project-root))
         (recursion-p +ivy--file-search-recursion-p)
         (all-files-p +ivy--file-search-all-files-p)
         (engine (or engine
                     (and (executable-find "rg") 'rg)
                     (and (executable-find "ag") 'ag)))
         (query
          (or query
              (if (evil-visual-state-p)
                  (and beg end
                       (> (abs (- end beg)) 1)
                       (rxt-quote-pcre (buffer-substring-no-properties beg end)))
                +ivy--file-last-search)
              +ivy--file-last-search))
         (prompt
          (format "%s%%s %s"
                  (symbol-name engine)
                  (cond ((equal directory default-directory)
                         "./")
                        ((equal directory project-root)
                         (projectile-project-name))
                        (t
                         (file-relative-name directory project-root)))))
         (default-directory directory))
    (setq +ivy--file-last-search query)
    (require 'counsel)
    (cl-letf (((symbol-function 'counsel-ag-function)
               (symbol-function '+ivy*counsel-ag-function))
              ((symbol-function 'counsel-git-grep-function)
               (symbol-function '+ivy*counsel-git-grep-function)))
      (pcase engine
        ('grep
         (let ((args (if recursion-p " -r"))
               (counsel-projectile-grep-initial-input query)
               (default-directory directory))
           (if all-files-p
               (cl-letf (((symbol-function #'projectile-ignored-directories-rel)
                          (symbol-function #'ignore))
                         ((symbol-function #'projectile-ignored-files-rel)
                          (symbol-function #'ignore)))
                 (counsel-projectile-grep args))
             (counsel-projectile-grep args))))
        ('ag
         (let ((args (concat " -S" ; smart-case
                             (if all-files-p " -a")
                             (unless recursion-p " --depth 1"))))
           (counsel-ag query directory args (format prompt args))))
        ('rg
         (let ((args (concat " -S" ; smart-case
                             (if all-files-p " -uu")
                             (unless recursion-p " --maxdepth 1"))))
           (counsel-rg query directory args (format prompt args))))
        ('pt
         (let ((counsel-pt-base-command
                (concat counsel-pt-base-command
                        " -S" ; smart-case
                        (if all-files-p " -U")
                        (unless recursion-p " --depth=1")))
               (default-directory directory))
           (counsel-pt query)))
        (_ (error "No search engine specified"))))))

;;;###autoload (autoload '+ivy:pt "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:pt (beg end query &optional all-files-p directory)
  "Perform a project file search using the platinum searcher. QUERY is a grep
regexp. If omitted, the current selection is used. If no selection is active,
the last known search is used.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'pt beg end query directory)))

;;;###autoload (autoload '+ivy:grep "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:grep (beg end query &optional all-files-p directory)
  "Perform a project file search using grep (or git-grep in git repos). QUERY is
a grep regexp. If omitted, the current selection is used. If no selection is
active, the last known search is used.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'grep beg end query directory)))

;;;###autoload (autoload '+ivy:ag "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag (beg end query &optional all-files-p directory)
  "Perform a project file search using the silver searcher. QUERY is a pcre
regexp. If omitted, the current selection is used. If no selection is active,
the last known search is used.

If ALL-FILES-P, don't respect .gitignore files and search everything."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'ag beg end query directory)))

;;;###autoload (autoload '+ivy:rg "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:rg (beg end query &optional all-files-p directory)
  "Perform a project file search using ripgrep. QUERY is a regexp. If omitted,
the current selection is used. If no selection is active, the last known search
is used.

If ALL-FILES-P, don't respect .gitignore files and search everything.

NOTE: ripgrep doesn't support multiline searches (yet)."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-all-files-p all-files-p))
    (+ivy--file-search 'rg beg end query directory)))


;;;###autoload (autoload '+ivy:pt-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:pt-cwd (beg end query &optional bang)
  "The same as :grep, but searches the current directory. If BANG, don't recurse
into sub-directories."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not bang)))
    (+ivy:pt beg end query t default-directory)))

;;;###autoload (autoload '+ivy:grep-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:grep-cwd (beg end query &optional bang)
  "The same as :grep, but searches the current directory. If BANG, don't recurse
into sub-directories."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not bang)))
    (+ivy:grep beg end query t default-directory)))

;;;###autoload (autoload '+ivy:ag-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:ag-cwd (beg end query &optional bang)
  "The same as :ag, but searches the current directory. If BANG, don't recurse
into sub-directories."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not bang)))
    (+ivy:ag beg end query t default-directory)))

;;;###autoload (autoload '+ivy:rg-cwd "completion/ivy/autoload/evil" nil t)
(evil-define-operator +ivy:rg-cwd (beg end query &optional bang)
  "The same as :rg, but only searches the current directory. If BANG, don't
recurse into sub-directories.

NOTE: ripgrep doesn't support multiline searches (yet)."
  (interactive "<r><a><!>")
  (let ((+ivy--file-search-recursion-p (not bang)))
    (+ivy:rg beg end query t default-directory)))


;;
;; Advice
;;

;;;###autoload
(defun +ivy*counsel-ag-function (string)
  "Advice to get rid of the character limit from `counsel-ag-function'.

NOTE This may need to be updated frequently, to meet changes upstream (in
counsel-rg)."
  (if (< (length string) 1)  ; <-- modified the character limit
      (counsel-more-chars 1) ; <--
    (let ((default-directory (ivy-state-directory ivy-last))
          (regex (counsel-unquote-regex-parens
                  (setq ivy--old-re
                        (ivy--regex string)))))
      (counsel--async-command (format counsel-ag-command
                                      (shell-quote-argument regex)))
      nil)))

;;;###autoload
(defun +ivy*counsel-git-grep-function (string)
  "Advice to get rid of the character limit from `counsel-git-grep-function'.

NOTE This may need to be updated frequently, to meet changes upstream (in
counsel-git-grep)."
  (if (and (> counsel--git-grep-count counsel--git-grep-count-threshold)
           (< (length string) 1)) ; <-- modified the character limit
      (counsel-more-chars 1)      ; <--
    (let* ((default-directory (ivy-state-directory ivy-last))
           (cmd (format counsel-git-grep-cmd
                        (setq ivy--old-re (ivy--regex string t)))))
      (if (<= counsel--git-grep-count counsel--git-grep-count-threshold)
          (split-string (shell-command-to-string cmd) "\n" t)
        (counsel--gg-candidates (ivy--regex string))
        nil))))
