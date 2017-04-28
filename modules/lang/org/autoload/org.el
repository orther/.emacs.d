;;; lang/org/autoload/org.el

(defun +org--get-context (types &optional context)
  (let ((context (or context (org-element-context))))
    (while (and context (not (memq (car context) types)))
      (setq context (plist-get (cadr context) :parent)))
    context))

(defun +org--get-types (context)
  (let ((context (or context (org-element-context)))
        types)
    (while context
      (push (car context) types)
      (setq context (plist-get (cadr context) :parent)))
    types))

;;;###autoload
(defun +org/indent ()
  "Indent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (org-indent-item-tree))
        ((org-at-heading-p)
         (ignore-errors (org-demote)))
        ((org-in-src-block-p t)
         (doom/dumb-indent))
        (t (call-interactively 'self-insert-command))))

;;;###autoload
(defun +org/indent-or-next-field-or-yas-expand ()
  "Depending on the context either a) indent the current line, b) go the next
table field or c) run `yas-expand'."
  (interactive)
  (call-interactively
   (cond ((and (bound-and-true-p yas-minor-mode)
               (yas--templates-for-key-at-point))
          'yas-expand)
         ((org-at-table-p)
          'org-table-next-field)
         (t
          '+org/indent))))

;;;###autoload
(defun +org/dedent ()
  "Dedent the current item (header or item). Otherwise, forward to
`self-insert-command'."
  (interactive)
  (cond ((org-at-item-p)
         (let ((struct (if (org-region-active-p)
                           (save-excursion (goto-char (region-beginning))
                                           (org-list-struct))
                         (org-list-struct))))
           (org-list-indent-item-generic -1 nil struct)))
        ((org-at-heading-p)
         (ignore-errors (org-promote)))
        (t (call-interactively 'self-insert-command))))

;;;###autoload
(defun +org/dedent-or-prev-field ()
  "Depending on the context either dedent the current item or go the previous
table field."
  (interactive)
  (call-interactively (if (org-at-table-p) 'org-table-previous-field '+org/dedent)))

;;;###autoload
(defun +org/insert-item (direction)
  "Inserts a new heading, table cell or item, depending on the context.
DIRECTION can be 'above or 'below.

I use this instead of `org-insert-item' or `org-insert-heading' which are too
opinionated and perform this simple task incorrectly (e.g. whitespace in the
wrong places)."
  (interactive)
  (let* ((context (org-element-lineage
                   (org-element-context)
                   '(table table-row headline inlinetask item plain-list)
                   t))
         (type (org-element-type context)))
    (cond ((eq type 'item)
           (let ((marker (org-element-property :bullet context)))
             (pcase direction
               ('below
                (goto-char (line-end-position))
                (insert (concat "\n" marker)))
               ('above
                (goto-char (line-beginning-position))
                (insert marker)
                (save-excursion (insert "\n")))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))
          ((memq type '(table table-row))
           (cl-case direction
             ('below (org-table-insert-row t))
             ('above (+org/table-prepend-row-or-shift-up))))
          ((memq type '(headline inlinetask plain-list))
           (let* ((subcontext (org-element-context))
                  (level (save-excursion
                           (org-back-to-heading)
                           (org-element-property
                            :level
                            (if (eq (org-element-type subcontext) 'headline)
                                subcontext
                              1)))))
             (cl-case direction
               ('below
                (let ((at-eol (= (point) (1- (line-end-position)))))
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert (concat "\n"
                                  (when (= level 1)
                                    (if at-eol
                                        (ignore (cl-incf level))
                                      "\n"))
                                  (make-string level ?*)
                                  " "))))
               ('above
                (org-back-to-heading)
                (org-insert-heading)
                (when (= level 1)
                  (save-excursion (evil-open-above 1))
                  (save-excursion (insert "\n")))))
             (when (org-element-property :todo-type context)
               (org-todo 'todo))))
          (t (user-error "Not a valid list")))
    (evil-append-line 1)))

;;;###autoload
(defun +org/toggle-checkbox ()
  (interactive)
  (when-let (context (org-element-lineage (org-element-context) '(item) t))
    (org-end-of-line)
    (org-beginning-of-line)
    (if (org-element-property :checkbox context)
        (when (search-backward-regexp "\\[[ +-]\\]" (line-beginning-position) t)
          (delete-char 4))
      (insert "[ ] "))))

;;;###autoload
(defun +org/toggle-fold ()
  "Toggle the local fold at the point (as opposed to cycling through all levels
with `org-cycle'). Also removes babel result blocks, if run from a code block."
  (interactive)
  (org-babel-when-in-src-block
   (call-interactively 'org-babel-remove-result-one-or-many))
  (cond ((org-at-heading-p)
         (outline-toggle-children))
        ((org-at-item-p)
         (let ((window-beg (window-start)))
           (org-cycle)
           (set-window-start nil window-beg)))))

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point. This includes following timestamp links, aligning
tables, toggling checkboxes/todos, executing babel blocks, previewing latex
fragments, opening links, or refreshing images."
  (interactive)
  (let* ((scroll-pt (window-start))
         (context (org-element-context))
         (type (org-element-type context))
         (value (org-element-property :value context)))
    (cond
     ((memq type '(planning timestamp))
      (org-follow-timestamp-link))

     ((memq type '(table table-row))
      (if (org-element-property :tblfm (org-element-property :parent context))
          (org-table-recalculate t)
        (org-table-align)))

     ((org-element-property :checkbox (org-element-lineage context '(item) t))
      (org-toggle-checkbox))

     ((and (eq type 'headline)
           (org-element-property :todo-type context))
      (org-todo
       (if (eq (org-element-property :todo-type context) 'done) 'todo 'done)))

     ((and (eq type 'headline)
           (string= "ARCHIVE" (car-safe (org-get-tags))))
      (org-force-cycle-archived))

     ((eq type 'headline)
      (org-remove-latex-fragment-image-overlays)
      (org-toggle-latex-fragment '(4)))

     ((eq type 'babel-call)
      (org-babel-lob-execute-maybe))

     ((memq type '(src-block inline-src-block))
      (org-babel-execute-src-block))

     ((memq type '(latex-fragment latex-environment))
      (org-toggle-latex-fragment))

     ((eq type 'link)
      (let ((path (org-element-property :path (org-element-lineage context '(link) t))))
        (if (and path (image-type-from-file-name path))
            (+org/refresh-inline-images)
          (org-open-at-point))))

     (t (+org/refresh-inline-images)))
    (set-window-start nil scroll-pt)))

;;;###autoload
(defun +org/refresh-inline-images ()
  "Refresh image previews in the current heading/tree."
  (interactive)
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
         (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
         (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))

;;;###autoload
(defun +org-surround (delim)
  "Surround the cursor (or selected region) with DELIM."
  (if (region-active-p)
      (save-excursion
        (goto-char (region-beginning))
        (insert delim)
        (goto-char (region-end))
        (insert delim))
    (insert delim)
    (save-excursion (insert delim))))


;;
;; tables
;;

;;;###autoload
(defun +org/table-next-row ()
  (interactive)
  (if (org-at-table-p) (org-table-next-row) (org-down-element)))

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (if (org-at-table-p)
      (progn
        (org-table-maybe-eval-formula)
        (org-table-maybe-recalculate-line)
        (if (and org-table-automatic-realign
                 org-table-may-need-update)
            (org-table-align))
        (let ((col (org-table-current-column)))
          (beginning-of-line 0)
          (when (or (not (org-at-table-p)) (org-at-table-hline-p))
            (beginning-of-line))
          (org-table-goto-column col)
          (skip-chars-backward "^|\n\r")
          (when (org-looking-at-p " ") (forward-char))))
    (org-up-element)))

;;;###autoload
(defun +org/table-next-field ()
  (interactive)
  (if (org-at-table-p) (org-table-next-field) (org-end-of-line)))

;;;###autoload
(defun +org/table-previous-field ()
  (interactive)
  (if (org-at-table-p) (org-table-previous-field) (org-beginning-of-line)))

;;;###autoload
(defun +org/table-append-field-or-shift-right ()
  (interactive)
  (org-shiftmetaright)
  (when (org-at-table-p) (org-metaright)))

;;;###autoload
(defun +org/table-prepend-field-or-shift-left ()
  (interactive)
  (if (org-at-table-p) (org-shiftmetaright) (org-shiftmetaleft)))

;;;###autoload
(defun +org/table-append-row-or-shift-down ()
  (interactive)
  (org-shiftmetadown)
  (when (org-at-table-p) (org-metadown)))

;;;###autoload
(defun +org/table-prepend-row-or-shift-up ()
  (interactive)
  (if (org-at-table-p)
      (org-shiftmetadown)
    (org-shiftmetaup)))

;;;###autoload
(defun +org/edit-special-same-window ()
  (interactive)
  (let ((shackle-rules '(("^\\*Org Src" :align t :select t :regexp t :noesc t :same t))))
    (call-interactively 'org-edit-special)
    (doom-buffer-mode +1)))
