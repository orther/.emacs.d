;;; lang/borg/autoload/borg.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +borg/insert-item (direction)
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
           (let ((marker (org-element-property :bullet context))
                 (pad (save-excursion
                        (back-to-indentation)
                        (- (point) (line-beginning-position)))))
             (pcase direction
               ('below
                (goto-char (line-end-position))
                (insert (concat  "\n" (make-string pad ? ) marker)))
               ('above
                (goto-char (line-beginning-position))
                (insert (make-string pad ? ) marker)
                (save-excursion (insert "\n")))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))

          ((memq type '(table table-row))
           (pcase direction
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
             (pcase direction
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

          (t (user-error "Not a valid list, heading or table")))

    (when (bound-and-true-p evil-mode)
      (evil-append-line 1))))

;;;###autoload
(defun +borg/toggle-checkbox ()
  "Toggle the presence of a checkbox in the current item."
  (interactive)
  (org-toggle-checkbox '(4)))

;;;###autoload
 (defun +borg/dwim-at-point ()
  "Do-what-I-mean at point. This includes following timestamp links, aligning
tables, toggling checkboxes/todos, executing babel blocks, previewing latex
fragments, opening links, or refreshing images."
  (interactive)
  (let* ((scroll-pt (window-start))
         (context (org-element-context))
         (type (org-element-type context)))
    (cond
     ((memq type '(planning timestamp))
      (org-follow-timestamp-link))

     ((memq type '(table table-row))
      (if (org-element-property :tblfm (org-element-property :parent context))
          (org-table-recalculate t)
        (org-table-align)))

     ((org-element-property :checkbox (org-element-lineage context '(item) t))
      (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
        (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

     ((and (eq type 'headline)
           (string= "ARCHIVE" (car-safe (org-get-tags))))
      (org-force-cycle-archived))

     ((eq type 'headline)
      (org-todo
       (cond
        ((eq (org-element-property :todo-type context) 'done) 'none)
        ((eq (org-element-property :todo-type context) 'todo) 'done)
        (t 'todo))))

     ((org-element-property :bullet (org-element-lineage context '(item) t))
      (+borg/toggle-checkbox))

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
(defun +borg/indent-or-next-field-or-yas-expand ()
  "Depending on the context either a) indent the current line, b) go the next
table field or c) run `yas-expand'."
  (interactive)
  (call-interactively
   (cond ((and (bound-and-true-p yas-minor-mode)
               (yas--templates-for-key-at-point))
          #'yas-expand)
         ((org-at-table-p)
          #'org-table-next-field)
         (t
          #'+org/indent))))
