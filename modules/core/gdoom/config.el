;;; core/gdoom/config.el

;; automatically indent pasted code

(defvar gdoom-yank-indent-threshold 1000 "don't auto indent over 1000 lines")

(defvar gdoom-indent-sensitive-modes '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "modes to limit auto indentation on")


(defmacro gdoom|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
  The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun gdoom|yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) gdoom-yank-indent-threshold)
      (indent-region beg end nil)))

(gdoom|advise-commands
  "indent" (evil-paste-before evil-paste-after) around
  "If current mode is not one of gdoom-indent-sensitive-modes
  indent yanked text (with universal arg don't indent)."
  ad-do-it
  (evil-with-single-undo
    (if (and (not (equal '(4) (ad-get-arg 0)))
             (not (member major-mode gdoom-indent-sensitive-modes))
             (derived-mode-p 'prog-mode))
        (let ((transient-mark-mode nil)
              (save-undo buffer-undo-list))
          (gdoom|yank-advised-indent-function (region-beginning)
                                                (region-end))))))


;; MAAASSSIVE IVY HACK (work around not including preselect)
(after! ivy
  (defun ivy--reset-state (state)
  "Reset the ivy to STATE.
This is useful for recursive `ivy-read'."
  (unless (equal (selected-frame) (ivy-state-frame state))
    (select-window (active-minibuffer-window)))
  (let ((prompt (or (ivy-state-prompt state) ""))
        (collection (ivy-state-collection state))
        (predicate (ivy-state-predicate state))
        (history (ivy-state-history state))
        (preselect (ivy-state-preselect state))
        (sort (ivy-state-sort state))
        (re-builder (ivy-state-re-builder state))
        (dynamic-collection (ivy-state-dynamic-collection state))
        (initial-input (ivy-state-initial-input state))
        (require-match (ivy-state-require-match state))
        (caller (ivy-state-caller state)))
    (unless initial-input
      (setq initial-input (cdr (assoc this-command
                                      ivy-initial-inputs-alist))))
    (setq ivy--directory nil)
    (setq ivy-case-fold-search ivy-case-fold-search-default)
    (setq ivy--regex-function
          (or re-builder
              (and (functionp collection)
                   (cdr (assoc collection ivy-re-builders-alist)))
              (and caller
                   (cdr (assoc caller ivy-re-builders-alist)))
              (cdr (assoc this-command ivy-re-builders-alist))
              (cdr (assoc t ivy-re-builders-alist))
              'ivy--regex))
    (setq ivy--subexps 0)
    (setq ivy--regexp-quote 'regexp-quote)
    (setq ivy--old-text "")
    (setq ivy--full-length nil)
    (setq ivy-text "")
    (setq ivy-calling nil)
    (setq ivy-use-ignore ivy-use-ignore-default)
    (let (reb)
      (setq ivy--highlight-function
            (if (and (eq ivy--regex-function 'swiper--re-builder)
                     (setq reb (cdr (assoc t ivy-re-builders-alist)))
                     (setq reb (cdr (assoc reb ivy-highlight-functions-alist))))
                reb
              (or (cdr (assoc ivy--regex-function
                              ivy-highlight-functions-alist))
                  #'ivy--highlight-default))))
    (let (coll sort-fn)
      (cond ((eq collection 'Info-read-node-name-1)
             (if (equal Info-current-file "dir")
                 (setq coll
                       (mapcar (lambda (x) (format "(%s)" x))
                               (cl-delete-duplicates
                                (all-completions "(" collection predicate)
                                :test #'equal)))
               (setq coll (all-completions "" collection predicate))))
            ((eq collection 'read-file-name-internal)
             (setq ivy--directory default-directory)
             (when (and initial-input
                        (not (equal initial-input "")))
               (cond ((file-directory-p initial-input)
                      (when (and (eq this-command 'dired-do-copy)
                                 (equal (file-name-nondirectory initial-input)
                                        ""))
                        (setf (ivy-state-preselect state) (setq preselect nil)))
                      (setq ivy--directory initial-input)
                      (setq initial-input nil)
                      (when preselect
                        (let ((preselect-directory
                               (file-name-directory preselect)))
                          (when (and preselect-directory
                                     (not (equal
                                           (expand-file-name
                                            preselect-directory)
                                           (expand-file-name ivy--directory))))
                            (setf (ivy-state-preselect state)
                                  (setq preselect nil))))))
                     ((ignore-errors
                        (file-exists-p (file-name-directory initial-input)))
                      (setq ivy--directory (file-name-directory initial-input))
                      (setf (ivy-state-preselect state)
                            (file-name-nondirectory initial-input)))))
             (require 'dired)
             (when preselect
               (let ((preselect-directory (file-name-directory preselect)))
                 (unless (or (null preselect-directory)
                             (string= preselect-directory
                                      default-directory))
                   (setq ivy--directory preselect-directory))
                 (setf
                  (ivy-state-preselect state)
                  (setq preselect (file-name-nondirectory preselect)))))
             (setq coll (ivy--sorted-files ivy--directory predicate))
             (when initial-input
               (unless (or require-match
                           (equal initial-input default-directory)
                           (equal initial-input ""))
                 (setq coll (cons initial-input coll)))
               (unless (and (ivy-state-action ivy-last)
                            (not (equal (ivy--get-action ivy-last) 'identity)))
                 (setq initial-input nil))))
            ((eq collection 'internal-complete-buffer)
             (setq coll (ivy--buffer-list "" ivy-use-virtual-buffers predicate)))
            (dynamic-collection
             (setq coll (funcall collection ivy-text)))
            ((and (consp collection) (listp (car collection)))
             (if (and sort (setq sort-fn (ivy--sort-function caller)))
                 (progn
                   (setq sort nil)
                   (setq coll (mapcar #'car
                                      (cl-sort
                                       (copy-sequence collection)
                                       sort-fn))))
               (setq collection
                     (setf (ivy-state-collection ivy-last)
                           (cl-remove-if-not predicate collection)))
               (setq coll (all-completions "" collection)))
             (let ((i 0))
               (ignore-errors
                 ;; cm can be read-only
                 (dolist (cm coll)
                   (add-text-properties 0 1 `(idx ,i) cm)
                   (cl-incf i)))))
            ((or (functionp collection)
                 (byte-code-function-p collection)
                 (vectorp collection)
                 (hash-table-p collection)
                 (and (listp collection) (symbolp (car collection))))
             (setq coll (all-completions "" collection predicate)))
            (t
             (setq coll collection)))
      (when sort
        (if (and (functionp collection)
                 (setq sort-fn (ivy--sort-function collection)))
            (when (not (eq collection 'read-file-name-internal))
              (setq coll (cl-sort coll sort-fn)))
          (when (and (not (eq history 'org-refile-history))
                     (<= (length coll) ivy-sort-max-size)
                     (setq sort-fn (ivy--sort-function caller)))
            (setq coll (cl-sort (copy-sequence coll) sort-fn)))))
      (setq coll (ivy--set-candidates coll))
      (when preselect
        (unless (or (not (stringp preselect))
                    (and require-match
                         (not (eq collection 'internal-complete-buffer)))
                    dynamic-collection
                    (let ((re (regexp-quote preselect)))
                      (cl-find-if (lambda (x) (string-match re x))
                                  coll)))
          (setq coll (cons preselect coll))))
      (setq ivy--old-re nil)
      (setq ivy--old-cands nil)
      (when (integerp preselect)
        (setq ivy--old-re "")
        (ivy-set-index preselect))
      (when initial-input
        ;; Needed for anchor to work
        (setq ivy--old-cands coll)
        (setq ivy--old-cands (ivy--filter initial-input coll)))
      (setq ivy--all-candidates coll)
      (unless (integerp preselect)
        (ivy-set-index (or
                        (and dynamic-collection
                             ivy--index)
                        (and preselect
                             (ivy--preselect-index
                              preselect
                              (if initial-input
                                  ivy--old-cands
                                coll)))
                        0))))
    (setq ivy-exit nil)
    (setq ivy--default
          (if (region-active-p)
              (buffer-substring
               (region-beginning)
               (region-end))
            (ivy-thing-at-point)))
    (setq ivy--prompt (ivy-add-prompt-count prompt))
    (setf (ivy-state-initial-input ivy-last) initial-input))))

