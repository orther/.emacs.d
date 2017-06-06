;;; tools/neotree/autoload.el

;;;###autoload
(defun +neotree/toggle ()
  "Toggle the neotree window."
  (interactive)
  (let ((in-neotree (and (neo-global--window-exists-p)
                         (window-live-p neo-global--buffer)
                         (eq (current-buffer) neo-global--buffer)))
        (path buffer-file-name))
    (if in-neotree
        (neotree-hide)
      (let ((project-root (doom-project-root)))
        (unless (and (neo-global--window-exists-p)
                     (equal (file-truename (neo-global--with-buffer neo-buffer--start-node))
                            (file-truename project-root)))
          (neotree-dir project-root))
        (neotree-find path project-root)))))

;;;###autoload
(defun +neotree/collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (if (file-directory-p node)
        (if (neo-buffer--expanded-node-p node)
            (+neotree/collapse)
          (neotree-select-up-node))
      (neotree-select-up-node))))

;;;###autoload
(defun +neotree/collapse ()
  "Collapse a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (when (file-directory-p node)
      (neo-buffer--set-expand node nil)
      (neo-buffer--refresh t))
    (when neo-auto-indent-point
      (neo-point-auto-indent))))

;;;###autoload
(defun +neotree/neo-hide-on-enter (type path arg)
  (if (or (and (eq +neotree/neotree-opening-file t)
               (equal type 'file))
          (and (eq +neotree/neotree-entering-dired t)
               (equal type 'directory)))
      (neotree-hide))
  (setq +neotree/neotree-opening-file nil
        +neotree/neotree-entering-dired nil))

;;;###autoload
(defun +neotree/before-neobuffer-execute (arg0 &optional file-fn dir-fn &rest args)
  (when (eq dir-fn 'neo-open-dired)
    (setq +neotree/neotree-entering-dired t))
  (when (or (eq file-fn 'neo-open-file)
            (eq file-fn 'neo-open-file-vertical-split)
            (eq file-fn 'neo-open-file-horizontal-split))
    (setq +neotree/neotree-opening-file t)))

;;;###autoload
(defun +neotree/expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (when-let (node (neo-buffer--get-filename-current-line))
    (if (file-directory-p node)
        (progn
          (neo-buffer--set-expand node t)
          (neo-buffer--refresh t)
          (when neo-auto-indent-point
            (forward-line)
            (neo-point-auto-indent)))
      (call-interactively 'neotree-enter))))
