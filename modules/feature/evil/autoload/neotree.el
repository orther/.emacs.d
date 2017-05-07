;;; feature/evil/autoload/neotree.el

;;;###autoload
(defun +evil/neotree ()
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
(defun +evil/neotree-expand-or-open ()
  "Expand or open a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (next-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

;;;###autoload
(defun +evil/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (+evil/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

;;;###autoload
(defun +evil/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

;;;###autoload
(defun +evil/neo-hide-on-enter (type path arg)
  (if (or (and (eq +evil/neotree-opening-file t)
               (equal type 'file))
          (and (eq +evil/neotree-entering-dired t)
               (equal type 'directory)))
      (neotree-hide))
  (setq +evil/neotree-opening-file nil
        +evil/neotree-entering-dired nil))

;;;###autoload
(defun +evil/before-neobuffer-execute (arg0 &optional file-fn dir-fn &rest args)
  (when (eq dir-fn 'neo-open-dired)
    (setq +evil/neotree-entering-dired t))
  (when (or (eq file-fn 'neo-open-file)
            (eq file-fn 'neo-open-file-vertical-split)
            (eq file-fn 'neo-open-file-horizontal-split))
    (setq +evil/neotree-opening-file t)))
