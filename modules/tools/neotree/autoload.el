;;; tools/neotree/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +neotree/toggle ()
  "Toggle the neotree window."
  (interactive)
  (let ((path buffer-file-name)
        (project-root (doom-project-root)))
    (require 'neotree)
    (if (and (neo-global--window-exists-p)
             (get-buffer-window neo-buffer-name t))
        (neotree-hide)
      (progn
        (if project-root
            (neotree-dir project-root))
        (if path
            (neotree-find path))))))

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
    (cond ((file-directory-p node)
           (neo-buffer--set-expand node t)
           (neo-buffer--refresh t)
           (when neo-auto-indent-point
             (forward-line)
             (neo-point-auto-indent)))
          (t
           (call-interactively #'neotree-enter)))))
