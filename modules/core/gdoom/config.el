;;; core/gdoom/config.el

(defvar +gdoom/large-file-size 1)

(defvar +gdoom/large-file-modes-list
  '(archive-mode tar-mode jka-compr git-commit-mode image-mode
                 doc-view-mode doc-view-mode-maybe ebrowse-tree-mode
                 pdf-view-mode))

(add-hook 'find-file-hook '+gdoom/check-large-file)
