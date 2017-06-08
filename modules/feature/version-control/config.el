;;; feature/version-control/config.el -*- lexical-binding: t; -*-

(load! +git)

(after! vc-annotate
  (set! :popup
    '("*vc-diff*" :size 15 :noselect t)
    '("*vc-change-log*" :size 15)
    '(vc-annotate-mode :same t))

  (set! :evil-state
    '(vc-annotate-mode . normal)
    '(vc-git-log-view-mode . normal)))
