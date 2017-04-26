;; -*- no-byte-compile: t; -*-
;;; feature/version-control/packages.el

;;; +git
(package! browse-at-remote)
(package! git-gutter-fringe)
(package! gitconfig-mode)
(package! gitignore-mode)
(package! git-timemachine)
(package! gist)
(package! magit)
(when (featurep! :feature evil)
  (package! evil-magit))
