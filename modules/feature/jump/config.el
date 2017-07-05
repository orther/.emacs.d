;;; feature/jump/config.el -*- lexical-binding: t; -*-

;; "What am I looking at?"
;;
;; This module helps you answer that question. It helps you look up whatever
;; you're looking at.
;;
;;   + `+jump/definition': a jump-to-definition that should 'just work'
;;   + `+jump/references': find a symbol's references in the current project
;;   + `+jump/online'; look up a symbol on online resources, like stackoverflow,
;;     devdocs.io or google.
;;

(defvar +jump-search-url-alist
  '(("Google"        . "https://google.com/search?q=%s")
    ("DuckDuckGo"    . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"    . "http://devdocs.io/#q=%s")
    ("StackOverflow" . "https://stackoverflow.com/search?q=%s"))
  "An alist that maps online resources to their search url or a function that
produces an url. Used by `+jump/online'.")

;; Recenter after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook
    counsel-grep-post-action-hook dumb-jump-after-jump-hook)
  #'recenter)

;;
;; Packages
;;

(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look dumb-jump-go-other-window
             dumb-jump-back dumb-jump-result-follow)
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-aggressive nil
        dumb-jump-use-visible-window nil
        dumb-jump-selector 'ivy))
