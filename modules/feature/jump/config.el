;;; feature/jump/config.el

(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-go-other-window dumb-jump-quick-look dumb-jump-back)
  :config
  (setq dumb-jump-default-project doom-emacs-dir
        dumb-jump-selector 'ivy))


(defvar +lookup-search-url-alist
  '(("Google"        . "https://google.com/?q=%s")
    ("DuckDuckGo"    . "https://duckduckgo.com/?q=%s")
    ("DevDocs.io"    . "http://devdocs.io/#q=%s")
    ("StackOverflow" . "https://stackoverflow.com/search?q=%s"))
  "An alist that maps online resources to their search url.")

(set! :popup "*xref*" :size 10 :noselect t :autokill t :autoclose t)

;; Let me control what backends to fall back on
(setq-default xref-backend-functions '())

(def-setting! :xref-backend (mode function)
  "TODO"
  `(add-hook! ,mode
     (add-hook 'xref-backend-functions #',function nil t)))

;; Recenter after certain jumps
(add-hook!
  '(imenu-after-jump-hook evil-jumps-post-jump-hook counsel-grep-post-action-hook)
  'recenter)


;;
;; Packages
;;

(def-package! dumb-jump
  :commands (dumb-jump-go dumb-jump-quick-look dumb-jump-back)
  :config
  (setq dumb-jump-default-project doom-emacs-dir))


;; (def-package! ggtags
;;   :commands (ggtags-find-tag-dwim
;;              ggtags-find-tag-mouse
;;              ggtags-find-definition
;;              ggtags-find-reference
;;              ggtags-find-other-symbol
;;              ggtags-find-tag-regexp
;;              ggtags-idutils-query
;;              ggtags-grep
;;              ggtags-find-file
;;              ggtags-query-replace
;;              ggtags-delete-tags
;;              ggtags-explain-tags))

