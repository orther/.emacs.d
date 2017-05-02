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
