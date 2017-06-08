;; -*- no-byte-compile: t; -*-
;;; core/packages.el

;; core packages
(package! async)
(package! s)
(package! f)

;; core-os.el
;; In case this config is shared across multiple computers (like mine is), let's
;; protect these from autoremoval.
(package! exec-path-from-shell :ignore (not IS-MAC))
(package! osx-clipboard        :ignore (not IS-MAC))

;; core-ui.el
(package! fringe-helper)
(package! highlight-indentation)
(package! highlight-numbers)
(package! nlinum)
(package! nlinum-hl)
(package! rainbow-delimiters)
(package! vi-tilde-fringe)
(package! visual-fill-column)

;; core-popups.el
(package! shackle)

;; core-editor.el
(package! editorconfig)
(package! smartparens)
(package! ace-link)
(package! ace-window)
(package! avy)
(package! command-log-mode)
(package! expand-region)
(package! goto-last-change)
(package! help-fns+)
(package! imenu-anywhere)
(package! imenu-list)
(package! pcre2el)
(package! smart-forward)
(package! wgrep)

;; core-projects.el
(package! projectile)

;; core-keybinds.el
(package! which-key)
