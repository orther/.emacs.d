;;; module-haskell.el

(def-package! haskell-mode
  :mode "\\.hs$"
  :mode ("\\.ghci$" . ghci-script-mode)
  :mode ("\\.cabal$" . haskell-cabal-mode)
  :interpreter (("runghc" . haskell-mode)
                ("runhaskell" . haskell-mode))
  :config
  (load "haskell-mode-autoloads" nil t)
  (add-hook 'haskell-mode-hook #'hindent-mode)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook #'structured-haskell-mode)

  (set! :repl 'haskell-mode #'switch-to-haskell)

  (push ".hi" completion-ignored-extensions)

  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t))

  (autoload 'switch-to-haskell "inf-haskell" nil t)
  (after! inf-haskell
    (map! :map inf-haskell-mode-map "ESC ESC" #'doom/popup-close)))

(def-package! ghc
  :commands (ghc-init ghc-debug))

(def-package! hindent
  :commands (hindent-mode))

(def-package! company-ghc
  :commands (company-ghc))

(def-package! shm
  :commands (structured-haskell-mode))

(def-package! dante
  :after haskell-mode
  :config
  (if (executable-find "cabal")
    (add-hook! 'haskell-mode-hook
      #'(flycheck-mode dante-mode interactive-haskell-mode))
    (warn "haskell-mode: couldn't find cabal")))

(def-package! company-ghc
  :after haskell-mode
  :config
  (set! :company-backend 'haskell-mode #'company-ghc)
  (setq company-ghc-show-info 'oneline)
  (if (executable-find "ghc-mod")
      (add-hook 'haskell-mode-hook #'ghc-comp-init)
    (warn "haskell-mode: couldn't find ghc-mode")))
