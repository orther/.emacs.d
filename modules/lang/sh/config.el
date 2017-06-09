;;; lang/sh/config.el -*- lexical-binding: t; -*-

(def-package! sh-script ; built-in
  :mode ("\\.zsh$"   . sh-mode)
  :mode ("/bspwmrc$" . sh-mode)
  :init
  (add-hook! sh-mode #'(flycheck-mode highlight-numbers-mode +sh|extra-fontify))
  :config
  (set! :electric 'sh-mode :words '("else" "elif" "fi" "done" "then" "do" "esac" ";;"))
  (set! :repl 'sh-mode #'+sh/repl)
  (setq sh-indent-after-continuation 'always)

  ;; sh-mode has file extensions checks for other shells, but not zsh, so...
  (defun +sh|detect-zsh ()
    (when (and buffer-file-name (string-match-p "\\.zsh\\'" buffer-file-name))
      (sh-set-shell "zsh")))
  (add-hook 'sh-mode-hook #'+sh|detect-zsh))


(def-package! company-shell
  :when (featurep! :completion company)
  :after sh-script
  :config
  (set! :company-backend 'sh-mode '(company-shell))
  (setq company-shell-delete-duplicates t))

