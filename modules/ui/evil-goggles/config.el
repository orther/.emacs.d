;;; ui/evil-goggles/config.el -*- lexical-binding: t; -*-

(def-package! evil-goggles
  :when (featurep! :feature evil)
  :commands evil-goggles-mode
  :init
  (setq evil-goggles-duration 0.2)
  (add-hook 'doom-post-init-hook #'evil-goggles-mode t)
  :config
  (evil-goggles-use-diff-faces))
