;;; tools/neotree/config.el

(def-package! neotree
  :commands (neotree-show
             neotree-hide
             neotree-toggle
             neotree-dir
             neotree-find
             neo-global--with-buffer
             neo-global--window-exists-p)
  :config
  (setq neo-create-file-auto-open nil
        neo-auto-indent-point nil
        neo-autorefresh nil
        neo-mode-line-type 'none
        neo-window-width 32
        neo-smart-open t
        neo-dont-be-alone t
        neo-persist-show nil
        neo-show-hidden-files t
        neo-show-updir-line nil
        neo-modern-sidebar t
        neo-theme 'nerd ; fallback
        neo-banner-message nil
        neo-confirm-create-file #'off-p
        neo-confirm-create-directory #'off-p
        neo-show-hidden-files nil
        neo-keymap-style 'concise
        neo-hidden-regexp-list
        '(;; vcs folders
          "^\\.\\(git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$"
          "^#.*#$"))

  (when (bound-and-true-p winner-mode)
    (push neo-buffer-name winner-boring-buffers))

  ;; BMACS - Hide neotree on enter file
  (add-hook 'neo-enter-hook #'+neotree/neo-hide-on-enter)
  (advice-add 'neo-buffer--execute :before #'+neotree/before-neobuffer-execute)

  (evil-set-initial-state 'neotree-mode 'motion))
