;;; completion/ivy/config.el

(def-package! ivy :demand t
  :config
  (setq ivy-height 14
        ivy-do-completion-in-region nil
        ivy-wrap t
        ivy-fixed-height-minibuffer t
        projectile-completion-system 'ivy
        smex-completion-method 'ivy
        ;; Don't use ^ as initial input
        ivy-initial-inputs-alist nil
        ;; highlight til EOL
        ivy-format-function #'ivy-format-function-line)

  (after! magit      (setq magit-completing-read-function #'ivy-completing-read))
  (after! yasnippet  (push #'+ivy-yas-prompt yas-prompt-functions))

  (ivy-mode +1)

  (map! :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-y" #'evil-paste-from-register
        "C-w" #'backward-kill-word
        "C-u" #'backward-kill-sentence
        "C-h" (kbd "DEL")
        "C-l" #'ivy-alt-done
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line)

  (map! :map ivy-mode-map
        [remap describe-face]             #'counsel-describe-face
        [remap find-file]                 #'counsel-find-file
        [remap switch-to-buffer]          #'+ivy/switch-buffer
        [remap persp-switch-to-buffer]    #'+ivy/switch-workspace-buffer
        [remap recentf]                   #'counsel-recentf
        [remap imenu]                     #'counsel-imenu
        [remap bookmark-jump]             #'counsel-bookmark
        [remap projectile-switch-project] #'counsel-projectile-switch-project
        [remap projectile-find-file]      #'counsel-projectile-find-file
        [remap imenu-anywhere]            #'ivy-imenu-anywhere
        [remap execute-extended-command]  #'counsel-M-x))

(def-package! swiper :commands (swiper swiper-all))


(def-package! counsel
  :after ivy
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  (set! :popup "^\\*ivy-occur counsel-ag" :size 25 :regexp t :autokill t)

  (require 'counsel-projectile)

  (add-hook! 'doom-popup-mode-hook
    (when (eq major-mode 'ivy-occur-grep-mode)
      (ivy-wgrep-change-to-wgrep-mode))))


;; Used by `counsel-M-x'
(def-package! smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat doom-cache-dir "/smex-items"))
  (smex-initialize))
