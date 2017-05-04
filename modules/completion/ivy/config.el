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


  (ivy-set-actions
    'counsel-bookmark
    '(("d" bookmark-delete "delete")
      ("e" bookmark-rename "edit")
      ("w" bookmark-set    "overwrite")))

  (map! :map ivy-minibuffer-map
        [escape] #'keyboard-escape-quit
        "C-y" #'yank
        "C-w" #'backward-kill-word
        "C-u" #'backward-kill-sentence
        "C-h" (kbd "DEL")
        "C-l" #'ivy-alt-done
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line
        "<C-return>" #'ivy-immediate-done)

  (map! :map ivy-occur-grep-mode-map
        (:desc "ivy occur actions"
         :prefix ","
         :desc "switch to ivy wgrep mode"    :n "," 'ivy-wgrep-change-to-wgrep-mode
         :desc "switch to ivy wgrep mode"    :n "w" 'ivy-wgrep-change-to-wgrep-mode))

  (map! :map ivy-mode-map
        [remap describe-face]             #'counsel-describe-face
        [remap find-file]                 #'counsel-find-file
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

  (require 'counsel-projectile))

;; Used by `counsel-M-x'
(def-package! smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat doom-cache-dir "/smex-items"))
  (smex-initialize))
