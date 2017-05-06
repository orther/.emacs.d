;;; feature/version-control/+git.el

(def-package! gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(def-package! gitignore-mode
  :mode "/\\.gitignore$")

(def-package! git-gutter-fringe
  :commands git-gutter-mode
  :init
  (defun +version-control|git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode +1)))
  (add-hook! (text-mode prog-mode conf-mode) #'+version-control|git-gutter-maybe)
  :config
  (set! :popup "^\\*git-gutter.+\\*$" :regexp t :size 15 :noselect t)

  ;; Update git-gutter on focus (in case I was using git externally)
  (add-hook 'focus-in-hook #'git-gutter:update-all-windows)

  (after! evil
    ;; Refreshing git-gutter on ESC
    (advice-add #'evil-force-normal-state :after #'git-gutter)))

(def-package! gist
  :commands (gist-list gist-buffer gist-region gist-buffer-private gist-region-private))

(def-package! git-timemachine
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (require 'magit-blame)

  ;; Force evil to rehash keybindings for the current state
  (add-hook 'git-timemachine-mode-hook #'evil-force-normal-state)
  (map! :map git-timemachine-mode-map
        :nv "p" #'git-timemachine-show-previous-revision
        :nv "n" #'git-timemachine-show-next-revision
        :nv "g" #'git-timemachine-show-nth-revision
        :nv "q" #'git-timemachine-quit
        :nv "w" #'git-timemachine-kill-abbreviated-revision
        :nv "W" #'git-timemachine-kill-revision
        :nv "b" #'git-timemachine-blame))

(def-package! magit
  :commands magit-status
  :config
  (set! :popup "^\\*magit" :regexp t)
  (map! :map magit-mode-map
        ;; Don't interfere with window movement keys
        :nv "C-j" nil
        :nv "C-k" nil))

(def-package! git-link
  :commands (git-link git-link-commit git-link-homepage))

(def-package! evil-magit
  :when (featurep! :feature evil)
  :after magit)
