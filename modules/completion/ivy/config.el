;;; completion/ivy/config.el

(defvar +ivy-task-tags '(("TODO"  . warning)
                         ("FIXME" . error))
  "An alist of tags for `+ivy/tasks' to include in its search, whose CDR is the
face to render it with.")

(defmacro ivy-do-action! (action)
  "A factory function that returns an interactive lamba that sets the current
ivy action and immediately runs it on the current candidate (ending the ivy
session)."
  `(lambda ()
     (interactive)
     (ivy-set-action ,action)
     (setq ivy-exit 'done)
     (exit-minibuffer)))


;;
;; Packages
;;

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
        "M-v" #'yank
        "M-z" #'undo
        "C-r" #'evil-paste-from-register
        "C-h" (kbd "DEL")
        "C-k" #'ivy-previous-line
        "C-j" #'ivy-next-line
        "C-l" #'ivy-alt-done
        "C-w" #'doom-minibuffer-kill-word
        "C-u" #'doom-minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
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
        [remap execute-extended-command]  #'counsel-M-x
        [remap describe-function]         #'counsel-describe-function
        [remap describe-variable]         #'counsel-describe-variable
        [remap describe-face]             #'counsel-describe-face)

  (when (featurep! :feature workspaces)
    (nconc ivy-sort-functions-alist
           '((persp-kill-buffer   . nil)
             (persp-remove-buffer . nil)
             (persp-add-buffer    . nil)
             (persp-switch        . nil)
             (persp-window-switch . nil)
             (persp-frame-switch  . nil)))))

(def-package! swiper :commands (swiper swiper-all))


(def-package! counsel
  :after ivy
  :config
  (require 'counsel-projectile)

  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  ;; Configure `counsel-rg'/`counsel-ag'
  (set! :popup "^\\*ivy-occur counsel-[ar]g" :size 25 :regexp t :autokill t)

  (ivy-add-actions
   'counsel-rg
   '(("O" +ivy-git-grep-other-window-action "open in other window")))

  (map! :map counsel-ag-map ; applies to counsel-rg too
        [backtab] #'+ivy/wgrep-occur  ; search/replace on results
        "C-SPC"   #'counsel-git-grep-recenter   ; preview
        "M-RET"   (ivy-do-action! #'+ivy-git-grep-other-window-action))

  ;; 1) Gets rid of the character limit from `counsel-ag-function' and
  ;; 2) Disables ivy's over-zealous parentheses quoting behavior
  ;;
  ;; These both interfere with my custom :[ar]g ex command `+ivy:file-search'.
  (advice-add #'counsel-ag-function :override #'+ivy*counsel-ag-function))

;; Used by `counsel-M-x'
(def-package! smex
  :commands (smex smex-major-mode-commands)
  :config
  (setq smex-save-file (concat doom-cache-dir "/smex-items"))
  (smex-initialize))
