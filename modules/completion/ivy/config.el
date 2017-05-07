;;; completion/ivy/config.el

;; Ivy is my completion backend of choice. With counsel's help, I get:
;;
;; + Project-wide search with `counsel-rg' (and `+ivy:file-search')
;; + Project-wide replace if you press <backtab> in the ag occur buffer.
;; + An Atom/Sublime-Text Command-T implementation with `counsel-find-file' and
;;   `counsel-projectile-find-file'.
;; + Ido-like completion for a slew of functions, like `counsel-M-x' and
;;   `counsel-imenu'.

;; TODO Make this a setting
(defmacro def-counsel-action! (name &rest forms)
  `(defun ,(intern (format "+ivy/counsel-%s" (symbol-name name))) ()
     (interactive)
     (ivy-set-action ',@forms)
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
        "C-r" #'evil-paste-from-register
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
        [remap execute-extended-command]  #'counsel-M-x))

(def-package! swiper :commands (swiper swiper-all))


(def-package! counsel
  :after ivy
  :config
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)")

  (set! :popup "^\\*ivy-occur counsel-[ar]g" :size 25 :regexp t :autokill t)

  (require 'counsel-projectile)

  ;; FIXME Messy workaround, refactor this
  (def-counsel-action! ag-open-in-other-window
    (lambda (x)
      (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" x)
        (let ((file-name (match-string-no-properties 1 x))
              (line-number (match-string-no-properties 2 x))
              dest-win)
          (with-ivy-window
            (find-file-other-window (expand-file-name file-name counsel--git-grep-dir))
            (setq dest-win (selected-window))
            (forward-line (1- (string-to-number line-number)))
            (re-search-forward (ivy--regex ivy-text t) (line-end-position) t)
            (recenter)
            (swiper--ensure-visible)
            (run-hooks 'counsel-grep-post-action-hook)
            (unless (eq ivy-exit 'done)
              (swiper--cleanup)
              (swiper--add-overlays (ivy--regex ivy-text))))
          (when dest-win
            (select-window dest-win))))))

  (map! :map counsel-ag-map ; applies to counsel-rg too
        [backtab] #'+ivy/wgrep-occur  ; search/replace on results
        "C-SPC"   #'counsel-git-grep-recenter   ; preview
        "M-RET"   #'+ivy/counsel-ag-open-in-other-window)

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
