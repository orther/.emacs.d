;;; private/gilbertw1/+bindings.el

(defmacro find-file-in! (path &optional project-p)
  "Returns a interactive function for searching files"
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(if project-p
              (command-remapping 'projectile-find-file)
            (command-remapping 'find-file))))))

(define-key input-decode-map [?\C-i] [C-i])

(setq doom-leader-key "SPC")
(setq doom-localleader-key "SPC m")

(map!
 ;; Essential
 "M-x"    #'execute-extended-command
 "A-x"    #'execute-extended-command
 "M-;"    #'eval-expression
 "A-;"    #'eval-expression
 ;; Tools
 "C-`"    #'doom/popup-toggle
 ;; Text-scaling
 "M-0"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease
 ;; Temporary escape into emacs mode
 :e [C-escape] #'evil-normal-state
 :n [C-escape] #'evil-emacs-state

 "M-r"  #'+eval/buffer
 "M-b"  #'+eval/build

 :nvi "TAB"     #'indent-for-tab-command

  ;;; <leader> and <localleader>
  :m ";" #'evil-ex
  (:leader
    ;; common
    :desc "Blink cursor line"          :nv "SPC" #'+doom/blink-cursor
    :desc "Counsel M-x"                :nv ":" #'counsel-M-x
    :desc "Eval expr"                  :nv ";" #'eval-expression
    :desc "Run shell command"          :nv "!" #'shell-command
    :desc "Find file in project"       :nv "/" #'counsel-projectile-rg
    :desc "Find file from here"        :nv "?" #'counsel-rg
    :desc "Find selection in project"  :nv "*" #'counsel-projectile-rg-region-or-symbol
    :desc "Switch to Emacs mode"       :n "|" #'evil-emacs-state
    :desc "Switch to last buffer"      :nv "TAB" #'+gdoom/alternate-buffer
    :desc "Ace window"                 :nv "W" #'ace-window
    :desc "Pop terminal"               :nv "'" #'+term/popup

    (:desc "file"
      :prefix "f"
      :desc "Open file from here"      :nv "f" #'counsel-find-file
      :desc "Sudo open file from here" :nv "F" #'doom/sudo-find-file
      :desc "Copy file"                :nv "c" #'copy-file
      :desc "Copy current file"        :nv "C" #'+gdoom/copy-file
      :desc "Rename current file"      :nv "R" #'+gdoom/rename-current-buffer-file
      :desc "Delete file"              :nv "d" #'+gdoom/delete-file-confirm
      :desc "Delete current file"      :nv "D" #'+gdoom/delete-current-buffer-file
      :desc "Sudo edit file"           :nv "E" #'+gdoom/sudo-edit
      :desc "Recent files"             :nv "r"  #'counsel-recentf
      :desc "Neotree toggle"           :nv "t"  #'neotree-toggle
      :desc "Save buffer"              :nv "s"  #'save-buffer
      :desc "Save all buffers"         :nv "S"  #'evil-write-all
      :desc "Counsel bookmark"         :nv "b"  #'counsel-bookmark
      :desc "Remote ssh tramp"         :nv "i"  (lambda ()
                                                 (interactive)
                                                 (counsel-find-file "/ssh:"))
      :desc "Show and copy filename"   :nv "y" #'+gdoom/show-and-copy-buffer-filename
      )

    (:desc "buffer"
      :prefix "b"
      :desc "Select entire buffer"     :nv "a" #'mark-whole-buffer
      :desc "Switch workspace buffer"  :nv "b" #'+ivy/switch-workspace-buffer
      :desc "Switch buffer"            :nv "B" #'+ivy/switch-buffer
      :desc "Kill buffer"              :nv "d" #'kill-this-buffer
      :desc "Kill other buffers"       :nv "D" #'doom/kill-other-buffers
      :desc "Kill a buffer"            :nv "k" #'kill-buffer
      :desc "New empty buffer"         :nv "n" #'evil-buffer-new
      :desc "Pop scratch buffer"       :nv "s" #'+doom:pop-scratch-buffer
      ;; TODO spacemacs/copy-whole-buffer-to-clipboard y
      ;; TODO spacemacs/paste-whole-file-to-clipboard p
      )

    (:desc "ivy"
      :prefix "i"
      :desc "Ivy resume"               :nv "r" #'ivy-resume)

    (:desc "project"
      :prefix "p"
      :desc "Find file in project"     :nv "f" #'counsel-projectile-find-file
      :desc "Run cmd in project root"  :nv "!" #'projectile-run-shell-command-in-root
      :desc "Toggle project neotree"   :nv "t" #'+evil/neotree
      :desc "Switch project"           :nv "p" #'counsel-projectile-switch-project
      :desc "Switch project workspace" :nv "l" #'+workspace-switch-project
      :desc "Kill project buffers"     :nv "k" #'projectile-kill-buffers
      :desc "Recent project files"     :nv "r" #'projectile-recentf
      :desc "Invalidate cache"         :nv "I" #'projectile-invalidate-cache
      :desc "List project tasks"       :nv "a" #'+ivy/tasks
      :desc "Pop term in project root" :nv "'" #'+term/popup-in-project)

    (:desc "search"
      :prefix "s"
      :desc "IEdit mode"              :nv "e" #'evil-iedit-state/iedit-mode
      :desc "Swiper search"           :nv "s" #'swiper)

    (:desc "workspace"
      :prefix "l"
      :desc "Switch to workspace"    :nv "l" #'persp-switch
      :desc "Save workspace state"   :nv "s" #'persp-save-state-to-file
      :desc "Switch last workspace"  :nv "TAB" #'+workspace-switch-last
      :desc "Kill workspace"         :nv "d" #'persp-kill)

    (:desc "window"
      :prefix "w"
      :desc "Split window vertical"   :nv "/" #'split-window-right
      :desc "Split window horizontal" :nv "-" #'split-window-below
      :desc "Balance windows"         :nv "=" #'balance-windows
      :desc "Ace delete window"       :nv "d" #'ace-delete-window
      :desc "Delete current window"   :nv "D" #'delete-window
      :desc "Window left"             :nv "h" #'evil-window-left
      :desc "Window down"             :nv "j" #'evil-window-down
      :desc "Window right"            :nv "l" #'evil-window-right
      :desc "Window up"               :nv "k" #'evil-window-up
      :desc "Move window left"        :nv "H" #'evil-window-move-far-left
      :desc "Move window down"        :nv "J" #'evil-window-move-very-bottom
      :desc "Move window right"       :nv "L" #'evil-window-move-far-right
      :desc "Move window up"          :nv "K" #'evil-window-move-very-top
      :desc "Winner undo"             :nv "u" #'winner-undo
      :desc "Winner redo"             :nv "U" #'winner-redo
      :desc "Ace window"              :nv "w" #'ace-window
      :desc "Toggle maximize window"  :nv "m" #'doom/window-zoom
      :desc "Refresh line numbers"    :nv "r" #'+gdoom/refresh-line-numbers)

    (:desc "jump"
      :prefix "j"
      :desc "Dumb jump go"            :nv "q" #'dumb-jump-go
      :desc "Dump jump go other"      :nv "Q" #'dumb-jump-go-other-window
      :desc "Avy go to line"          :nv "l" #'avy-goto-line
      :desc "Avy go to word"          :nv "w" #'evil-avy-goto-word-or-subword-1
      :desc "Avy go to char"          :nv "j" #'avy-goto-char-2)

    (:desc "git"
      :prefix "g"
      :desc "Git status"              :nv "s" #'magit-status
      :desc "Git blame"               :nv "b" #'magit-blame
      :desc "Git time machine"        :nv "t" #'git-timemachine
      :desc "Git log file"            :nv "l" #'magit-log-buffer-file
      (:desc "gist"
        :prefix "g"
        :desc "Gist from buffer"         :nv "b" #'gist-buffer
        :desc "Private gist from buffer" :nv "B" #'gist-buffer-private
        :desc "Gist from region"         :nv "r" #'gist-region
        :desc "Private gist from region" :nv "R" #'gist-region-private
        :desc "List gists"               :nv "l" #'gist-list))

   (:desc "quit"
     :prefix "q"
     :desc "Quit"                     :nv "q" #'evil-save-and-quit
     :desc "Quit (forget session)"    :nv "Q" #'+workspace/kill-session-and-quit)

   (:desc "toggle"
     :prefix "t"
     :desc "Text size increase"       :nv "+" #'text-scale-increase
     :desc "Text size decrease"       :nv "+" #'text-scale-decrease
     :desc "Spell check"              :nv "s" #'flyspell-mode
     :desc "Line numbers"             :nv "l" #'doom/toggle-line-numbers
     :desc "Indent guides"            :nv "i" #'highlight-indentation-mode
     :desc "Indent guides (column)"   :nv "I" #'highlight-indentation-current-column-mode)
   (:desc "code"
     :prefix "c"
     :desc "Build"                    :nv  "b" #'+eval/build
     :desc "Open/Send to REPL"        :nv "r" #'+eval/repl
     :desc "Open debugger"            :nv  "R" #'+debug/open))


 ;;; Evil-esque bindings
 ;;; indent on new line
 :i "RET" #'evil-ret-and-indent
 ;; Yank to EOL
 :n  "Y"  "y$"
 ;; Repeat in visual mode (buggy)
 :v  "."  #'evil-repeat
 :v  "<"  #'+evil/visual-dedent     ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent     ; vnoremap > >gv
 ;; undo/redo for regions (buggy)
 :nv "u"   #'undo-tree-undo
 :nv "C-r" #'undo-tree-redo
 ;; without yank commands
 :n  "c"  #'evil-change-without-register
 :n  "C"  #'evil-change-line-without-register
 :n  "p"  #'evil-paste-after-without-register
 :n  "P"  #'evil-paste-before-without-register
 :n  "x"  #'evil-delete-char-without-register
 :n  "X"  #'evil-delete-backward-char-without-register
 :n  "d"  #'evil-delete-without-register-if-whitespace
 ;; simple motions
 :nv "H"  #'evil-first-non-blank
 :nv "L"  #'evil-last-non-blank
 ;; search avy goto
 :nv "C-f"  #'avy-goto-char-timer
 ;; evil commentary
 :nv "gc" #'evil-commentary

 (:map evil-window-map ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-zoom
   ;; Delete window
   "C-C"     #'ace-delete-window)

 ;;; Plugins
 ;; company-mode (+ vim-like omnicompletion)
 :i "C-SPC" #'+company/complete
 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "C-]"   #'company-tags
   :i "s"     #'company-ispell
   :i "C-s"   #'company-yasnippet
   :i "C-o"   #'company-capf
   :i "C-n"   #'company-dabbrev-code
   :i "C-p"   (λ! (let ((company-selection-wrap-around t))
                    (call-interactively 'company-dabbrev-code)
                    (company-select-previous-or-abort))))

 ;; help-mode
 (:map help-mode-map
   :n "]]"  #'help-go-forward
   :n "[["  #'help-go-back
   :n "o"   #'ace-link-help)


 ;; --- Plugin bindings ------------------------------
 ;; auto-yasnippet
 :i  [C-tab] #'aya-expand
 :nv [C-tab] #'aya-create

 ;; company-mode (+ vim-like omnicompletion)
 :i "C-SPC"  #'+company/complete

(:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"        nil
     "C-o"        #'company-search-kill-others
     "C-n"        #'company-select-next
     "C-p"        #'company-select-previous
     "C-j"        #'company-select-next
     "C-k"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     [enter]      #'company-complete-common
     "C-h"        #'company-quickhelp-manual-begin
     [tab]        #'company-complete-common-or-cycle
     [backtab]    #'company-select-previous
     [escape]     (λ! (company-abort) (evil-normal-state 1)))
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-j"        #'company-search-repeat-forward
     "C-k"        #'company-search-repeat-backward
     "C-s"        (λ! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))

 ;; counsel
 (:after counsel
   :map counsel-ag-map
   [backtab] #'+ivy/wgrep-occur  ; search/replace on results
   "C-SPC"   #'counsel-git-grep-recenter   ; preview
   "M-RET"   (+ivy-do-action! #'+ivy-git-grep-other-window-action))

 ;; evil-commentary
 :n  "gc" #'evil-commentary

 ;; evil-exchange
 :n  "gx" #'evil-exchange

 ;; evil-snipe
 (:after evil-snipe
   ;; Binding to switch to evil-easymotion/avy after a snipe
   :map evil-snipe-parent-transient-map
   "C-;" (λ! (require 'evil-easymotion)
             (call-interactively +evil--snipe-repeat-fn)))

 ;; evil-surround
 :v  "s"  #'evil-surround-region
 :o  "s"  #'evil-surround-edit
 :o  "S"  #'evil-Surround-edit

 ;; expand-region
 :v  "v"  #'er/expand-region
 :v  "V"  #'er/contract-region

 ;; flycheck
 :m  "]e" #'next-error
 :m  "[e" #'previous-error
 (:after flycheck
   :map flycheck-error-list-mode-map
   :n "C-n" #'flycheck-error-list-next-error
   :n "C-p" #'flycheck-error-list-previous-error
   :n "j"   #'flycheck-error-list-next-error

   :n "RET" #'flycheck-error-list-goto-error)

 ;; flyspell
 :m  "]S" #'flyspell-correct-word-generic
 :m  "[S" #'flyspell-correct-previous-word-generic

 ;; git-gutter
 :m  "]d" #'git-gutter:next-hunk
 :m  "[d" #'git-gutter:previous-hunk

 ;; git-timemachine
 (:after git-timemachine
   (:map git-timemachine-mode-map
     :nv "p" #'git-timemachine-show-previous-revision
     :nv "n" #'git-timemachine-show-next-revision
     :nv "g" #'git-timemachine-show-nth-revision
     :nv "q" #'git-timemachine-quit
     :nv "w" #'git-timemachine-kill-abbreviated-revision
     :nv "W" #'git-timemachine-kill-revision
     :nv "b" #'git-timemachine-blame))

 ;; gist
 (:after gist
   :map gist-list-menu-mode-map
   :n "RET" #'+gist/open-current
   :n "b"   #'gist-browse-current-url
   :n "c"   #'gist-add-buffer
   :n "d"   #'gist-kill-current
   :n "f"   #'gist-fork
   :n "q"   #'quit-window
   :n "r"   #'gist-list-reload
   :n "s"   #'gist-star
   :n "S"   #'gist-unstar
   :n "y"   #'gist-print-current-url)

 ;; hl-todo
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape] #'keyboard-escape-quit
   "C-y" #'yank
   "M-v" #'yank
   "M-z" #'undo
   "C-r" #'evil-paste-from-register
   "C-h" (kbd "DEL")
   "C-k" #'ivy-previous-line
   "C-j" #'ivy-next-line
   "C-l" #'ivy-alt-done
   "C-w" #'+ivy/backward-kill-word
   "C-u" #'doom/minibuffer-kill-line
   "C-b" #'backward-word
   "C-f" #'forward-word
   "C-o" #'ivy-dispatching-done
   "<C-return>" #'ivy-immediate-done)

(:map ivy-occur-grep-mode-map
  (:desc "ivy occur actions"
    :prefix ","
    :desc "switch to ivy wgrep mode"    :n "," 'ivy-wgrep-change-to-wgrep-mode
    :desc "switch to ivy wgrep mode"    :n "w" 'ivy-wgrep-change-to-wgrep-mode))

 ;; neotree
 (:after neotree
   :map neotree-mode-map
   :n "g"         nil
   :n [tab]       #'neotree-quick-look
   :n "RET"       #'neotree-enter
   :n [backspace] #'evil-window-prev
   :n "j"         #'neotree-next-line
   :n "k"         #'neotree-previous-line
   :n "n"         #'neotree-next-line
   :n "p"         #'neotree-previous-line
   :n "h"         #'+evil/neotree-collapse-or-up
   :n "l"         #'+evil/neotree-expand-or-open
   :n "J"         #'neotree-select-next-sibling-node
   :n "K"         #'neotree-select-previous-sibling-node
   :n "H"         #'neotree-select-up-node
   :n "L"         #'neotree-select-down-node
   :n "G"         #'evil-goto-line
   :n "gg"        #'evil-goto-first-line
   :n "v"         #'neotree-enter-vertical-split
   :n "s"         #'neotree-enter-horizontal-split
   :n "q"         #'neotree-hide
   :n "R"         #'neotree-refresh)

 ;; realgud
 (:after realgud
   :map realgud:shortkey-mode-map
   :n "j" #'evil-next-line
   :n "k" #'evil-previous-line
   :n "h" #'evil-backward-char
   :n "l" #'evil-forward-char
   :m "n" #'realgud:cmd-next
   :m "b" #'realgud:cmd-break
   :m "B" #'realgud:cmd-clear
   :n "c" #'realgud:cmd-continue)

 ;; rotate-text
 :n  "!"  #'rotate-text

 ;; smart-forward
 :nv "K"  #'smart-up
 :m  "g]" #'smart-forward
 :m  "g[" #'smart-backward

 ;; undo-tree -- undo/redo for visual regions
 :v "C-u" #'undo-tree-undo
 :v "C-r" #'undo-tree-redo

 ;; yasnippet
 (:after yasnippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [escape]        #'evil-normal-state
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     :i "M-SPC" yas-maybe-expand)))


;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:map input-decode-map
        [?\C-i] [C-i]
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      :niv "<C-i>" #'evil-jump-forward

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i [C-return]     #'evil-open-below
      :i [S-C-return]   #'evil-open-above
      ;; textmate-esque deletion
      [C-backspace]     #'doom/backward-kill-to-bol-and-indent
      :i [backspace]    #'delete-backward-char
      :i [C-backspace]  #'doom/backward-kill-to-bol-and-indent
      ;; Emacsien motions for insert mode
      :i "C-b" #'backward-word
      :i "C-f" #'forward-word

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i "SPC"                          #'doom/inflate-space-maybe
      :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      :i [remap newline]                #'doom/newline-and-indent

      (:after org-mode
        (:map org-mode-map
          :i [remap doom/inflate-space-maybe] #'org-self-insert-command))

      ;; Make ESC quit all the things
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register)

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:map tabulated-list-mode-map
        [remap evil-record-macro] #'doom/popup-close-maybe)

      (:map (evil-ex-completion-map evil-ex-search-keymap read-expression-map)
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom/minibuffer-undo)

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))
