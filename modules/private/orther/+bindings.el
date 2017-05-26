;;; private/orther/+bindings.el

;; I've swapped these keys on my keyboard
;; (setq x-super-keysym 'alt
;;      x-alt-keysym   'meta)

(defmacro find-file-in! (path &optional project-p)
  "Returns an interactive function for searching files."
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(command-remapping
           (if project-p
               #'projectile-find-file
             #'find-file))))))

(setq doom-leader-key "SPC")
(setq doom-localleader-key "SPC m")

(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag)

(map!
 ;; Make M-x available everywhere
 :nvime "M-x" #'execute-extended-command
 :nvime "A-x" #'execute-extended-command
 :nvime "M-:" #'+orther/C-u-M-x
 :nvime "A-:" #'+orther/C-u-M-x
 ;; Emacs debug utilities
 "M-;"    #'eval-expression
 "A-;"    #'eval-expression
 [f9]     #'doom/what-face
 [f10]    #'doom/blink-cursor
 "C-`"    #'doom/popup-toggle
 ;; Text-scaling
 "M-+"    (λ! (text-scale-set 0))
 "M-="    #'text-scale-increase
 "M--"    #'text-scale-decrease
 ;; Simple window navigation/manipulation
 "M-t"    #'+workspace/new
 "M-T"    #'+workspace/display
 "M-w"    #'delete-window
 "M-W"    #'delete-frame
 "M-n"    #'evil-buffer-new
 "M-N"    #'make-frame
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-h"    #'evil-window-left
 "C-l"    #'evil-window-right
 "M-1"    (λ! (+workspace/switch-to 0))
 "M-2"    (λ! (+workspace/switch-to 1))
 "M-3"    (λ! (+workspace/switch-to 2))
 "M-4"    (λ! (+workspace/switch-to 3))
 "M-5"    (λ! (+workspace/switch-to 4))
 "M-6"    (λ! (+workspace/switch-to 5))
 "M-7"    (λ! (+workspace/switch-to 6))
 "M-8"    (λ! (+workspace/switch-to 7))
 "M-9"    (λ! (+workspace/switch-to 8))
 "M-0"    #'+workspace/switch-to-last
 ;; Basic escape keys for emacs mode
 "C-h"    #'evil-window-left
 "C-j"    #'evil-window-down
 "C-k"    #'evil-window-up
 "C-l"    #'evil-window-right

 "M-r"    #'+eval/buffer
 "M-S-r"  #'+eval/region-and-replace
 "M-b"    #'+eval/build
 "M-a"    #'mark-whole-buffer
 "M-c"    #'evil-yank
 "M-q"    #'save-buffers-kill-emacs
 "M-s"    #'save-buffer
 "M-v"    #'clipboard-yank
 "M-f"    #'+ivy:swiper
 "C-M-f"  #'doom/toggle-fullscreen
 :m "A-j" #'+orther:multi-next-line
 :m "A-k" #'+orther:multi-previous-line

 ;;; <leader> and <localleader>
 :m ";" 'evil-ex
 (:leader
   ;; Most commonly used
   :desc "Previous buffer"       :n "TAB" #'doom/previous-buffer
   :desc "Browse files"          :n "."  #'find-file
   :desc "Find file in project"  :n "/"  #'projectile-find-file
   :desc "Find in file (swiper)" :n "?"  #'swiper
   :desc "Imenu"                 :n ";"  #'imenu
   :desc "Imenu across buffers"  :n ":"  #'imenu-anywhere
   :desc "Find other file"       :n "a"  #'projectile-find-other-file
   ;; :desc "Jump to bookmark"      :n "b"  #'bookmark-jump
   ;; :desc "Delete bookmark"       :n "B"  #'bookmark-delete
   :desc "List errors"           :n "e"  #'flycheck-list-errors
   :desc "View Emacs Log"        :n "M"  #'doom/popup-toggle-messages
   :desc "Recent files"          :n "r"  #'recentf
   :desc "Recent project files"  :n "R"  #'projectile-recentf
   :desc "Insert from kill ring" :n "y"  #'counsel-yank-pop
   ;; :desc "Switch project"        :n "p"  #'projectile-switch-project
   ;; :desc "Open Neotree"          :n "\\" #'+evil/neotree

   ;; Since I've remapped C-h...
   :desc "help"                  :n "h"  #'help-command

   (:desc "file"
     :prefix "f"
     :desc "Open file from here"      :nv "f" #'counsel-find-file
     :desc "Sudo open file from here" :nv "F" #'doom/sudo-find-file
     :desc "Copy file"                :nv "c" #'copy-file
     ;; :desc "Copy current file"        :nv "C" #'+gdoom/copy-file
     ;; :desc "Rename current file"      :nv "R" #'+gdoom/rename-current-buffer-file
     ;; :desc "Delete file"              :nv "d" #'+gdoom/delete-file-confirm
     ;; :desc "Delete current file"      :nv "D" #'+gdoom/delete-current-buffer-file
     ;; :desc "Sudo edit file"           :nv "E" #'+gdoom/sudo-edit
     :desc "Recent files"             :nv "r"  #'counsel-recentf
     :desc "Neotree toggle"           :nv "t"  #'+evil/neotree
     :desc "Save buffer"              :nv "s"  #'save-buffer
     :desc "Save all buffers"         :nv "S"  #'evil-write-all
     :desc "Counsel bookmark"         :nv "b"  #'counsel-bookmark
     :desc "Remote ssh tramp"         :nv "i"  (lambda ()
                                                (interactive)
                                                (counsel-find-file "/ssh:"))
     ;; :desc "Show and copy filename"   :nv "y" #'+gdoom/show-and-copy-buffer-filename
     )

   (:desc "buffer"
     :prefix "b"
     :desc "Switch workspace buffer"  :nv "b" #'persp-switch-to-buffer
     :desc "Switch buffer"            :nv "B" #'switch-buffer
     :desc "Kill buffer"              :nv "d" #'kill-this-buffer
     :desc "Kill other buffers"       :nv "D" #'doom/kill-other-buffers
     :desc "Kill a buffer"            :nv "k" #'kill-buffer
     :desc "New empty buffer"         :nv "n" #'evil-buffer-new
     :desc "Pop scratch buffer"       :nv "s" #'+doom:pop-scratch-buffer
     ;; TODO spacemacs/copy-whole-buffer-to-clipboard y
     ;; TODO spacemacs/paste-whole-file-to-clipboard p
     )

   ;; (:desc "ivy"
   ;;   :prefix "i"
   ;;   :desc "Ivy resumt"               :nv "r" #'ivy-resume)

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
     :desc "Ace delete window"       :nv "D" #'ace-delete-window
     :desc "Delete current window"   :nv "d" #'delete-window
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
     ;; :desc "Refresh line numbers"    :nv "r" #'+gdoom/refresh-line-numbers
     )

   (:desc "jump"
     :prefix "j"
     :desc "Dumb jump go"            :nv "q" #'dumb-jump-go
     :desc "Dump jump go other"      :nv "Q" #'dumb-jump-go-other-window
     :desc "Avy go to line"          :nv "l" #'avy-goto-line
     :desc "Avy go to word"          :nv "w" #'evil-avy-goto-word-or-subword-1
     :desc "Avy go to char"          :nv "j" #'avy-goto-char-2)

   (:desc "quit"
     :prefix "q"
     :desc "Quit"                   :n "q" #'evil-save-and-quit
     :desc "Quit (forget session)"  :n "Q" #'+workspace/kill-session-and-quit)

   (:desc "git"
     :prefix "g"
     :desc "Git status"        :n "s" #'magit-status
     :desc "Git blame"         :n "b" #'magit-blame
     :desc "Git time machine"  :n "t" #'git-timemachine-toggle
     :desc "Git revert hunk"   :n "r" #'git-gutter:revert-hunk
     :desc "List gists"        :n "g" #'+gist:list)

   ;; (:desc "session"
   ;;   :prefix "s"
   ;;   :desc "Kill all buffers"         :n "k" #'doom/kill-all-buffers
   ;;   :desc "Load last session"        :n "r" (λ! (+workspace/load-session))
   ;;   :desc "Load workspace from file" :n "l" #'+workspace/load
   ;;   :desc "Save workspace to file"   :n "s" #'+workspace/save
   ;;   :desc "Switch workspace"         :n "." #'+workspace/switch-to
   ;;   :desc "Delete this workspace"    :n "x" #'+workspace/delete
   ;;   :desc "Load session"             :n "L" #'+workspace/load-session
   ;;   :desc "Autosave current session" :n "S" #'+workspace/save-session
   ;;   :desc "Delete session"           :n "X" #'+workspace/kill-session)

   (:desc "toggle"
     :prefix "t"
     :desc "Spell check"            :n "s" #'flyspell-mode
     :desc "Line numbers"           :n "l" #'doom/toggle-line-numbers
     :desc "Fullscreen"             :n "f" #'doom/toggle-fullscreen
     :desc "Indent guides"          :n "i" #'highlight-indentation-mode
     :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"         :n "h" #'+present/impatient-mode
     :desc "Big mode"               :n "b" #'+present/big-mode)

   (:desc "remote"
     :prefix "u"
     :desc "Upload local"           :n "u" #'+upload/local
     :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
     :desc "Download remote"        :n "d" #'+upload/remote-download
     :desc "Diff local & remote"    :n "D" #'+upload/diff
     :desc "Browse remote files"    :n "." #'+upload/browse
     :desc "Detect remote changes"  :n ">" #'+upload/check-remote)

   (:desc "open"
     :prefix "o"
     :desc "Default browser"         :n  "o" #'browse-url-of-file
     :desc "Debugger"                :n  "d" #'+debug/open
     :desc "Sudo find file"          :n  "s" #'doom/sudo-this-file
     :desc "Sudo edit"               :n  "S" #'doom/sudo-find-file
     :desc "Build tasks"             :n  "b" #'+eval/build
     :desc "Open/Send to REPL"       :nv "r" #'+eval:repl
     :desc "Terminal"                :n  "t" #'+term/popup
     :desc "Terminal @ project root" :n  "T" #'+term/popup-in-project

     ;; applications
     :desc "APP: elfeed"            :n "E"   #'=rss
     :desc "APP: email"             :n "M"   #'=email
     :desc "APP: twitter"           :n "T"   #'=twitter
     :desc "APP: regex"             :n "X"   #'=regex

     ;; macos
     (:when IS-MAC
       :desc "Reveal in Finder"          :n "o" #'+macos/reveal
       :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project
       :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
       :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
       :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
       :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

   (:desc "Personal"
     :prefix "SPC"
     :desc "Browse emacs.d"         :n "."   #'+orther/browse-emacsd
     :desc "Find file in emacs.d"   :n "/"   #'+orther/find-in-emacsd
     :desc "Browse dotfiles"        :n ">"   #'+orther/browse-dotfiles
     :desc "Find file in dotfiles"  :n "?"   #'+orther/find-in-dotfiles
     :desc "Reload theme"           :n "R"   #'+doom/reset-theme
     ;; Org notes
     :desc "Browse notes"           :n "n"   #'+orther/browse-notes
     :desc "Find file in notes"     :n "N"   #'+orther/find-in-notes
     :desc "Browse project notes"   :n "p"   #'+org/browse-notes-for-project
     :desc "Browse mode notes"      :n "m"   #'+org/browse-notes-for-major-mode
     :desc "Org Capture"            :n "SPC" #'+org/capture
     ;; misc
     :desc "Find snippet for mode"  :n "s"  #'yas-visit-snippet-file
     :desc "Find snippet"           :n "S"  #'+orther/find-in-snippets
     ))

 (:localleader
  (:desc "Refactor..." :prefix "r")

  (:desc "Find..."     :prefix "f"
    :desc "Get type..." :prefix "t")

  (:desc "Unit tests"  :prefix "t"))

 ;;; Evil-esque bindings
 ;; Buffers
 :n  "zx" #'doom/kill-this-buffer
 :n  "ZX" #'bury-buffer
 :n  "]b" #'doom/next-buffer
 :n  "[b" #'doom/previous-buffer
 ;; Diffs
 :m  "]d" #'git-gutter:next-hunk
 :m  "[d" #'git-gutter:previous-hunk
 :m  "]e" #'next-error
 :m  "[e" #'previous-error
 ;; Switch tabs
 :n  "]w" #'+workspace/switch-right
 :n  "[w" #'+workspace/switch-left
 :m  "gt" #'+workspace/switch-right
 :m  "gT" #'+workspace/switch-left
 ;; Increment/decrement number under cursor
 :n  "g=" #'evil-numbers/inc-at-pt
 :n  "g-" #'evil-numbers/dec-at-pt
 ;; Todos
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous
 ;; Navigation
 :nv "K"  #'smart-up
 :m  "gd" #'+jump/definition
 :m  "gD" #'+jump/references
 :n  "gp" #'+evil/reselect-paste
 :n  "gc" #'evil-commentary
 :n  "gx" #'evil-exchange
 :n  "gr" #'+eval:region
 :n  "gR" #'+eval/buffer
 :v  "gR" #'+eval:replace-region
 :m  "g]" #'smart-forward
 :m  "g[" #'smart-backward
 :v  "@"  #'+evil:macro-on-all-lines
 :n  "g@" #'+evil:macro-on-all-lines
 ;; repeat in visual mode (buggy)
 :v  "."  #'evil-repeat
 ;; don't leave visual mode after shifting
 :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
 ;; paste from recent yank register (which isn't overwritten)
 :v  "C-p" "\"0p"

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
   "o"       #'doom/window-enlargen
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window)

 ;;; Plugins
 ;; company-mode (+ vim-like omnicompletion)
 :i "C-SPC"  #'+company/complete
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
 ;; evil-mc
 :n "M-d" #'evil-mc-make-cursor-here
 (:after evil-mc
   :map evil-mc-key-map
   :n "M-D" #'+evil/mc-toggle-cursors)
 ;; evil-multiedit
 :v "M-d"   #'evil-multiedit-match-and-next
 :v "M-D"   #'evil-multiedit-match-and-prev
 :v "C-M-d" #'evil-multiedit-restore
 :v "R"     #'evil-multiedit-match-all
 (:after evil-multiedit
   (:map evil-multiedit-state-map
     "M-d" #'evil-multiedit-match-and-next
     "M-D" #'evil-multiedit-match-and-prev
     "RET" #'evil-multiedit-toggle-or-restrict-region)
   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
     "C-n" #'evil-multiedit-next
     "C-p" #'evil-multiedit-prev))
 ;; evil-surround
 :v  "S"   #'evil-surround-region
 :o  "s"   #'evil-surround-edit
 :o  "S"   #'evil-Surround-edit
 ;; expand-region
 :v  "v"   #'er/expand-region
 :v  "V"   #'er/contract-region
 ;; rotate-text
 :n  "!"   #'rotate-text
 ;; hide-show/evil-matchit
 :nv [tab] #'+evil/matchit-or-toggle-fold

 ;; help-mode
 (:map help-mode-map
   :n "q"   #'quit-window
   :n "Q"   #'+ivy-quit-and-resume
   :n "]]"  #'help-go-forward
   :n "[["  #'help-go-back
   :n "o"   #'ace-link-help)

 (:map help-map
   :desc "Describe face" "h"   #'describe-face ; overwrites `view-hello-file'
   :desc "Face at point" "H"   #'doom/what-face
   :desc "Minor-mode"    "M"   #'doom/what-minor-mode
   :desc "Command log"   "W"   #'global-command-log-mode
   :desc "Find library"  "l"   #'find-library
   :desc "View lossage"  "L"   #'view-lossage
   :desc "Describe char" "g"   #'describe-char)) ; overwrites `describe-gnu-project'


;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:unless window-system "TAB" [tab]) ; Fix TAB in terminal
      [S-iso-lefttab] [backtab]           ; Fix TAB in GNU Emacs

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i [M-return]     #'evil-open-below
      :i [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace]     #'doom/backward-kill-to-bol-and-indent
      :i [backspace]    #'delete-backward-char
      :i [M-backspace]  #'doom/backward-kill-to-bol-and-indent
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
        "C-w" #'doom-minibuffer-kill-word
        "C-u" #'doom-minibuffer-kill-line
        "C-b" #'backward-word
        "C-f" #'forward-word
        "M-z" #'doom-minibuffer-undo)

      (:after view
        (:map view-mode-map "<escape>" #'View-quit-all)))

