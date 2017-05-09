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

(map!
 ;; Essential
 "M-x"    'execute-extended-command
 "A-x"    'execute-extended-command
 "M-;"    'eval-expression
 "A-;"    'eval-expression
 ;; Tools
 [f9]     'doom/what-face
 [f10]    'doom/blink-cursor
 "C-`"    'doom/popup-toggle
 ;; Text-scaling
 "M-0"    (λ! (text-scale-set 0))
 "M-="    'text-scale-increase
 "M--"    'text-scale-decrease
 ;; Simple window navigation/manipulation
 ;"M-t"    '+workspace/new
 ;"M-T"    '+workspace/display
 ;"M-w"    'delete-window
 ;"M-W"    'delete-frame
 ;"M-n"    'evil-buffer-new
 ;"M-N"    'make-frame
 ;"C-j"    'evil-window-down
 ;"C-k"    'evil-window-up
 ;"C-h"    'evil-window-left
 ;"C-l"    'evil-window-right
 ;; Basic escape keys for emacs mode
; :e "C-h" 'evil-window-left
; :e "C-j" 'evil-window-down
; :e "C-k" 'evil-window-up
; :e "C-l" 'evil-window-right
 ;; Temporary escape into emacs mode
 :e [C-escape] 'evil-normal-state
 :n [C-escape] 'evil-emacs-state
 "M-0"  '+workspace/switch-to-last ;; TODO: THIS!
 :e "M-w" 'kill-ring-save

 "M-r"  '+eval/buffer
 "M-b"  '+eval/build

 [M-backspace]  'doom/backward-kill-to-bol-and-indent
 "M-a"          'mark-whole-buffer
 "M-c"          'evil-yank
 "M-q"          'save-buffers-kill-emacs
 "M-s"          'save-buffer
 "M-v"          'clipboard-yank
 "C-M-f"        'doom/toggle-fullscreen
 :m "A-j"       '+hlissner:multi-next-line
 :m "A-k"       '+hlissner:multi-previous-line
 :nvi "TAB"       'indent-for-tab-command

  ;;; <leader> and <localleader>
  :m ";" 'evil-ex
  (:leader
    ;; common
    :desc "Counsel M-x"                :nv ":" 'counsel-M-x
    :desc "Run shell command"          :nv "!" 'shell-command
    :desc "Find file in project"       :nv "/" 'counsel-projectile-rg
    :desc "Find file from here"        :nv "?" 'counsel-rg
    :desc "Find selection in project"  :nv "*" 'counsel-projectile-rg-region-or-symbol
    :desc "Switch to Emacs mode"       :n "|" 'evil-emacs-state
    :desc "Switch to last buffer"      :nv "TAB" 'evil-switch-to-windows-last-buffer
    :desc "Ace window"                 :nv "W" 'ace-window

    (:desc "file"
      :prefix "f"
      :desc "Open file from here"      :nv "f" 'counsel-find-file
      :desc "Sudo open file from here" :nv "F" 'doom/sudo-find-file
      :desc "Copy file"                :nv "c" 'copy-file
      :desc "Copy current file"        :nv "C" '+gdoom/copy-file
      :desc "Rename current file"      :nv "R" '+gdoom/rename-current-buffer-file
      :desc "Delete file"              :nv "d" '+gdoom/delete-file-confirm
      :desc "Delete current file"      :nv "D" '+gdoom/delete-current-buffer-file
      :desc "Sudo edit file"           :nv "E" '+gdoom/sudo-edit
      ;; TODO: spacemacs/sudo-edit
      :desc "Recent files"             :nv "r"  'counsel-recentf
      :desc "Neotree toggle"           :nv "t"  'neotree-toggle ;; TODO: neesd work
      :desc "Save buffer"              :nv "s"  'save-buffer
      :desc "Save all buffers"         :nv "S"  'evil-write-all
      :desc "Counsel bookmark"         :nv "b"  'counsel-bookmark
      :desc "Remote ssh tramp"         :nv "i"  (lambda ()
                                                 (interactive)
                                                 (counsel-find-file "/ssh:"))
      :desc "Show and copy filename"   :nv "y" '+gdoom/show-and-copy-buffer-filename
      )

    (:desc "buffer"
      :prefix "b"
      :desc "Switch workspace buffer"  :nv "b" '+ivy/switch-workspace-buffer
      :desc "Switch buffer"            :nv "B" '+ivy/switch-buffer
      :desc "Kill buffer"              :nv "d" 'kill-this-buffer
      :desc "Kill other buffers"       :nv "D" 'doom/kill-other-buffers
      :desc "Kill a buffer"            :nv "k" 'kill-buffer
      :desc "New empty buffer"         :nv "n" 'evil-buffer-new
      :desc "Pop scratch buffer"       :nv "s" '+doom:pop-scratch-buffer
      ;; TODO spacemacs/copy-whole-buffer-to-clipboard y
      ;; TODO spacemacs/paste-whole-file-to-clipboard p
      )

    (:desc "project"
      :prefix "p"
      :desc "Find file in project"    :nv "f" 'counsel-projectile-find-file
      :desc "Run cmd in project root" :nv "!" 'projectile-run-shell-command-in-root
      :desc "Toggle project neotree"  :nv "t" '+evil/neotree
      :desc "Switch project"          :nv "p" 'counsel-projectile-switch-project
      :desc "Kill project buffers"    :nv "k" 'projectile-kill-buffers
      :desc "Recent project files"    :nv "r" 'projectile-recentf
      :desc "Invalidate cache"        :nv "I" 'projectile-invalidate-cache
      ;; TODO spacemacs/project-shell-pop '
      )

    (:desc "search"
      :prefix "s"
      :desc "IEdit mode"              :nv "e" 'evil-iedit-state/iedit-mode
      :desc "Swiper search"           :nv "s" 'swiper)

    (:desc "workspace"
      :prefix "l"
      :desc "Switch to workspace"    :nv "l" 'persp-switch
      :desc "Save workspace state"   :nv "s" 'persp-save-state-to-file
      :desc "Switch last workspace"  :nv "TAB" '+workspace-switch-last
      :desc "Kill workspace"         :nv "d" 'persp-kill)

    (:desc "window"
      :prefix "w"
      :desc "Split window vertical"   :nv "/" 'split-window-right
      :desc "Split window horizontal" :nv "-" 'split-window-below
      :desc "Balance windows"         :nv "=" 'balance-windows
      :desc "Delete window"           :nv "d" 'delete-window
      :desc "Window left"             :nv "h" 'evil-window-left
      :desc "Window down"             :nv "j" 'evil-window-down
      :desc "Window right"            :nv "l" 'evil-window-right
      :desc "Window up"               :nv "k" 'evil-window-up
      :desc "Move window left"        :nv "H" 'evil-window-move-far-left
      :desc "Move window down"        :nv "J" 'evil-window-move-very-bottom
      :desc "Move window right"       :nv "L" 'evil-window-move-far-right
      :desc "Move window up"          :nv "K" 'evil-window-move-very-top
      :desc "Winner undo"             :nv "u" 'winner-undo
      :desc "Winner redo"             :nv "U" 'winner-redo
      :desc "Ace window"              :nv "w" 'ace-window
      :desc "Toggle maximize window"  :nv "m" 'doom/window-zoom)

    (:desc "jump"
      :prefix "j"
      :desc "Dumb jump go"            :nv "q" 'dumb-jump-go
      :desc "Dump jump go other"      :nv "Q" 'dumb-jump-go-other-window
      :desc "Avy go to line"          :nv "l" 'avy-goto-line
      :desc "Avy go to word"          :nv "w" 'avy-goto-word-0
      :desc "Avy go to char"          :nv "j" 'evil-avy-goto-word-or-subword-1)

    (:desc "git"
      :prefix "g"
      :desc "Git status"              :nv "s" 'magit-status
      :desc "Git blame"               :nv "b" 'magit-blame
      :desc "Git time machine"        :nv "t" 'git-timemachine
      (:desc "gist"
        :prefix "g"
        :desc "Gist from buffer"         :nv "b" 'gist-buffer
        :desc "Private gist from buffer" :nv "B" 'gist-buffer-private
        :desc "Gist from region"         :nv "r" 'gist-region
        :desc "Private gist from region" :nv "R" 'gist-region-private
        :desc "List gists"               :nv "l" 'gist-list))

   (:desc "quit"
     :prefix "q"
     :desc "Quit"                     :nv "q" 'evil-save-and-quit
     :desc "Quit (forget session)"    :nv "Q" '+workspace/kill-session-and-quit)

   (:desc "toggle"
     :prefix "t"
     :desc "Text size increase"       :nv "+" 'text-scale-increase
     :desc "Text size decrease"       :nv "+" 'text-scale-decrease
     :desc "Spell check"              :nv "s" 'flyspell-mode
     :desc "Line numbers"             :nv "l" 'doom/toggle-line-numbers
     :desc "Indent guides"            :nv "i" 'highlight-indentation-mode
     :desc "Indent guides (column)"   :nv "I" 'highlight-indentation-current-column-mode)
   (:desc "code"
     :prefix "c"
     :desc "Build"                    :nv  "b" '+eval/build
     :desc "Open/Send to REPL"        :nv "r" '+eval/repl
     :desc "Open debugger"            :nv  "R" '+debug/open))


 ;;; Evil-esque bindings
 ;;; indent on new line
 :i "RET" 'evil-ret-and-indent
 ;; Yank to EOL
 :n  "Y"  "y$"
 ;; Repeat in visual mode (buggy)
 :v  "."  'evil-repeat
 :v  "<"  '+evil/visual-dedent     ; vnoremap < <gv
 :v  ">"  '+evil/visual-indent     ; vnoremap > >gv
 ;; undo/redo for regions (buggy)
 :nv "u"   'undo-tree-undo
 :nv "C-r" 'undo-tree-redo
 ;; without yank commands
 :n  "c"  'evil-change-without-register
 :n  "C"  'evil-change-line-without-register
 :n  "p"  'evil-paste-after-without-register
 :n  "P"  'evil-paste-before-without-register
 :n  "x"  'evil-delete-char-without-register
 :n  "X"  'evil-delete-backward-char-without-register
 :n  "d"  'evil-delete-without-register-if-whitespace
 ;; simple motions
 :nv "H"  'evil-first-non-blank
 :nv "L"  'evil-last-non-blank
 ;; search avy goto
 :nv "C-f"  'evil-avy-goto-word-or-subword-1
 :nv "/" 'swiper

 (:map evil-window-map ; prefix "C-w"
   ;; Navigation
   "C-h"     'evil-window-left
   "C-j"     'evil-window-down
   "C-k"     'evil-window-up
   "C-l"     'evil-window-right
   "C-w"     'ace-window
   ;; Swapping windows
   "H"       '+evil/window-move-left
   "J"       '+evil/window-move-down
   "K"       '+evil/window-move-up
   "L"       '+evil/window-move-right
   "C-S-w"   'ace-swap-window
   ;; Window undo/redo
   "u"       'winner-undo
   "C-u"     'winner-undo
   "C-r"     'winner-redo
   "o"       'doom/window-zoom
   ;; Delete window
   "c"       '+workspace/close-window-or-workspace
   "C-C"     'ace-delete-window)

 ;;; Plugins
 ;; company-mode (+ vim-like omnicompletion)
 :i "C-SPC" '+company/complete
 (:prefix "C-x"
   :i "C-l"   '+company/whole-lines
   :i "C-k"   '+company/dict-or-keywords
   :i "C-f"   'company-files
   :i "C-]"   'company-tags
   :i "s"     'company-ispell
   :i "C-s"   'company-yasnippet
   :i "C-o"   'company-capf
   :i "C-n"   'company-dabbrev-code
   :i "C-p"   (λ! (let ((company-selection-wrap-around t))
                    (call-interactively 'company-dabbrev-code)
                    (company-select-previous-or-abort))))
 ;; evil-visual-star
 :v  "*"   'evil-visualstar/begin-search-forward
 :v  "#"   'evil-visualstar/begin-search-backward
 ;; evil-multiedit
 :v  "R"     'evil-multiedit-match-all
 :n  "M-C-D" 'evil-multiedit-restore
 :n  "M-d"   'evil-multiedit-match-symbol-and-next
 :n  "M-D"   'evil-multiedit-match-symbol-and-prev
 :v  "M-d"   'evil-multiedit-match-and-next
 :v  "M-D"   'evil-multiedit-match-and-prev
 ;; evil-surround
 :v  "s"   'evil-surround-region
 :o  "S"   'evil-surround-edit
 :o  "S"   'evil-Surround-edit
 ;; evil-forward
 :niv "<C-i>" 'evil-jump-forward

 ;; help-mode
 (:map help-mode-map
   :n "]]"  'help-go-forward
   :n "[["  'help-go-back
   :n "o"   'ace-link-help))


;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:unless window-system "TAB" [tab]) ; Fix TAB in terminal

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace.
      :i "C-a" 'doom/backward-to-bol-or-indent
      :i "C-e" 'doom/forward-to-last-non-comment-or-eol
      :i "C-u" 'doom/backward-kill-to-bol-and-indent

      ;; escape from insert mode (more responsive than using key-chord-define)
      :irv "C-g" 'evil-normal-state

      ;; Make ESC quit all the things
      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map)
        [escape] 'abort-recursive-edit
        "C-r" 'evil-paste-from-register)

      (:map messages-buffer-mode-map
        "M-;"  'eval-expression
        "A-;"  'eval-expression)

      (:map (evil-ex-completion-map evil-ex-search-keymap read-expression-map)
        "C-a" 'move-beginning-of-line
        "C-w" 'backward-kill-word
        "C-u" 'backward-kill-sentence
        "C-b" 'backward-word
        "C-f" 'forward-word)

      (:after view
        (:map view-mode-map "<escape>" 'View-quit-all))

      (:after help-mode
        (:map help-map
          ;; Remove slow/annoying help subsections
          "h" nil
          "g" nil)))


