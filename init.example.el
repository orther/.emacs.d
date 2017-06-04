;;; init.el

(require 'core (concat user-emacs-directory "core/core"))

(doom! :core
       gdoom           ; custom doom core changes

       :feature
       evil            ; come to the dark side, we have cookies
       jump            ; helping you navigate your code base
       snippets        ; my elves. They type so I don't have to
       file-templates  ; auto-snippets for empty files
       spellcheck      ; tasing you for misspelling mispelling
       syntax-checker  ; tasing you for every forgotten semicolon
       version-control ; remember, remember that commit in November
       workspaces      ; tab emulation, persistence and separate workspaces
       eval            ; repls, runners 'n builders; run code, run
       debug           ; stepping through code, to help you add bugs
       search          ; search specific configurations
       editor          ; non-core editor changes (primarily indentation)


       :completion
       company         ; auto-completion backend
       ivy             ; a search engine for love and life

       :ui
       doom            ; doom-one; a look inspired by Atom's Dark One
       doom-dashboard  ; a nifty splash screen for Emacs
       doom-modeline   ; a snazzy Atom-inspired mode-line
       doom-quit       ; DOOM quit-message prompts when you quit Emacs
       hl-todo         ; highlight TODO/FIXME/NOTE tags
       nav-flash       ; blink the current line after jumping
       evil-goggles    ; display visual hints when editing in evil

       :tools
       dired           ; making dired pretty [functional]
       gist            ; interacting with github gists
       term            ; terminals in Emacs
       eshell          ; a consistent, cross-platform shell (WIP)
       neotree         ; a project drawer, like NERDTree for vim

       :app
       present         ; showing off presentations in emacs

       :lang
       assembly        ; assembly for fun or debugging
       cc              ; C/C++/Obj-C madness
       clojure         ; lisp, but java
       csharp          ; unity, .NET, and mono shenanigans
       data            ; config/data formats
       elm             ; care for a cup of TEA?
       emacs-lisp      ; drown in parentheses
       go              ; the hipster dialect
       haskell         ; a language that's lazier than I am
       java            ; the poster child for carpal tunnel syndrome
       javascript      ; all(hope(abandon(ye(who(enter(here))))))
       julia           ; a better, faster MATLAB
       latex           ; writing papers in Emacs has never been so fun
       lua             ; one-based indices? one-based indices
       markdown        ; writing docs for people to ignore
       org             ; for organized fearless leader (WIP)
       php             ; make php less awful to work with
       purescript      ; javascript, but functional
       python          ; beautiful is better than ugly
       rest            ; Emacs as a REST client
       ruby            ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust            ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala           ; java, but good
       sh              ; she sells (ba|z)sh shells on the C xor
       swift           ; who asked for emoji variables?
       typescript      ; javascript, but better
       web             ; the tubes

       :framework
       play            ; Support for the play framework (specifically routes file)

       :private gilbertw1)
