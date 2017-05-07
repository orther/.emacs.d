;;; init.el

(require 'core (concat user-emacs-directory "core/core"))

(doom! :core
       gdoom

       :feature
       evil
       jump
       syntax-checker
       version-control
       workspaces
       eval
       debug
       search
       editor
       snippet

       :completion
       company
       ivy

       :ui
       doom
       doom-dashboard
       doom-modeline
       doom-quit
       hl-todo
       window
      
       :tools
       dired           ; making dired pretty [functional]
       electric-indent ; smarter, keyword-based electric-indent
       eshell          ; a consistent, cross-platform shell (WIP)
       gist            ; manage & create gists
       macos           ; macos-specific commands
       rotate-text     ; cycle region at point between text candidates
       tmux            ; an API for interacting with tmux
       upload          ; map local to remote projects via ssh/ftp

       :lang
       assembly
       cc
       csharp
       data
       emacs-lisp
       go
       haskell
       java
       javascript
       julia
       latex
       lua
       markdown
       octave
       org
       php
       python
       rest
       ruby
       rust
       scala
       sh
       swift
       typescript
       web

       :framework
       play

       :private gilbertw1)
