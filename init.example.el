;;; init.el

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature
       evil
       jump
       syntax-checker
       version-control
       workspaces
       eval
       debug
       search

       :completion
       company
       ivy

       :ui
       doom
       doom-dashboard
       doom-modeline
       doom-quit
       hl-todo
      
       :tools
       eshell
       dired
       restclient

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
