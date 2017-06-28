;;; init.el

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature evil
       :completion company
       :tools password-store
       :private hlissner)
