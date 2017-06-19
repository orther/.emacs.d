;;; private/orther/init.el -*- lexical-binding: t; -*-

;; This is a special file, unique to private modules, that is loaded after DOOM
;; core but before any module is activated, giving you an opportunity to
;; overwrite variables or settings before initialization.

(setq user-mail-address "brandon@omt.tech"
      user-full-name    "Brandon Orther"

      +doom-modeline-height 25

      +doom-font (font-spec :family "Iosevka" :size 12)
      +doom-variable-pitch-font (font-spec :family "Iosevka" :size 13)
      +doom-unicode-font (font-spec :family "Iosevka" :size 12))
