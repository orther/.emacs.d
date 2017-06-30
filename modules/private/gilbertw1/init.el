;;; private/gilbertw1/init.el -*- lexical-binding: t; -*-

(setq doom-modeline-height 25
      doom-font (font-spec :family "Iosevka" :size 25)
      doom-variable-pitch-font (font-spec :family "Iosevka" :size 25)
      doom-unicode-font (font-spec :family "Iosevka" :size 24)
      doom-line-number-lpad 3)

;; increasing elisp runtime thresholds
(setq max-lisp-eval-depth 50000)
(setq max-specpdl-size 10000)
