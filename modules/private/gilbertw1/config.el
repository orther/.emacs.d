;;; private/gilbertw1/config.el

(when (featurep 'evil)
  (load! +bindings))  ; my key bindings

(defvar +gilbertw1-dir
  (file-name-directory load-file-name))

;; projectile ignore directories
(setq projectile-globally-ignored-directories '("target" ".ensime_cache" ".fingerprint" "project/target"))
(setq grep-find-ignored-directories '("target" ".ensime_cache" ".fingerprint"))

;; Widen fringes (easier to see git-gutter with twm window border)
(fringe-mode '(12 . 12))
