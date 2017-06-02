;;; private/gilbertw1/config.el

(load! +bindings)  ; my key bindings

(defvar +gilbertw1-dir
  (file-name-directory load-file-name))

;; projectile ignore directories
(setq projectile-globally-ignored-directories '("target" ".ensime_cache" ".fingerprint" "project/target"))
(setq grep-find-ignored-directories '("target" ".ensime_cache" ".fingerprint"))

;; customize doom neotree
(setq doom-neotree-file-icons t)

;; Widen fringes (easier to see git-gutter with twm window border)
(fringe-mode '(12 . 12))

;; Override vc modified color (gray -> yellow)
(custom-set-faces
  '(diff-hl-change         ((t (:foreground "#ECBE7B"))))
  '(git-gutter:modified    ((t (:foreground "#ECBE7B"))))
  '(git-gutter+-modified   ((t (:foreground "#ECBE7B"))))
  '(git-gutter-fr:modified ((t (:foreground "#ECBE7B")))))

;; Override smerge colors
(custom-set-faces
  '(smerge-refined-removed ((t (:inherit 'smerge-mine))))
  '(smerge-refined-added   ((t (:inherit 'smerge-other)))))
