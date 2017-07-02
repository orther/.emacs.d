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

;; Override evil googles colors
(custom-set-faces
 '(evil-goggles-delete-face ((t (:foreground "#ff6c6b"))))
 '(evil-goggles-paste-face  ((t (:foreground "#98be65"))))
 '(evil-goggles-yank-face   ((t (:foreground "#51afef")))))

;; Override org mode colors
(custom-set-faces
 '(org-level-1              ((t :foreground "#51afef" :inherit nil :height 1.2)))
 '(org-level-2              ((t :foreground "#a9a1e1" :inherit nil :height 1.1)))
 '(org-level-3              ((t :foreground "#98be65" :inherit nil :height 1.1)))
 '(org-level-4              ((t :foreground "#da8548" :inherit nil :height 1.1)))
 '(org-level-5              ((t :foreground "#46D9FF" :inherit nil :height 1.1))))

;; Close magit buffer after following file
(defun close-magit-buffer ()
  (when (and (boundp 'magit-mode) magit-mode)
    (magit-mode-bury-buffer)))

(add-hook 'magit-diff-visit-file-hook #'close-magit-buffer)

