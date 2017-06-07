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

(after! nlinum
  (defun nlinum--region (start limit)
    (save-excursion
      ;; Text may contain those nasty intangible properties, but
      ;; that shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (goto-char start)
        (unless (bolp) (forward-line 1))
        (remove-overlays (point) limit 'nlinum t)
        (let ((line (nlinum--line-number-at-pos)))
          (while
              (and (not (eobp)) (<= (point) limit)
                   (let* ((ol (make-overlay (point) (1+ (point))))
                          (str (funcall nlinum-format-function
                                        line nlinum--width))
                          (width (string-width str)))
                     (when (< nlinum--width width)
                       (setq nlinum--width width)
                       (nlinum--flush))
                     (overlay-put ol 'nlinum t)
                     (overlay-put ol 'evaporate t)
                     (overlay-put ol 'before-string
                                  (propertize " " 'display
                                              `((margin left-margin) ,str)))
                     ;; (setq nlinum--ol-counter (1- nlinum--ol-counter))
                     ;; (when (= nlinum--ol-counter 0)
                     ;;   (run-with-idle-timer 0.5 nil #'nlinum--flush-overlays
                     ;;                        (current-buffer)))
                     (setq line (1+ line))
                     (zerop (forward-line 1))))))))
    ;; (setq nlinum--desc (format "-%d" (nlinum--ol-count)))
    nil))
