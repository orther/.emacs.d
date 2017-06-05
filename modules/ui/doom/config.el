;;; ui/doom/config.el

(defvar +doom-theme 'doom-vibrant
  "The color theme to use.")

(defvar +doom-font
  (font-spec :family "Iosevka" :size 24)
  "The font currently in use.")

(defvar +doom-variable-pitch-font
  (font-spec :family "Iosevka" :size 26)
  "The font currently in use.")

(defvar +doom-unicode-font
  (font-spec :family "Iosevka" :size 24)
  "Fallback font for unicode glyphs.")


;;; Set fonts
(when (display-graphic-p)
  (with-demoted-errors "FONT ERROR: %s"
    (set-frame-font +doom-font t t)
    ;; Fallback to `doom-unicode-font' for Unicode characters
    (when +doom-unicode-font
      (set-fontset-font t 'unicode +doom-unicode-font))
    ;; ...and for variable-pitch mode
    (when +doom-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :font +doom-variable-pitch-font))))


;; doom-one: gives Emacs a look inspired by Dark One in Atom.
;; <https://github.com/hlissner/emacs-doom-theme>
(def-package! doom-themes
  :load-path "~/work/plugins/emacs-doom-themes/"
  :demand t
  :config
  (load-theme +doom-theme t)


  ;; Add file icons to doom-neotree
  (doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch t
        doom-neotree-file-icons 'simple
        doom-neotree-line-spacing 2)

  ;; Since Fira Mono doesn't have an italicized variant, highlight it instead
  (set-face-attribute 'italic nil
                      :weight 'ultra-light
                      :foreground "#ffffff"
                      :background (doom-color 'current-line))

  ;; Dark frames by default
  (when (display-graphic-p)
    (push (cons 'background-color (doom-color 'bg)) initial-frame-alist)
    (push (cons 'foreground-color (doom-color 'fg)) initial-frame-alist))

  (after! neotree
    (defun +doom|neotree-fix-popup ()
      "Ensure the fringe settings are maintained on popup restore."
      (neo-global--when-window
        (doom--neotree-no-fringes)))
    (add-hook 'doom-popup-mode-hook #'+doom|neotree-fix-popup nil t)))


(def-package! solaire-mode
  :commands (solaire-mode turn-on-solaire-mode turn-off-solaire-mode)
  :init
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'doom-popup-mode-hook #'turn-off-solaire-mode)
  :config
  (setq solaire-mode-real-buffer-fn #'doom-real-buffer-p)

  ;; Extra modes to activate doom-buffer-mode in
  (add-hook! (gist-mode
              twittering-mode
              mu4e-view-mode
              org-tree-slide-mode
              +regex-mode)
    #'solaire-mode))


(after! hideshow
  (defface +doom-folded-face
    `((((background dark))
       (:inherit font-lock-comment-face :background ,(doom-color 'black)))
      (((background light))
       (:inherit font-lock-comment-face :background ,(doom-color 'light-grey))))
    "Face to hightlight `hideshow' overlays."
    :group 'doom)

  ;; Nicer code-folding overlays (with fringe indicators)
  (setq hs-set-up-overlay
        (lambda (ov)
          (when (eq 'code (overlay-get ov 'hs))
            (when (featurep 'vimish-fold)
              (overlay-put
               ov 'before-string
               (propertize "…" 'display
                           (list vimish-fold-indication-mode
                                 'empty-line
                                 'vimish-fold-fringe))))
            (overlay-put
             ov 'display (propertize "  [...]  " 'face '+doom-folded-face))))))


(when (and (display-graphic-p) (fboundp 'define-fringe-bitmap))
  ;; NOTE Adjust these bitmaps if you change `doom-ui-fringe-size'
  (after! flycheck
    ;; because git-gutter is in the left fringe
    (setq flycheck-indication-mode 'right-fringe)
    ;; A non-descript, left-pointing arrow
    (fringe-helper-define 'flycheck-fringe-bitmap-double-arrow 'center
      "...X...."
      "..XX...."
      ".XXX...."
      "XXXX...."
      ".XXX...."
      "..XX...."
      "...X...."))

  ;; subtle diff indicators in the fringe
  (after! git-gutter-fringe
    ;; places the git gutter outside the margins.
    (setq-default fringes-outside-margins t)
    ;; thin fringe bitmaps
    (fringe-helper-define 'git-gutter-fr:added '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:deleted 'bottom
      "X......."
      "XX......"
      "XXX....."
      "XXXX....")))
