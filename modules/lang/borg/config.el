;;; lang/borg/config.el -*- lexical-binding: t; -*-


(defvar +org-init-hook nil
  "TODO")

(add-hook 'org-load-hook #'+borg|init)
(add-hook 'org-mode-hook #'+borg|hook)

;; Custom variables
(defvar +borg-dir (expand-file-name "~/org/")
  "The directory where org files are kept.")
(defvaralias 'org-directory '+borg-dir)

(defvar +borg-organizer (concat +borg-dir "organizer.org")
  "The default organizer.")

(defvar +borg-attachment-dir ".attach/"
  "Where to store attachments (relative to current org file).")

;; Ensure ELPA org is prioritized above built-in org.
(when-let (path (locate-library "org" nil doom--package-load-path))
  (cl-pushnew (file-name-directory path) load-path))

(load! +agenda)
(load! +capture)


;;
;; Config
;;

(defun evil-org-eol-call (fun)
  "Go to end of line and call provided function.
FUN function callback"
  (end-of-line)
  (funcall fun)
  (evil-append nil))

(defun +borg|hook ()
  (setq line-spacing 1)

  ;; show-paren-mode causes problems for org-indent-mode
  (make-local-variable 'show-paren-mode)
  (setq show-paren-mode nil)
  (visual-line-mode +1)

  (evil-org-mode +1)

  (require 'toc-org)
  (toc-org-enable)

  (unless org-agenda-inhibit-startup
    ;; If saveplace places the point in a folded position, unfold it on load
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree)))))

  (defun +org|realign-table-maybe ()
    "Auto-align table under cursor."
    (when (org-at-table-p)
      (save-excursion
        (org-table-align))))
  (add-hook 'evil-insert-state-exit-hook #'+org|realign-table-maybe nil t)

  (defun +org|update-cookies ()
    "Update counts in headlines (aka \"cookies\")."
    (when (and buffer-file-name (file-exists-p buffer-file-name))
      (org-update-statistics-cookies nil)))

  (add-hook 'before-save-hook #'+org|update-cookies nil t)
  (add-hook 'evil-insert-state-exit-hook #'+org|update-cookies nil t))


(defun +borg|init ()
  "Initializes borg core."

  (define-minor-mode evil-org-mode
    "Evil-mode bindings for org-mode."
    :init-value nil
    :lighter " !"
    :keymap (make-sparse-keymap)
    :group 'evil-org)

  (setq-default
   org-export-coding-system 'utf-8
   org-todo-keywords '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)"))

   ;; Behavior
   org-catch-invisible-edits 'show
   org-checkbox-hierarchical-statistics t
   org-enforce-todo-checkbox-dependencies nil
   org-confirm-elisp-link-function nil
   org-default-priority ?C
   org-hierarchical-todo-statistics t
   org-loop-over-headlines-in-active-region t
   org-refile-use-outline-path t
   org-outline-path-complete-in-steps nil
   org-special-ctrl-a/e t

   ;; Sorting/refiling
   org-archive-location (concat +borg-dir "/archived/%s::")
   org-refile-targets '((nil . (:maxlevel . 6))))
  ;; smartparens config
  (sp-with-modes '(org-mode)
    (sp-local-pair "\\[" "\\]" :post-handlers '(("| " "SPC")))
    (sp-local-pair "\\(" "\\)" :post-handlers '(("| " "SPC")))
    (sp-local-pair "$$" "$$"   :post-handlers '((:add " | ")) :unless '(sp-point-at-bol-p))
    (sp-local-pair "{" nil))

  ;; Initialize everything else
  (+borg|init-ui)
  (+borg|init-keybinds)

  (run-hooks '+borg-init-hook)

  (+borg|hacks))

(defun +borg|init-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   ;org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   ;org-fontify-whole-heading-line t
   org-hide-emphasis-markers nil
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-indent-indentation-per-level 2
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-startup-folded t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   org-use-sub-superscripts '{}
   org-log-done 'time

   ;; Appearance
   outline-blank-line t
   org-indent-mode-turns-on-hiding-stars t
   org-adapt-indentation nil
   org-cycle-separator-lines 1
   org-cycle-include-plain-lists t
   ;; org-ellipsis "  "
   org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭")
                       ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-footnote-auto-label 'plain
   org-hidden-keywords nil)

  ;; Custom fontification
  (defsubst +borg--tag-face (n)
    (let ((kwd (match-string n)))
      (or (and (equal kwd "#") 'org-tag)
          (and (equal kwd "@") 'org-special-keyword))))

  (defun +borg|adjust-faces ()
    "Correct (and improve) org-mode's font-lock keywords.

  1. Re-set `org-todo' & `org-headline-done' faces, to make them respect
     underlying faces.
  2. Fontify item bullets
  3. Fontify item checkboxes (and when they're marked done)"
    (let ((org-todo (format org-heading-keyword-regexp-format
                            org-todo-regexp))
          (org-done (format org-heading-keyword-regexp-format
                            (concat "\\(?:" (mapconcat #'regexp-quote org-done-keywords "\\|") "\\)"))))
      (setq
       org-font-lock-extra-keywords
       (append (org-delete-all
                `(("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                   (0 (org-get-checkbox-statistics-face) t))
                  (,org-todo (2 (org-get-todo-face 2) t))
                  (,org-done (2 'org-headline-done t)))
                org-font-lock-extra-keywords)
               `((,org-todo (2 (org-get-todo-face 2) prepend))
                 (,org-done (2 'org-headline-done prepend))
                 ;; Make checkbox statistic cookies respect underlying faces
                 ("\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
                  (0 (org-get-checkbox-statistics-face) prepend))
                 ;; I like how org-mode fontifies checked TODOs and want this to extend to
                 ;; checked checkbox items:
                 ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                  1 'org-headline-done prepend)
                 ;; make plain list bullets stand out
                 ("^ *\\([-+]\\|[0-9]+[).]\\) " 1 'org-list-dt append)
                 ;; and separators/dividers
                 ("^ *\\(-----+\\)$" 1 'org-meta-line)
                 ;; custom #hashtags & @at-tags for another level of organization
                 ;; TODO refactor this into a single rule
                 ("\\s-\\(\\([#@]\\)[^ \n]+\\)" 1 (+borg--tag-face 2)))))))
  (add-hook 'org-font-lock-set-keywords-hook #'+borg|adjust-faces)

  (def-package! org-bullets
    :commands org-bullets-mode
    :init (add-hook 'org-mode-hook #'org-bullets-mode)))

(defun +borg|init-keybinds ()
  "Initialize my `org-mode' keybindings."
  (map! (:map org-mode-map
          "RET" #'org-return-indent
          "C-j" nil
          "C-k" nil)

        (:map evil-org-mode-map
          :n "RET" #'+borg/dwim-at-point
          ;; Navigate table cells
          :i  "M-l"   #'+borg/table-next-field
          :i  "M-h"   #'+borg/table-previous-field
          :i  "M-k"   #'+borg/table-previous-row
          :i  "M-j"   #'+borg/table-next-row

          :i  [tab]     #'+borg/indent-or-next-field-or-yas-expand
          :i  [backtab] #'+borg/dedent-or-prev-field

          :nv "j"   #'evil-next-visual-line
          :nv "k"   #'evil-previous-visual-line

          :ni [M-return]   (λ! (+borg/insert-item 'below))
          :ni [S-M-return] (λ! (+borg/insert-item 'above))
          :ni [C-return]   (λ! (+borg/insert-item 'below))
          :ni [S-C-return] (λ! (+borg/insert-item 'above))

          ;; Evil-org
          :niv "M-h" 'org-metaleft
          :niv "M-l" 'org-metaright
          :niv "M-k" 'org-metaup
          :niv "M-j" 'org-metadown
          :niv "M-L" 'org-shiftmetaright
          :niv "M-H" 'org-shiftmetaleft
          :niv "M-K" 'org-shiftmetaup
          :niv "M-J" 'org-shiftmetadown
          :niv "M-o" '(lambda () (interactive)
                        (evil-org-eol-call
                         '(lambda()
                            (org-insert-heading)
                            (org-metaright))))
          :niv "C-t" '(lambda () (interactive)
                        (evil-org-eol-call
                         '(lambda()
                            (org-insert-todo-heading nil)
                            (org-metaright))))
          :niv "M-t" '(lambda () (interactive)
                        (evil-org-eol-call
                         '(lambda()
                            (org-insert-todo-heading nil)
                            (org-metaright))))

          (:localleader
           :n  "RET" #'org-archive-subtree
           :n  "SPC" #'+borg/toggle-checkbox
           :n  "/"   #'org-sparse-tree
           :n  "="   #'org-align-all-tags
           :n  "?"   #'org-tags-view
           :n  "a"   #'org-agenda
           :n  "d"   #'org-time-stamp
           :n  "D"   #'org-deadline
           :n  "e"   #'org-edit-special
           :n  "E"   #'+borg/edit-special-same-window
           :n  "n"   (λ! (if (buffer-narrowed-p) (widen) (org-narrow-to-subtree)))
           :n  "r"   #'org-refile
           :n  "R"   (λ! (org-metaleft) (org-archive-to-archive-sibling)) ; archive to parent sibling
           :n  "s"   #'org-schedule
           :n  "t"   (λ! (org-todo (if (org-entry-is-todo-p) 'none 'todo)))
           :v  "t"   (λ! (evil-ex-normal evil-visual-beginning evil-visual-end "\\t"))
           :n  "T"   #'org-todo
           :n  "v"   #'variable-pitch-mode
           :nv "l"   #'org-insert-link
           :nv "L"   #'org-store-link
           :n  "+"   #'org-timestamp-up-day
           :n  "-"   #'org-timestamp-down-day)

          :n  "<"   #'org-metaleft
          :n  ">"   #'org-metaright
          :v  "<"   (λ! (org-metaleft)  (evil-visual-restore))
          :v  ">"   (λ! (org-metaright) (evil-visual-restore)))))

(defun +borg|hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (cl-pushnew '(file . find-file) org-link-frame-setup)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("\\.org$" . emacs)
          (t . ,(cond (IS-MAC "open -R \"%s\"")
                      (IS-LINUX "xdg-open \"%s\"")))))

  ;; Remove highlights on ESC
  (defun +borg|remove-occur-highlights ()
    (when (derived-mode-p 'org-mode)
      (org-remove-occur-highlights)
      t))
  (add-hook '+evil-esc-hook #'+borg|remove-occur-highlights))
