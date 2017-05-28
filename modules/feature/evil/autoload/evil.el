;;; feature/evil/autoload/evil.el

(eval-when-compile (require 'subr-x))

;;;###autoload
(defun +evil/matchit ()
  "Invoke `evil-matchit', but silently."
  (interactive)
  (ignore-errors (call-interactively #'evilmi-jump-items)))

;;;###autoload
(defun +evil/visual-indent ()
  "vnoremap < <gv"
  (interactive)
  (evil-shift-right (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun +evil/visual-dedent ()
  "vnoremap > >gv"
  (interactive)
  (evil-shift-left (region-beginning) (region-end))
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload (autoload 'evil-delete-char-without-register "feature/evil/autoload/evil" nil t)
(evil-define-operator evil-delete-char-without-register (beg end type reg)
  "delete character without yanking unless in visual mode"
  :motion evil-forward-char
  (interactive "<R><y>")
  (if (evil-visual-state-p)
    (evil-delete beg end type reg)
    (evil-delete beg end type ?_)))

;;;###autoload (autoload 'evil-delete-backward-char-without-register "feature/evil/autoload/evil" nil t)
(evil-define-operator evil-delete-backward-char-without-register (beg end type reg)
  "delete backward character without yanking"
  :motion evil-backward-char
  (interactive "<R><y>")
  (evil-delete beg end type ?_))

;;;###autoload (autoload 'evil-delete-without-register "feature/evil/autoload/evil" nil t)
(evil-define-operator evil-delete-without-register (beg end type reg yank-handler)
  (interactive "<R><y>")
  (evil-delete beg end type ?_))

;;;###autoload (autoload 'evil-delete-without-register-if-whitespace "feature/evil/autoload/evil" nil t)
(evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
  (interactive "<R><y>")
  (let ((text (filter-buffer-substring beg end)))
    (if (s-blank? (s-trim text))
      (evil-delete beg end type ?_)
      (evil-delete beg end type reg yank-handler))))

;;;###autoload (autoload 'evil-delete-line-without-register "feature/evil/autoload/evil" nil t)
(evil-define-operator evil-delete-line-without-register (beg end type reg yank-handler)
    (interactive "<R><y>")
    (evil-delete-line beg end type ?_ yank-handler))

;;;###autoload (autoload 'evil-change-without-register "feature/evil/autoload/evil" nil t)
(evil-define-operator evil-change-without-register (beg end type reg yank-handler)
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler))

;;;###autoload (autoload 'evil-change-line-without-register "feature/evil/autoload/evil" nil t)
(evil-define-operator evil-change-line-without-register (beg end type reg yank-handler)
  "Change to end of line without yanking."
  :motion evil-end-of-line
  (interactive "<R><y>")
  (evil-change beg end type ?_ yank-handler #'evil-delete-line))

;;;###autoload (autoload 'evil-paste-after-without-register "feature/evil/autoload/evil" nil t)
(evil-define-command evil-paste-after-without-register (count &optional register yank-handler)
  "evil paste before without yanking"
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste-without-register count register)
      (evil-paste-after count register yank-handler)))

;;;###autoload (autoload 'evil-paste-before-without-register "feature/evil/autoload/evil" nil t)
(evil-define-command evil-paste-before-without-register (count &optional register yank-handler)
  "evil paste before without yanking"
  :suppress-operator t
  (interactive "P<x>")
  (if (evil-visual-state-p)
      (evil-visual-paste-without-register count register)
      (evil-paste-before count register yank-handler)))

;;;###autoload (autoload 'evil-visual-paste-without-register "feature/evil/autoload/evil" nil t)
(evil-define-command evil-visual-paste-without-register (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         new-kill
         paste-eob)
    (evil-with-undo
      (let* ((kill-ring (list (current-kill 0)))
             (kill-ring-yank-pointer kill-ring))
        (when (evil-visual-state-p)
          (evil-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `evil-paste-after' because `evil-delete'
          ;; will move point to the line above
          (when (and (= evil-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (evil-delete-without-register evil-visual-beginning evil-visual-end
                       (evil-visual-type))
          (when (and (eq yank-handler #'evil-yank-line-handler)
                     (not (eq (evil-visual-type) 'line))
                     (not (= evil-visual-end (point-max))))
            (insert "\n"))
          (evil-normal-state)
          (setq new-kill (current-kill 0))
          (current-kill 1))
        (if paste-eob
            (evil-paste-after count register)
          (evil-paste-before count register)))
      (kill-new new-kill)
      ;; mark the last paste as visual-paste
      (setq evil-last-paste
            (list (nth 0 evil-last-paste)
                  (nth 1 evil-last-paste)
                  (nth 2 evil-last-paste)
                  (nth 3 evil-last-paste)
                  (nth 4 evil-last-paste)
                  t)))))
;;;###autoload
(defun +evil/reselect-paste ()
  "Go back into visual mode and reselect the last pasted region."
  (interactive)
  (evil-goto-mark ?\[)
  (evil-visual-state)
  (evil-goto-mark ?\]))

;;;###autoload
(defun +evil*ex-replace-special-filenames (file-name)
  "Replace special symbols in FILE-NAME. Modified to include other substitution
flags. See http://vimdoc.sourceforge.net/htmldoc/cmdline.html#filename-modifiers."
  (let* (case-fold-search
         (regexp (concat "\\(?:^\\|[^\\\\]\\)"
                         "\\([#%]\\)"
                         "\\(\\(?::\\(?:[PphtreS~.]\\|g?s[^:\t\n ]+\\)\\)*\\)"))
         (matches
          (let ((all-strings ())
                (i 0))
            (while (and (< i (length file-name))
                        (string-match regexp file-name i))
              (setq i (1+ (match-beginning 0)))
              (let (strings)
                (push (dotimes (i (/ (length (match-data)) 2) (nreverse strings))
                        (push (match-string i file-name) strings))
                      all-strings)))
            (nreverse all-strings))))
    (dolist (match matches)
      (let ((flags (split-string (car (cdr (cdr match))) ":" t))
            (path (and buffer-file-name
                       (pcase (car (cdr match))
                         ("%" (file-relative-name buffer-file-name))
                         ("#" (save-excursion (other-window 1) (file-relative-name buffer-file-name))))))
            flag global)
        (if (not path)
            (setq path "")
          (while flags
            (setq flag (pop flags))
            (when (string-suffix-p "\\" flag)
              (setq flag (concat flag (pop flags))))
            (when (string-prefix-p "gs" flag)
              (setq global t
                    flag (substring flag 1)))
            (setq path
                  (or (pcase (substring flag 0 1)
                        ("p" (expand-file-name path))
                        ("~" (concat "~/" (file-relative-name path "~")))
                        ("." (file-relative-name path default-directory))
                        ("t" (file-name-nondirectory (directory-file-name path)))
                        ("r" (file-name-sans-extension path))
                        ("e" (file-name-extension path))
                        ("S" (shell-quote-argument path))
                        ("h"
                         (let ((parent (file-name-directory (expand-file-name path))))
                           (unless (equal (file-truename path)
                                          (file-truename parent))
                             (if (file-name-absolute-p path)
                                 (directory-file-name parent)
                               (file-relative-name parent)))))
                        ("s"
                         (when-let (args (evil-delimited-arguments (substring flag 1) 2))
                           (let ((pattern (evil-transform-vim-style-regexp (car args)))
                                 (replace (cadr args)))
                             (replace-regexp-in-string
                              (if global pattern (concat "\\(" pattern "\\).*\\'"))
                              (evil-transform-vim-style-regexp replace) path t t
                              (unless global 1)))))
                        ("P"
                         (let ((default-directory (file-name-directory (expand-file-name path))))
                           (abbreviate-file-name (doom-project-root))))
                        (_ path))
                      "")))
          ;; strip trailing slash, if applicable
          (when (and (not (string= path "")) (equal (substring path -1) "/"))
            (setq path (substring path 0 -1))))
        (setq file-name
              (replace-regexp-in-string (format "\\(?:^\\|[^\\\\]\\)\\(%s\\)"
                                                (regexp-quote (string-trim-left (car match))))
                                        path file-name t t 1))))
    (setq file-name (replace-regexp-in-string regexp "\\1" file-name t))))

(defun +evil--window-swap (direction)
  "Move current window to the next window in DIRECTION. If there are no windows
there and there is only one window, split in that direction and place this
window there. If there are no windows and this isn't the only window, use
evil-window-move-* (e.g. `evil-window-move-far-left')"
  (let* ((this-window (get-buffer-window))
         (this-buffer (current-buffer))
         (that-window (windmove-find-other-window direction nil this-window))
         (that-buffer (window-buffer that-window)))
    (when (or (minibufferp that-buffer)
              (doom-popup-p that-window))
      (setq that-buffer nil that-window nil))
    (if (not (or that-window (one-window-p t)))
        (funcall (case direction
                   ('left 'evil-window-move-far-left)
                   ('right 'evil-window-move-far-right)
                   ('up 'evil-window-move-very-top)
                   ('down 'evil-window-move-very-bottom)))
      (unless that-window
        (setq that-window
              (split-window this-window nil (cond ((eq direction 'up) 'above)
                                                  ((eq direction 'down) 'below)
                                                  (t direction))))
        (with-selected-window that-window
          (switch-to-buffer doom-buffer))
        (setq that-buffer (window-buffer that-window)))
      (with-selected-window this-window
        (switch-to-buffer that-buffer))
      (with-selected-window that-window
        (switch-to-buffer this-buffer))
      (select-window that-window))))

;;;###autoload
(defun +evil/window-move-left () "`+evil--window-swap'"  (interactive) (+evil--window-swap 'left))
;;;###autoload
(defun +evil/window-move-right () "`+evil--window-swap'" (interactive) (+evil--window-swap 'right))
;;;###autoload
(defun +evil/window-move-up () "`+evil--window-swap'"    (interactive) (+evil--window-swap 'up))
;;;###autoload
(defun +evil/window-move-down () "`+evil--window-swap'"  (interactive) (+evil--window-swap 'down))

;;;###autoload (autoload '+evil:macro-on-all-lines "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:macro-on-all-lines (beg end &optional macro)
  "Apply macro to each line."
  :motion nil
  :move-point nil
  (interactive "<r><a>")
  (unless (and beg end)
    (setq beg (region-beginning)
          end (region-end)))
  (evil-ex-normal beg end
                  (concat "@"
                          (single-key-description
                           (or macro (read-char "@-"))))))

;;;###autoload (autoload '+evil:retab "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:retab (&optional beg end)
  "Wrapper around `doom/retab'."
  :motion nil :move-point nil :type line
  (interactive "<r>")
  (doom/retab beg end))

;;;###autoload (autoload '+evil:narrow-buffer "feature/evil/autoload/evil" nil t)
(evil-define-command +evil:narrow-buffer (beg end &optional bang)
  "Wrapper around `doom-narrow-buffer'."
  :move-point nil
  (interactive "<r><!>")
  (doom-narrow-buffer beg end bang))


;; --- custom arg handlers ----------------

(defun +evil--ex-match-init (name &optional face update-hook)
  (with-current-buffer evil-ex-current-buffer
    (cond
     ((eq flag 'start)
      (evil-ex-make-hl name
        :face (or face 'evil-ex-substitute-matches)
        :update-hook (or update-hook #'evil-ex-pattern-update-ex-info))
      (setq flag 'update))

     ((eq flag 'stop)
      (evil-ex-delete-hl name)))))

(defun +evil--ex-buffer-match (arg &optional hl-name flags beg end)
  (when (and (eq flag 'update)
             evil-ex-substitute-highlight-all
             (not (zerop (length arg))))
    (condition-case lossage
        (let ((pattern (evil-ex-make-substitute-pattern
                        (if evil-ex-bang (regexp-quote arg) arg)
                        (or flags (list))))
              (range (or (evil-copy-range evil-ex-range)
                         (evil-range (or beg (line-beginning-position))
                                     (or end (line-end-position))
                                     'line
                                     :expanded t))))
          (evil-expand-range range)
          (evil-ex-hl-set-region hl-name
                                 (max (evil-range-beginning range) (window-start))
                                 (min (evil-range-end range) (window-end)))
          (evil-ex-hl-change hl-name pattern))
      (end-of-file
       (evil-ex-pattern-update-ex-info nil "incomplete replacement"))
      (user-error
       (evil-ex-pattern-update-ex-info nil (format "?%s" lossage))))))

;;;###autoload
(defun +evil-ex-buffer-match (flag &optional arg)
  (let ((hl-name 'evil-ex-buffer-match))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (+evil--ex-buffer-match arg hl-name (list (if evil-ex-substitute-global ?g))))))

;;;###autoload
(defun +evil-ex-global-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-match))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (+evil--ex-buffer-match arg hl-name nil (point-min) (point-max)))))

;;;###autoload
(defun +evil-ex-global-delim-match (flag &optional arg)
  (let ((hl-name 'evil-ex-global-delim-match))
    (with-selected-window (minibuffer-selected-window)
      (+evil--ex-match-init hl-name)
      (let ((result (car-safe (evil-delimited-arguments arg 2))))
        (+evil--ex-buffer-match result hl-name nil (point-min) (point-max))))))

;;;###autoload (autoload '+evil:align "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil:align (beg end pattern &optional bang)
  "Ex interface to `align-regexp'. Accepts vim-style regexps."
  (interactive "<r><//><!>")
  (align-regexp
   beg end
   (concat "\\(\\s-*\\)"
           (if bang
               (regexp-quote pattern)
             (evil-transform-vim-style-regexp pattern)))
   1 1))


;; --- wgrep ------------------------------

;;;###autoload (autoload '+evil-delete "feature/evil/autoload/evil" nil t)
(evil-define-operator +evil-delete (beg end type register yank-handler)
  "A wrapper around `evil-delete' for `wgrep' buffers that will invoke
`wgrep-mark-deletion' on lines you try to delete."
  (interactive "<R><x><y>")
  (condition-case ex
      (evil-delete beg end type register yank-handler)
    ('text-read-only
     (evil-apply-on-block
      (lambda (beg _)
        (goto-char beg)
        (call-interactively #'wgrep-mark-deletion))
      beg (1- end) nil))))
