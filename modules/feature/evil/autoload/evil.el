;;; feature/evil/packages.el

(eval-when-compile (require 'subr-x))

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
  (evil-delete beg end type ?_ yank-handler))

;;;###autoload (autoload 'evil-delete-without-register-if-whitespace "feature/evil/autoload/evil" nil t)

(evil-define-operator evil-delete-without-register-if-whitespace (beg end type reg yank-handler)
  (interactive "<R><y>")
  (let ((text (filter-buffer-substring beg end)))
    (if (s-blank? (s-collapse-whitespace text))
      (evil-delete beg end type ?_ yank-handler))
      (evil-delete beg end type reg yank-handler)))

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
