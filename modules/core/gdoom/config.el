;;; core/gdoom/config.el

;; automatically indent pasted code

(defvar doom-yank-indent-threshold 1000 "don't auto indent over 1000 lines")

(defvar doom-indent-sensitive-modes '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "modes to limit auto indentation on")


(defmacro doom|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
  The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun doom|yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) doom-yank-indent-threshold)
      (indent-region beg end nil)))

(doom|advise-commands
 "indent" (yank yank-pop evil-paste-before evil-paste-after) around
 "If current mode is not one of doom-indent-sensitive-modes
 indent yanked text (with universal arg don't indent)."
 (evil-start-undo-step)
 ad-do-it
 (if (and (not (equal '(4) (ad-get-arg 0)))
          (not (member major-mode doom-indent-sensitive-modes))
          (derived-mode-p 'prog-mode))
     (let ((transient-mark-mode nil)
           (save-undo buffer-undo-list))
       (doom|yank-advised-indent-function (region-beginning)
                                               (region-end))))
 (evil-end-undo-step))


