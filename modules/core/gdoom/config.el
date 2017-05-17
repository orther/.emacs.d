;;; core/gdoom/config.el

;; automatically indent pasted code

(defvar gdoom-yank-indent-threshold 1000 "don't auto indent over 1000 lines")

(defvar gdoom-indent-sensitive-modes '(conf-mode coffee-mode haml-mode python-mode slim-mode yaml-mode)
  "modes to limit auto indentation on")


(defmacro gdoom|advise-commands (advice-name commands class &rest body)
  "Apply advice named ADVICE-NAME to multiple COMMANDS.
  The body of the advice is in BODY."
  `(progn
     ,@(mapcar (lambda (command)
                 `(defadvice ,command
                      (,class ,(intern (format "%S-%s" command advice-name))
                              activate)
                    ,@body))
               commands)))

(defun gdoom|yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) gdoom-yank-indent-threshold)
      (indent-region beg end nil)))

(gdoom|advise-commands
  "indent" (yank yank-pop evil-paste-before evil-paste-after) around
  "If current mode is not one of gdoom-indent-sensitive-modes
  indent yanked text (with universal arg don't indent)."
  ad-do-it
  (evil-with-single-undo
    (if (and (not (equal '(4) (ad-get-arg 0)))
             (not (member major-mode gdoom-indent-sensitive-modes))
             (derived-mode-p 'prog-mode))
        (let ((transient-mark-mode nil)
              (save-undo buffer-undo-list))
          (gdoom|yank-advised-indent-function (region-beginning)
                                                (region-end))))))
