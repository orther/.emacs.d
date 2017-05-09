;;; tools/term/autoload.el

;;;###autoload
(defun +term ()
  (interactive)
  (call-interactively 'multi-term))

;;;###autoload
(defun +term/popup ()
  (interactive)
  (require 'multi-term)
  (let ((buf (get-buffer "*doom-esc:term*")))
    (if buf
      (doom-popup-buffer buf)
      (let* ((buffer (multi-term-get-buffer current-prefix-arg))
             (window (doom-popup-buffer buffer :popup t :align t :size 25 :select t)))
        (select-window window)
        (setq multi-term-buffer-list (nconc multi-term-buffer-list (list buffer)))
        (multi-term-internal)
        (with-current-buffer buffer
          (rename-buffer "*doom-esc:term*"))))))

;;;###autoload
(defun +term/project-popup ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (call-interactively '+term/popup)))
