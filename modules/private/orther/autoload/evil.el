;;; private/orther/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+orther:multi-next-line "private/orther/autoload/evil" nil t)
(evil-define-motion +orther:multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+orther:multi-previous-line "private/orther/autoload/evil" nil t)
(evil-define-motion +orther:multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+orther:cd "private/orther/autoload/evil" nil t)
(evil-define-command +orther:cd ()
  "Change `default-directory' with `cd'."
  (interactive "<f>")
  (cd input))

;;;###autoload (autoload '+orther:kill-all-buffers "private/orther/autoload/evil" nil t)
(evil-define-command +orther:kill-all-buffers (&optional bang)
  "Kill all buffers. If BANG, kill current session too."
  (interactive "<!>")
  (if bang
      (+workspace/kill-session)
    (doom/kill-all-buffers)))

;;;###autoload (autoload '+orther:kill-matching-buffers "private/orther/autoload/evil" nil t)
(evil-define-command +orther:kill-matching-buffers (&optional bang pattern)
  "Kill all buffers matching PATTERN regexp. If BANG, only match project
buffers."
  (interactive "<a>")
  (doom/kill-matching-buffers pattern bang))
