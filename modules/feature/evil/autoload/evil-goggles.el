;;; feature/evil/autoload/evil-goggles.el

;;;###autoload
(defun +evil/toggle-evil-goggles ()
  "Toggle evil goggles mode"
  (interactive)
  (if evil-goggles-mode
      (evil-goggles-mode -1)
    (evil-goggles-mode +1)))
