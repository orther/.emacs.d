;;; feature/evil/packages.el

;;;###autoload
(defun counsel-projectile-rg-initial (&optional value)
  "Ivy version of `projectile-rg'."
  (interactive)
  (if (projectile-project-p)
        (counsel-rg value
                    (projectile-project-root)
                    nil
                    (projectile-prepend-project-name "rg"))
  (user-error "You're not in a project")))

;;;###autoload
(defun counsel-projectile-rg-region-or-symbol ()
  "Use `counsel-rg' to search for the selected region or
 the symbol around point in the current project with git grep."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (if (projectile-project-p)
      (counsel-projectile-rg-initial input)
      (counsel-rg input))))
