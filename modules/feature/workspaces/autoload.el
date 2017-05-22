;;; feature/workspaces/autoload.el

;;;###autoload
(defun +workspace-exists-p (name)
  "Returns t if NAME is the name of an existing workspace."
  (when (symbolp name)
    (setq name (symbol-name name)))
  (unless (stringp name)
    (error "Expected a string, got a %s" (type-of name)))
    (member name (persp-names-current-frame-fast-ordered)))

;;;###autoload
(defun +workspace-switch-last ()
  "Switches to the last workspace"
  (interactive)
  (if (+workspace-exists-p +workspaces-last-persp)
    (persp-switch +workspaces-last-persp)
    (error "No previous workspace.")))

;;;###autoload
(defun +workspace-switch-project (arg)
  (interactive "P")
  (ivy-read "Switch to Project Perspective: "
            (if (projectile-project-p)
                (cons (abbreviate-file-name (projectile-project-root))
                      (projectile-relevant-known-projects))
              projectile-known-projects)
            :action (lambda (project)
                      (let ((persp-reset-windows-on-nil-window-conf t))
                        (persp-switch project)
                        (let ((projectile-completion-system 'ivy))
                          (projectile-switch-project-by-name project))))))
