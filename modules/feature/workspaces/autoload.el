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
