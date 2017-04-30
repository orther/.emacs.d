;;; feature/workspaces/config.el

(defvar +workspaces-last-persp nil
  "A variable that contains the last accessed perspective")

(def-package! persp-mode :demand t
  :config
  (setq wg-morph-on nil
        persp-autokill-buffer-on-remove 'kill-weak
        persp-nil-name "nil"
        persp-nil-hidden t
        persp-auto-save-fname "autosave"
        persp-auto-resume-time 1
        persp-auto-save-opt 1
        persp-save-dir (concat doom-cache-dir "workspaces/"))

  (defun +workspaces*track-last-persp (switch-fun &rest args)
    (let ((before-persp (safe-persp-name (get-current-persp)))
          (after-persp (apply switch-fun args)))
      (when (not (string= before-persp after-persp))
        (setq +workspaces-last-persp before-persp))))

  (advice-add #'persp-switch :around #'+workspaces*track-last-persp)
  
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
