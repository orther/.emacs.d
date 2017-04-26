;;; feature/workspaces/config.el

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

 (with-eval-after-load "persp-mode"
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
              #'(lambda (b)
                  (when persp-mode
                    (let ((persp (get-current-persp)))
                      (if persp
                          (not (persp-contain-buffer-p b persp))
                        nil)))))

    (setq ivy-sort-functions-alist
          (append ivy-sort-functions-alist
                  '((persp-kill-buffer   . nil)
                    (persp-remove-buffer . nil)
                    (persp-add-buffer    . nil)
                    (persp-switch        . nil)
                    (persp-window-switch . nil)
                    (persp-frame-switch . nil)))))) 

  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
