;;; core-os.el
(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

;; clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use a shared clipboard
      select-enable-clipboard t
      select-enable-primary t)

(after! evil
  (fset 'evil-visual-update-x-selection 'ignore))

(cond
 (IS-MAC
   (unless window-system
     (setq interprogram-cut-function
           (lambda (text &optional push)
             (let* ((process-connection-type nil)
                    (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
               (process-send-string pbproxy text)
               (process-send-eof pbproxy))))))

 (IS-LINUX
   (progn
     (setq select-enable-clipboard t)
     (defun xsel-cut-function (text &optional push)
       (with-temp-buffer
         (insert text)
         (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
     (defun xsel-paste-function()
       (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
         (unless (string= (car kill-ring) xsel-output)
           xsel-output )))
     (setq interprogram-cut-function 'xsel-cut-function)
     (setq interprogram-paste-function 'xsel-paste-function))))

(provide 'core-os)
;;; core-os.el ends here
