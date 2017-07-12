;;; core-os.el -*- lexical-binding: t; -*-

(defconst IS-MAC   (eq system-type 'darwin))
(defconst IS-LINUX (eq system-type 'gnu/linux))

;; clipboard
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
      ;; Use a shared clipboard
      select-enable-clipboard t
      select-enable-primary t)

(after! evil
  (fset 'evil-visual-update-x-selection 'ignore))

(cond (IS-MAC
       (setq mac-command-modifier 'meta
             mac-option-modifier  'alt
             ;; sane trackpad/mouse scroll settings
             mac-redisplay-dont-reset-vscroll t
             mac-mouse-wheel-smooth-scroll nil
             mouse-wheel-scroll-amount '(5 ((shift) . 2))  ; one line at a time
             mouse-wheel-progressive-speed nil             ; don't accelerate scrolling
             ;; Curse Lion and its sudden but inevitable fullscreen mode!
             ;; NOTE Meaningless to railwaycat's emacs-mac build
             ns-use-native-fullscreen nil
             ;; Don't open files from the workspace in a new frame
             ns-pop-up-frames nil)

       (cond ((display-graphic-p)
              ;; A known problem with GUI Emacs on MacOS: it runs in an isolated
              ;; environment, so envvars will be wrong. That includes the PATH
              ;; Emacs picks up. `exec-path-from-shell' fixes this. This is slow
              ;; and benefits greatly from compilation.
              (setq exec-path
                    (or (eval-when-compile
                          (when (require 'exec-path-from-shell nil t)
                            (setq exec-path-from-shell-check-startup-files nil
                                  exec-path-from-shell-arguments (delete "-i" exec-path-from-shell-arguments))
                            (nconc exec-path-from-shell-variables '("GOPATH" "GOROOT" "PYTHONPATH"))
                            (exec-path-from-shell-initialize)
                            exec-path))
                        exec-path)))
             (t
              (when (require 'osx-clipboard nil t)
                (osx-clipboard-mode +1)))))

;; (cond
;;  (IS-MAC
;;    (unless window-system
;;      (setq interprogram-cut-function
;;            (lambda (text &optional push)
;;              (let* ((process-connection-type nil)
;;                     (pbproxy (start-process "pbcopy" "pbcopy" "/usr/bin/pbcopy")))
;;                (process-send-string pbproxy text)
;;                (process-send-eof pbproxy))))))

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
