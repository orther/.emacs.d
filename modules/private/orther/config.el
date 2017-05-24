;;; private/orther/config.el

(when (featurep 'evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +orther-dir
  (file-name-directory load-file-name))

;; (defvar +orther-snippets-dir
;;   (expand-file-name "snippets/" +orther-dir))
;;
;; (setq user-mail-address "brandon@omt.tech"
;;       user-full-name "Brandon Orther"
;;       epa-file-encrypt-to user-mail-address
;;       auth-sources (list (expand-file-name ".authinfo.gpg" +orther-dir)))
;;
;; (defun +orther*no-authinfo-for-tramp (orig-fn &rest args)
;;   "Don't look into .authinfo for local sudo TRAMP buffers."
;;   (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
;;     (apply orig-fn args)))
;; (advice-add #'tramp-read-passwd :around #'+orther*no-authinfo-for-tramp)
;;
;;
;; ;; Don't use default snippets, use mine.
;; (after! yasnippet
;;   (setq yas-snippet-dirs (append (list '+orther-snippets-dir)
;;                                  (delete 'yas-installed-snippets-dir yas-snippet-dirs))))


;; ;; Repeat all sorts of motion and searches with SPC & C-SPC
;; (defmacro +my!repeat-with-spc (command next-func prev-func)
;;   "Repeat motions with SPC/S-SPC"
;;   (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
;;     `(progn
;;        (defun ,fn-sym (&rest _)
;;          (define-key evil-motion-state-map (kbd "SPC") ',next-func)
;;          (define-key evil-motion-state-map (kbd "S-SPC") ',prev-func))
;;        (advice-add #',command :before #',fn-sym))))

;; (after! evil
;;   n/N
;;   (+my!repeat-with-spc evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
;;   (+my!repeat-with-spc evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
;;   (+my!repeat-with-spc evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
;;   (+my!repeat-with-spc evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)
;;
;;   ;; f/F/t/T/s/S
;;   (after! evil-snipe
;;     (setq evil-snipe-repeat-keys nil
;;           evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;
;;
;;     (+my!repeat-with-spc evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
;;     (+my!repeat-with-spc evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
;;     (+my!repeat-with-spc evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
;;     (+my!repeat-with-spc evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
;;     (+my!repeat-with-spc evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
;;     (+my!repeat-with-spc evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
;;     (+my!repeat-with-spc evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
;;     (+my!repeat-with-spc evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))
;;
;;   ;; */#
;;   (after! evil-visualstar
;;     (+my!repeat-with-spc evil-visualstar/begin-search-forward
;;       evil-ex-search-next evil-ex-search-previous)
;;     (+my!repeat-with-spc evil-visualstar/begin-search-backward
;;       evil-ex-search-previous evil-ex-search-next)))


(after! mu4e
  (setq-default
   smtpmail-stream-type 'starttls
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

  (set! :email "gmail.com"
    '((mu4e-sent-folder       . "/%s/Sent Mail")
      (mu4e-drafts-folder     . "/%s/Drafts")
      (mu4e-trash-folder      . "/%s/Trash")
      (mu4e-refile-folder     . "/%s/All Mail")
      (smtpmail-smtp-user     . "brandonorther")
      (user-mail-address      . "brandon.orther@gmail.com")
      (mu4e-compose-signature . "---\nBrandon Orther")))

  (set! :email "omt.tech"
    '((mu4e-sent-folder       . "/%s/Sent Mail")
      (mu4e-drafts-folder     . "/%s/Drafts")
      (mu4e-trash-folder      . "/%s/Trash")
      (mu4e-refile-folder . "/%s/All Mail") (smtpmail-smtp-user .
      "brandon@omt.tech")
      (user-mail-address      . "brandon@omt.tech")
      (mu4e-compose-signature . "---\nBrandon Orther"))
    t))
