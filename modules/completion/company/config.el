;;; completion/company/config.el -*- lexical-binding: t; -*-

;; BMACS - hack to make flatten company backend lists
;; I prefer dumb completions normally and only enable expensive smart ones in select scenarios
;; TODO: revisit this and remove this hack
(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))


(def-setting! :company-backend (modes &rest backends)
  "Prepends BACKENDS to `company-backends' in major MODES.

MODES should be one major-mode symbol or a list of them."
  `(progn
     ,@(cl-loop for mode in (doom-enlist (doom-unquote modes))
                for def-name = (intern (format "doom--init-company-%s" mode))
                collect `(defun ,def-name ()
                           (when (and (eq major-mode ',mode)
                                      ,(not (eq backends '(nil))))
                             (require 'company)
                             (make-local-variable 'company-backends)
                             ,@(cl-loop for backend in (flatten backends)
                                        collect `(cl-pushnew ',backend company-backends :test #'equal))))
                collect `(add-hook! ,mode #',def-name))))


;;
;; Packages
;;

(def-package! company
  :commands (company-mode global-company-mode company-complete
             company-complete-common company-manual-begin company-grab-line)
  :config
  (setq company-idle-delay nil
        company-tooltip-limit 10
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-minimum-prefix-length 3
        company-require-match 'never
        company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode)
        company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-backends '(company-capf company-dabbrev-code company-keywords company-files company-dabbrev)
        company-transformers '(company-sort-by-occurrence))

  (after! yasnippet
    (nconc company-backends '(company-yasnippet)))

  (global-company-mode +1))


(def-package! company-statistics
  :after company
  :config
  (setq company-statistics-file (concat doom-cache-dir "company-stats-cache.el"))
  (quiet! (company-statistics-mode +1)))


;; Looks ugly on OSX without emacs-mac build
(def-package! company-quickhelp
  :after company
  :config
  (setq company-quickhelp-delay nil)
  (company-quickhelp-mode +1))


(def-package! company-dict
  :commands company-dict
  :config
  ;; Project-specific dictionaries
  (defun +company|enable-project-dicts (mode &rest _)
    (if (symbol-value mode)
        (push mode company-dict-minor-mode-list)
      (setq company-dict-minor-mode-list (delq mode company-dict-minor-mode-list))))
  (add-hook 'doom-project-hook #'+company|enable-project-dicts))


;;
;; Autoloads
;;

(autoload 'company-capf "company-capf")
(autoload 'company-yasnippet "company-yasnippet")
(autoload 'company-dabbrev "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-etags "company-etags")
(autoload 'company-elisp "company-elisp")
(autoload 'company-files "company-files")
(autoload 'company-gtags "company-gtags")
(autoload 'company-ispell "company-ispell")

