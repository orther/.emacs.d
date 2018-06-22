;;; core-modules.el --- module & package management system -*- lexical-binding: t; -*-

(defvar doom-init-modules-p nil
  "Non-nil if `doom-initialize-modules' has run.")

(defvar doom-modules ()
  "A hash table of enabled modules. Set by `doom-initialize-modules'.")

(defvar doom-modules-dirs
  (list (expand-file-name "modules/" doom-private-dir) doom-modules-dir)
  "A list of module root directories. Order determines priority.")

(defconst doom-obsolete-modules
  '(((:tools rotate-text)       (:editor rotate-text))
    ((:emacs electric-indent)   (:emacs electric))
    ((:feature version-control) (:emacs vc) (:ui vc-gutter)))
  "An alist of deprecated modules, mapping deprecated modules to an optional new
location (which will create an alias). Each CAR and CDR is a (CATEGORY .
MODULES). E.g.

  ((:emacs . electric-indent) . (:emacs electric))
  ((:feature . version-control) (:emacs vc) (:ui . vc-gutter))

A warning will be put out if these deprecated modules are used.")

(defvar doom--current-module nil)


;;
;; Bootstrap API
;;

(defun doom-initialize-modules (&optional force-p)
  "Loads the init.el in `doom-private-dir' and sets up hooks for a healthy
session of Dooming. Will noop if used more than once, unless FORCE-P is
non-nil."
  (when (or force-p (not doom-init-modules-p))
    ;; Set `doom-init-modules-p' early, so `doom-pre-init-hook' won't infinitely
    ;; recurse by accident if any of them need `doom-initialize-modules'.
    (setq doom-init-modules-p t)
    (when doom-private-dir
      (condition-case e
          (load (expand-file-name "init" doom-private-dir)
                'noerror 'nomessage)
        ((debug doom-error) (signal (car e) (cdr e)))
        ((debug error) (signal 'doom-private-error (list "init.el" e)))))))


;;
;; Module API
;;

(defun doom-module-p (category module)
  "Returns t if CATEGORY MODULE is enabled (ie. present in `doom-modules')."
  (and (hash-table-p doom-modules)
       (gethash (cons category module) doom-modules)
       t))

(defun doom-module-get (category module &optional property)
  "Returns the plist for CATEGORY MODULE. Gets PROPERTY, specifically, if set."
  (when-let* ((plist (gethash (cons category module) doom-modules)))
    (if property
        (plist-get plist property)
      plist)))

(defun doom-module-put (category module property value &rest rest)
  "Set a PROPERTY for CATEGORY MODULE to VALUE. PLIST should be additional pairs
of PROPERTY and VALUEs."
  (when-let* ((plist (doom-module-get category module)))
    (plist-put plist property value)
    (when rest
      (when (cl-oddp (length rest))
        (signal 'wrong-number-of-arguments (list (length rest))))
      (while rest
        (plist-put rest (pop rest) (pop rest))))
    (puthash (cons category module) plist doom-modules)))

(defun doom-module-set (category module &rest plist)
  "Enables a module by adding it to `doom-modules'.

CATEGORY is a keyword, module is a symbol, PLIST is a plist that accepts the
following properties:

  :flags [SYMBOL LIST]  list of enabled category flags
  :path  [STRING]       path to category root directory

Example:
  (doom-module-set :lang 'haskell :flags '(+intero))"
  (when plist
    (let ((old-plist (doom-module-get category module)))
      (unless (plist-member plist :flags)
        (plist-put plist :flags (plist-get old-plist :flags)))
      (unless (plist-member plist :path)
        (plist-put plist :path (or (plist-get old-plist :path)
                                   (doom-module-locate-path category module))))))
  (puthash (cons category module)
           plist
           doom-modules))

(defun doom-module-path (category module &optional file)
  "Like `expand-file-name', but expands FILE relative to CATEGORY (keywordp) and
MODULE (symbol).

If the category isn't enabled this will always return nil. For finding disabled
modules use `doom-module-locate-path'."
  (let ((path (doom-module-get category module :path)))
    (if file (expand-file-name file path)
      path)))

(defun doom-module-locate-path (category &optional module file)
  "Searches `doom-modules-dirs' to find the path to a module.

CATEGORY is a keyword (e.g. :lang) and MODULE is a symbol (e.g. 'python). FILE
is a string that will be appended to the resulting path. If no path exists, this
returns nil, otherwise an absolute path.

This doesn't require modules to be enabled. For enabled modules us
`doom-module-path'."
  (when (keywordp category)
    (setq category (substring (symbol-name category) 1)))
  (when (and module (symbolp module))
    (setq module (symbol-name module)))
  (cl-loop for default-directory in doom-modules-dirs
           for path = (concat category "/" module "/" file)
           if (file-exists-p path)
           return (expand-file-name path)))

(defun doom-module-from-path (&optional path)
  "Returns a cons cell (CATEGORY . MODULE) derived from PATH (a file path)."
  (or doom--current-module
      (let ((path (or path (FILE!))))
        (save-match-data
          (setq path (file-truename path))
          (when (string-match "/modules/\\([^/]+\\)/\\([^/]+\\)\\(?:/.*\\)?$" path)
            (when-let* ((category (match-string 1 path))
                        (module   (match-string 2 path)))
              (cons (doom-keyword-intern category)
                    (intern module))))))))

(defun doom-module-load-path (&optional all-p)
  "Return a list of absolute file paths to activated modules. If ALL-P is
non-nil, return paths of possible modules, activated or otherwise."
  (append (if all-p
              (doom-files-in doom-modules-dirs
                             :type 'dirs
                             :mindepth 1
                             :depth 1
                             :full t)
            (cl-loop for plist being the hash-values of (doom-modules)
                     collect (plist-get plist :path)))
          (list doom-private-dir)))

(defun doom-modules (&optional refresh-p)
  "Minimally initialize `doom-modules' (a hash table) and return it."
  (or (unless refresh-p doom-modules)
      (let ((noninteractive t)
            doom-init-modules-p)
        (message "Initializing modules")
        (doom-initialize-modules t)
        (unless doom-modules
          (setq doom-modules
                (make-hash-table :test #'equal
                                 :size 20
                                 :rehash-threshold 1.0)))
        doom-modules)))


;;
;; Use-package modifications
;;

(autoload 'use-package "use-package-core" nil nil t)

(setq use-package-compute-statistics doom-debug-mode
      use-package-verbose doom-debug-mode
      use-package-minimum-reported-time (if doom-debug-mode 0 0.1)
      use-package-expand-minimally (not noninteractive))

;; Adds the :after-call custom keyword to `use-package' (and consequently,
;; `def-package!'). :after-call takes a symbol or list of symbols. These symbols
;; can be functions or hook variables.
;;
;;   (use-package X :after-call find-file-hook)
;;
;; This will load X on the first invokation of `find-file-hook' (then it will
;; remove itself from the hook/function).
(defvar doom--deferred-packages-alist ())
(after! use-package-core
  (add-to-list 'use-package-deferring-keywords :after-call nil #'eq)
  (setq use-package-keywords
        (use-package-list-insert :after-call use-package-keywords :after))

  (defalias 'use-package-normalize/:after-call 'use-package-normalize-symlist)
  (defun use-package-handler/:after-call (name _keyword hooks rest state)
    (let ((fn (intern (format "doom|transient-hook--load-%s" name)))
          (hooks (delete-dups hooks)))
      (if (plist-get state :demand)
          (use-package-process-keywords name rest state)
        (use-package-concat
         `((fset ',fn
                 (lambda (&rest _)
                   (when doom-debug-mode
                     (message "Loading deferred package %s from %s" ',name ',fn))
                   (condition-case e (require ',name)
                     ((debug error)
                      (message "Failed to load deferred package %s: %s" ',name e)))
                   (dolist (hook (cdr (assq ',name doom--deferred-packages-alist)))
                     (if (functionp hook)
                         (advice-remove hook #',fn)
                       (remove-hook hook #',fn)))
                   (map-delete doom--deferred-packages-alist ',name)
                   (fmakunbound ',fn))))
         (cl-loop for hook in hooks
                  collect (if (functionp hook)
                              `(advice-add #',hook :before #',fn)
                            `(add-hook ',hook #',fn)))
         `((map-put doom--deferred-packages-alist
                    ',name
                    '(,@hooks ,@(cdr (assq name doom--deferred-packages-alist)))))
         (use-package-process-keywords name rest state))))))


;;
;; Module config macros
;;

(defmacro doom! (&rest modules)
  "Bootstraps DOOM Emacs and its modules.

The bootstrap process involves making sure the essential directories exist, core
packages are installed, `doom-autoload-file' is loaded, `doom-packages-file'
cache exists (and is loaded) and, finally, loads your private init.el (which
should contain your `doom!' block).

If the cache exists, much of this function isn't run, which substantially
reduces startup time.

The overall load order of Doom is as follows:

  ~/.emacs.d/init.el
  ~/.emacs.d/core/core.el
  `doom-pre-init-hook'
  ~/.doom.d/init.el
  Module init.el files
  `doom-init-hook'
  Module config.el files
  ~/.doom.d/config.el
  `after-init-hook'
  `emacs-startup-hook'
  `doom-post-init-hook' (at end of `emacs-startup-hook')

Module load order is determined by your `doom!' block. See `doom-modules-dirs'
for a list of all recognized module trees. Order defines precedence (from most
to least)."
  (let ((doom-modules
         (make-hash-table :test #'equal
                          :size (if modules (length modules) 100)
                          :rehash-threshold 1.0))
        category init-forms config-forms)
    (while modules
      (let ((m (pop modules)))
        (cond ((keywordp m) (setq category m))
              ((not category) (error "No module category specified for %s" m))
              ((catch 'doom-modules
                 (let* ((module (if (listp m) (car m) m))
                        (flags  (if (listp m) (cdr m))))
                   (when-let* ((new (assoc (list category module) doom-obsolete-modules)))
                     (let ((newkeys (cdr new)))
                       (if (null newkeys)
                           (message "Warning: the %s module is deprecated" key)
                         (message "Warning: the %s module is deprecated. Use %s instead."
                                  (list category module) newkeys)
                         (push category modules)
                         (dolist (key newkeys)
                           (setq modules (append key modules)))
                         (throw 'doom-modules t))))
                   (let ((path (doom-module-locate-path category module)))
                     (if (not path)
                         (message "Couldn't find the %s %s module" category module)
                       (let ((key (cons category module)))
                         (doom-module-set category module :flags flags :path path)
                         (push `(let ((doom--current-module ',key))
                                  (load! "init" ,path t))
                               init-forms)
                         (push `(let ((doom--current-module ',key))
                                  (load! "config" ,path t))
                               config-forms))))))))))
    `(let (file-name-handler-alist)
       (setq doom-modules ',doom-modules)
       ,@(nreverse init-forms)
       (run-hook-wrapped 'doom-init-hook #'doom-try-run-hook)
       (unless noninteractive
         (let ((doom--stage 'config))
           ,@(nreverse config-forms)
           (when doom-private-dir
             (load! "config" doom-private-dir t)))))))

(defvar doom-disabled-packages)
(defmacro def-package! (name &rest plist)
  "This is a thin wrapper around `use-package'."
  `(use-package ,name
     ,@(append (if (memq name doom-disabled-packages) `(:disabled t))
               plist)))

(defmacro def-package-hook! (package when &rest body)
  "Reconfigures a package's `def-package!' block.

Only use this macro in a module's init.el file.

Under the hood, this uses use-package's `use-package-inject-hooks'.

PACKAGE is a symbol; the package's name.
WHEN should be one of the following:
  :pre-init :post-init :pre-config :post-config

WARNING: If :pre-init or :pre-config hooks return nil, the original
`def-package!''s :init/:config block (respectively) is overwritten, so remember
to have them return non-nil (or exploit that to overwrite Doom's config)."
  (declare (indent defun))
  (doom--assert-stage-p 'init #'package!)
  (unless (memq when '(:pre-init :post-init :pre-config :post-config))
    (error "'%s' isn't a valid hook for def-package-hook!" when))
  `(progn
     (setq use-package-inject-hooks t)
     (add-hook!
       ',(intern (format "use-package--%s--%s-hook"
                         package
                         (substring (symbol-name when) 1)))
       ,@body)))

(defmacro require! (category module &rest plist)
  "Loads the module specified by CATEGORY (a keyword) and MODULE (a symbol)."
  (let ((doom-modules (copy-hash-table doom-modules)))
    (apply #'doom-module-set category module
           (mapcar #'eval plist))
    (let ((module-path (doom-module-locate-path category module)))
      (if (directory-name-p module-path)
          `(condition-case-unless-debug ex
               (let ((doom--current-module ',(cons category module)))
                 (load! "init" ,module-path :noerror)
                 (let ((doom--stage 'config))
                   (load! "config" ,module-path :noerror)))
             ('error
              (lwarn 'doom-modules :error
                     "%s in '%s %s' -> %s"
                     (car ex) ,category ',module
                     (error-message-string ex))))
        (warn 'doom-modules :warning "Couldn't find module '%s %s'"
              category module)))))

(defmacro featurep! (module &optional submodule flag)
  "Returns t if MODULE SUBMODULE is enabled. If FLAG is provided, returns t if
MODULE SUBMODULE has FLAG enabled.

  (featurep! :config default)

Module FLAGs are set in your config's `doom!' block, typically in
~/.emacs.d/init.el. Like so:

  :config (default +flag1 -flag2)

When this macro is used from inside a module, MODULE and SUBMODULE can be
omitted. eg. (featurep! +flag1)"
  (unless submodule
    (let* ((path (FILE!))
           (module-pair (doom-module-from-path path)))
      (unless module-pair
        (error "featurep! couldn't detect what module its in! (in %s)" path))
      (setq flag module
            module (car module-pair)
            submodule (cdr module-pair))))
  (if flag
      (and (memq flag (doom-module-get module submodule :flags)) t)
    (doom-module-p module submodule)))


;;
;; FIXME Cross-module configuration (deprecated)
;;

;; I needed a way to reliably cross-configure modules without littering my
;; modules with `after!' blocks or testing whether they were enabled, so I wrote
;; `set!'. If a setting doesn't exist at runtime, the `set!' call is ignored and
;; its arguments are left unevaluated (and entirely omitted when byte-compiled).

(defmacro def-setting! (keyword arglist &optional docstring &rest forms)
  "Define a setting. Like `defmacro', this should return a form to be executed
when called with `set!'. FORMS are not evaluated until `set!' calls it.

See `doom/describe-setting' for a list of available settings.

Do not use this for configuring Doom core."
  (declare (indent defun) (doc-string 3))
  (or (keywordp keyword)
      (signal 'wrong-type-argument (list 'keywordp keyword)))
  (unless (stringp docstring)
    (push docstring forms)
    (setq docstring nil))
  (let ((alias (plist-get forms :obsolete)))
    (when alias
      (setq forms (plist-put forms :obsolete 'nil)))
    `(fset ',(intern (format "doom--set%s" keyword))
           (lambda ,arglist
             ,(if (and (not docstring) (fboundp alias))
                  (documentation alias t)
                docstring)
             ,(when alias
                `(declare (obsolete ,alias "2.1.0")))
             (prog1 (progn ,@forms)
               ,(when alias
                  `(unless noninteractive
                     (message ,(format "The `%s' setting is deprecated, use `%s' instead"
                                       keyword alias)))))))))

(defmacro set! (keyword &rest values)
  "Set an option defined by `def-setting!'. Skip if doesn't exist. See
`doom/describe-setting' for a list of available settings.

VALUES doesn't get evaluated if the KEYWORD setting doesn't exist."
  (declare (indent defun))
  (let ((fn (intern-soft (format "doom--set%s" keyword))))
    (if (and fn (fboundp fn))
        (apply fn values)
      (when (or doom-debug-mode after-init-time)
        (message "No setting found for %s" keyword)
        nil))))

(provide 'core-modules)
;;; core-modules.el ends here
