;;; tools/dash/config.el -*- lexical-binding: t; -*-

(def-package! counsel-dash
  :after counsel
  :config
  (setq counsel-dash-docsets-path (concat doom-cache-dir "docsets"))
  ;; Language Hooks
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq-local counsel-dash-docsets '("Emacs Lisp"))))
  (add-hook 'scala-mode-hook (lambda () (setq-local counsel-dash-docsets '("Scala" "Akka" "Play_Scala" "Java"))))
  (add-hook 'java-mode-hook (lambda () (setq-local counsel-dash-docsets '("Java" "Play_Java"))))
  (add-hook 'rust-mode-hook (lambda () (setq-local counsel-dash-docsets '("Rust"))))
  (add-hook 'clojure-mode-hook (lambda () (setq-local counsel-dash-docsets '("Clojure"))))
  (add-hook 'haskell-mode-hook (lambda () (setq-local counsel-dash-docsets '("Haskell"))))
  (add-hook 'sh-mode-hook (lambda () (setq-local counsel-dash-docsets '("Bash"))))
  (add-hook 'c-mode-hook (lambda () (setq-local counsel-dash-docsets '("C"))))
  (add-hook 'c++-mode-hook (lambda () (setq-local counsel-dash-docsets '("C++"))))
  (add-hook 'js2-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript"))))
  (add-hook 'js-mode-hook (lambda () (setq-local counsel-dash-docsets '("Javascript"))))
  (add-hook 'html-mode-hook (lambda () (setq-local counsel-dash-docsets '("HTML" "Javascript"))))
  (add-hook 'python-mode-hook (lambda () (setq-local counsel-dash-docsets '("Python_3")))))

