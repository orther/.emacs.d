;;; lang/rust/config.el -*- lexical-binding: t; -*-

(defvar +rust-src-dir (concat doom-etc-dir "rust/")
  "The path to Rust source library. Required by racer.")


;;
;; Plugins
;;

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (def-menu! +rust/build-menu
    "TODO"
    '(("run"   :exec "cargo run"   :cwd t :when (+rust-cargo-project-p))
      ("build" :exec "cargo build" :cwd t :when (+rust-cargo-project-p)))
    :prompt "Cargo: "))


(def-package! racer
  :after rust-mode
  :preface
  :init
  (add-hook! 'rust-mode-hook #'(racer-mode eldoc-mode flycheck-rust-setup))
  :config
  (setq racer-cmd (expand-file-name "racer/target/release/racer" +rust-src-dir)
        racer-rust-src-path (expand-file-name "rust/src/" +rust-src-dir))

  (set! :jump 'rust-mode :definition #'racer-find-definition)

  (unless (file-exists-p racer-cmd)
    (warn "rust-mode: racer binary can't be found; auto-completion is disabled")))


(def-package! company-racer
  :when (featurep! :completion company)
  :after racer
  :config (set! :company-backend 'rust-mode '(company-racer)))


(def-package! flycheck-rust
  :when (featurep! :feature syntax-checker)
  :after rust-mode
  :config (add-hook 'rust-mode-hook #'flycheck-mode))

