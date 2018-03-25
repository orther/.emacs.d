;; ~/.emacs.d/init.el

;; Set shell to bash to protect against problems caused by fish shell
(setq shell-file-name "/bin/bash")

(require 'core (concat user-emacs-directory "core/core"))
(doom! :config (private +xdg))
