;;
;; Packages
;;

(def-package! restclient
  :config
  (defun +restclient|keymap-setup ()
    "Setup eshell keybindings. This must be done in a hook because eshell
    redefines its keys every time `eshell-mode' is enabled."
    (map! :map restclient-mode-map
      (:leader
        (:desc "mode"
         :prefix "m"
         :desc "Send http request"           :n "s" 'restclient-http-send-current
         :desc "Send http request other"     :n "S" 'restclient-http-send-current-stay-in-window
         :desc "Send http request raw"       :n "r" 'restclient-http-send-current-raw
         :desc "Copy curl command"           :n "c" 'restclient-copy-curl-command))))
  (add-hook 'restclient-mode-hook #'+restclient|keymap-setup)
  (add-hook! 'restclient-mode-hook #'nlinum-mode))
