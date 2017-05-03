;;; private/gilbertw1/config.el

(when (featurep 'evil)
  (load! +bindings))  ; my key bindings

(defvar +gilbertw1-dir
  (file-name-directory load-file-name))
