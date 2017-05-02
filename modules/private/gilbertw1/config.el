;;; private/gilbertw1/config.el

(when (featurep 'evil)
  (load! +bindings))  ; my key bindings

(defvar +gilbertw1-dir
  (file-name-directory load-file-name))

(defvar +gilbertw1-snippets-dir
  (expand-file-name "snippets/" +gilbertw1-dir))

;; additional snippets
(after! yasnippet
  (setq yas-snippet-dirs (append (list '+gilbertw1-snippets-dir) yas-snippet-dirs)))
