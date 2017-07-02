;; -*- no-byte-compile: t; -*-
;;; lang/borg/packages.el

(package! org-plus-contrib :recipe (:fetcher git :url "http://orgmode.org/org-mode.git"))

(package! org-download)
(package! org-bullets :recipe (:fetcher github :repo "hlissner/org-bullets"))
(package! toc-org)
(package! ob-go)
(package! ob-restclient)
(package! ob-rust :recipe (:fetcher github :repo "zweifisch/ob-rust"))
(package! ob-sql-mode)
(package! ob-translate)

