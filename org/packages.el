;; -*- no-byte-compile: t; -*-
;;; org/org/packages.el

;; NOTE This is an insecure source, but unavoidable if we want org 9.0+.
;; orgmode.org offers no secure access to this repo. If this bothers you,
;; comment out this `package!' block and download org-plus-contrib from
;; orgmode.org.
(package! evil-org :recipe (:fetcher github :repo "Somelauw/evil-org-mode"))
(package! org-bullets :recipe (:fetcher github :repo "amosbird/org-bullets"))
(package! toc-org)
(package! ox-hugo)
(package! ox-twbs)
(package! ox-pandoc)
