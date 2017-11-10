;; -*- no-byte-compile: t; -*-
;;; lang/python/packages.el

;; requires: python jedi setuptools

(package! anaconda-mode)
(package! nose)
(package! pytest)
(package! pyenv-mode)
(package! pyvenv)
(package! live-py-mode)
(package! pip-requirements)

(when (featurep! :completion company)
  (package! company-anaconda))
