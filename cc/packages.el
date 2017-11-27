;; -*- no-byte-compile: t; -*-
;;; lang/cc/packages.el

(package! cmake-ide)
(package! irony)
(package! flycheck-irony)
(package! company-irony)
(package! company-irony-c-headers)
(package! cmake-mode)
(package! cuda-mode)
(package! demangle-mode)
(package! disaster)
(package! glsl-mode)
(package! clang-format)
(package! opencl-mode)
(package! modern-cpp-font-lock)
(package! company-glsl :recipe (:fetcher github :repo "Kaali/company-glsl"))
(package! lsp-mode)
(package! company-lsp)

;; (package! rtags)
;; (package! company-rtags)
;; (package! ivy-rtags)
;; (package! flycheck-rtags)
