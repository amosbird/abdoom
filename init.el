;;; private/amos/init.el -*- lexical-binding: t; -*-
(setq-default
 evil-ex-substitute-global t
 intent-tabs-mode t
 tab-always-indent t
 powerline-default-separator 'alternate
 find-file-visit-truename t
 require-final-newline t
 evil-cjk-emacs-word-boundary t
 evil-shift-width 4
 evil-shift-round nil
 evil-esc-delay 0.001
 visible-cursor nil
 package-check-signature nil
 undo-tree-auto-save-history t
 undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo-files"))
 password-cache-expiry nil
 user-mail-address "amosbird@gmail.com"
 user-full-name "Amos Bird"
 process-environment initial-environment
 browse-url-chrome-program (expand-file-name "~/scripts/vivaldi")
 browse-url-firefox-program (expand-file-name "~/scripts/vivaldi")
 browse-url-firefox-arguments '("new")
 browse-url-mailto-function 'mu4e~compose-browse-url-mail
 sp-escape-quotes-after-insert nil
 reftex-default-bibliography '("~/zotero.bib")
 helm-bibtex-bibliography '("~/zotero.bib")
 helm-bibtex-pdf-field "file"
 helm-bibtex-notes-path "~/bibnotes.org"
 bibtex-completion-browser-function 'browser-url-chromium
 bibtex-completion-pdf-open-function
 (lambda (fpath)
   (call-process "zathura" nil 0 nil fpath))
 shell-file-name "/bin/bash"
 explicit-shell-file-name "/bin/bash"
 org-ref-default-bibliography '("~/Papers/references.bib")
 org-ref-pdf-directory "~/Papers/"
 org-ref-bibliography-notes "~/Papers/notes.org"
 org-ref-open-pdf-function
 (lambda (fpath)
   (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)
(setq +org-dir (expand-file-name "~/org/"))

(add-hook! :append 'org-load-hook (setq org-agenda-files (directory-files (concat +org-dir "todos/") t "\\.org$" t)))


(advice-add #'nlinum-mode :override #'ignore)
(advice-add #'eldoc-mode :override #'ignore)
(fset 'fringe-mode nil)

(def-package-hook! centered-window-mode
  :post-config
  (defun amos-special-window-p (window)
    (let* ((buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (equal buffname "*doom*")
          (equal (with-current-buffer buffer major-mode) 'pdf-view-mode))))
  (push #'amos-special-window-p cwm-ignore-window-predicates)
  t)

(def-package-hook! org-bullets
  :pre-config nil)

(def-package-hook! stripe-buffer
  :pre-init nil)

(set! :popup "*Stardict Output*" :size 0.6 :autoclose t :noselect t :autofit t)
(set! :popup " *mu4e-verify*" :size 0.4 :autoclose t :noselect t :autofit t)

;; host-specific settings
(pcase (system-name)
  ("t450s"
   ;; smaller screen, smaller fonts
   (set! :font "Ubuntu Mono" :size 14)
   (set! :variable-font "Fira Sans" :size 14)
   (set! :unicode-font "DejaVu Sans Mono" :size 14)
   (setq +doom-modeline-height 25)
   (setq helm-dash-browser-func 'browse-url-firefox))
  (_
   ;; smaller screen, smaller fonts
   (set! :font "Fira Mono" :size 10)
   (set! :variable-font "Fira Sans" :size 10)
   (set! :unicode-font "DejaVu Sans Mono" :size 10)
   (setq +doom-modeline-height 25))
  )


