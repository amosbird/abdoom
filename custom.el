;;; ../.local/@lab/etc/custom.el -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.3)
 '(bibtex-completion-browser-function (quote browser-url-chromium))
 '(bibtex-completion-pdf-open-function (lambda (fpath) (call-process "zathura" nil 0 nil fpath)))
 '(browse-url-chrome-program (expand-file-name "~/scripts/vivaldi"))
 '(browse-url-firefox-arguments (quote ("new")))
 '(browse-url-firefox-program (expand-file-name "~/scripts/vivaldi"))
 '(browse-url-mailto-function (quote mu4e~compose-browse-url-mail))
 '(company-dabbrev-code-everywhere t)
 '(company-dabbrev-code-ignore-case t)
 '(counsel-org-goto-display-style (quote path))
 '(counsel-org-goto-face-style (quote org))
 '(counsel-org-goto-separator " ➜ ")
 '(counsel-org-headline-display-style (quote path))
 '(counsel-org-headline-path-separator " ➜ ")
 '(dired-omit-verbose nil)
 '(dired-open-extensions
   (quote
    (("pdf" . "xdg-open")
     ("ps" . "xdg-open")
     ("jpg" . "xdg-open")
     ("iso" . "xdg-open")
     ("callgrind.out.*" . "kcachegrind")
     ("doc" . "xdg-open")
     ("docx" . "xdg-open")
     ("html" . "xdg-open")
     ("xlsx" . "xdg-open")
     ("xls" . "xdg-open")
     ("odt" . "xdg-open")
     ("ppt" . "xdg-open")
     ("mkv" . "xdg-open")
     ("pptx" . "xdg-open")
     ("torrent" . "xdg-open"))))
 '(dired-open-find-file-function (function +amos/find-file))
 '(evil-cjk-emacs-word-boundary t)
 '(evil-esc-delay 0.001)
 '(evil-ex-substitute-global t)
 '(evil-kill-on-visual-paste nil)
 '(evil-shift-round nil)
 '(evil-shift-width 4)
 '(explicit-shell-file-name "/bin/bash")
 '(find-file-visit-truename t)
 '(flycheck-pos-tip-mode nil)
 '(fringes-outside-margins t t)
 '(helm-bibtex-bibliography (quote ("~/zotero.bib")))
 '(helm-bibtex-notes-path "~/bibnotes.org")
 '(helm-bibtex-pdf-field "file")
 '(intent-tabs-mode t)
 '(ivy-re-builders-alist (quote ((t . ivy--regex-ignore-order))) t)
 '(ivy-rich-abbreviate-paths t)
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-rich-switch-buffer-delimiter "|")
 '(ivy-use-virtual-buffers t)
 '(ivy-virtual-abbreviate (quote full))
 '(mode-line-format nil)
 '(nav-flash-delay 0.3)
 '(org-M-RET-may-split-line (quote ((default))))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(org-babel-load-languages
   (quote
    ((python . t)
     (emacs-lisp . t)
     (dot . t)
     (gnuplot . t)
     (C . t)
     (sql . t)
     (awk . t))))
 '(org-beamer-frame-level 2)
 '(org-beamer-theme "metropolis")
 '(org-capture-templates
   (quote
    (("c" "code" entry
      (file+headline "~/org/code.org" "Triage")
      "** %a " :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("i" "idea" entry
      (file "~/org/idea.org")
      "* %u %?
%i" :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("n" "notes" entry
      (file "~/org/notes.org")
      (file "~/org/template/idea")
      :empty-lines-before 1 :empty-lines-after 1)
     ("t" "Templates for todo items")
     ("te" "ergonomics" entry
      (file+headline "~/org/todo.org" "Ergonomics")
      "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("tw" "working" entry
      (file+headline "~/org/todo.org" "Work")
      "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("tl" "learning" entry
      (file+headline "~/org/todo.org" "Learning")
      "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1))) t)
 '(org-goto-interface (quote outline-path-completion))
 '(org-html-text-markup-alist
   (quote
    ((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<strong style=\"color : red;\">%s</strong>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>"))))
 '(org-hugo-default-section-directory "post")
 '(org-latex-compiler "xelatex")
 '(org-latex-custom-lang-environments nil)
 '(org-latex-default-packages-alist
   (quote
    (("AUTO" "inputenc" t
      ("pdflatex"))
     ("T1" "fontenc" t
      ("pdflatex"))
     ("" "graphicx" t)
     ("" "ctex" t)
     ("" "booktabs" t)
     ("" "grffile" t)
     ("" "longtable" nil)
     ("" "wrapfig" nil)
     ("" "rotating" nil)
     ("normalem" "ulem" t)
     ("" "amsmath" t)
     ("" "textcomp" t)
     ("" "amssymb" t)
     ("" "capt-of" nil)
     ("" "hyperref" nil))))
 '(org-latex-tables-booktabs t)
 '(org-latex-text-markup-alist
   (quote
    ((bold . "\\textbf{%s}")
     (code . protectedtexttt)
     (italic . "\\emph{%s}")
     (strike-through . "\\emph{%s}")
     (underline . "\\uline{%s}")
     (verbatim . protectedtexttt))))
 '(org-mime-beautify-quoted-mail t)
 '(org-outline-path-complete-in-steps nil)
 '(org-preview-latex-default-process (quote imagemagick))
 '(org-ref-bibliography-notes "~/Papers/notes.org")
 '(org-ref-default-bibliography (quote ("~/Papers/references.bib")))
 '(org-ref-open-pdf-function
   (lambda
     (fpath)
     (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))
 '(org-ref-pdf-directory "~/Papers/")
 '(org-src-block-faces (quote (("c++" default))))
 '(org-src-tab-acts-natively t)
 '(org-twbs-text-markup-alist
   (quote
    ((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<strong style=\"color : red;\">%s</strong>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>"))))
 '(package-check-signature nil)
 '(password-cache-expiry nil)
 '(powerline-default-separator (quote alternate))
 '(process-environment initial-environment t)
 '(query-replace-skip-read-only t)
 '(recentf-max-saved-items 10000)
 '(reftex-default-bibliography (quote ("~/zotero.bib")) t)
 '(require-final-newline t)
 '(safe-local-variable-values
   (quote
    ((cmake-ide-build-dir . "/home/amos/git/htop-vim")
     (cmake-ide-project-dir . "/home/amos/git/htop-vim"))))
 '(shell-file-name "/bin/bash")
 '(show-paren-priority -50)
 '(show-trailing-whitespace t)
 '(sp-escape-quotes-after-insert nil)
 '(swiper-include-line-number-in-search t)
 '(tab-always-indent t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.emacs.d/undo-files"))))
 '(user-full-name "Amos Bird")
 '(user-mail-address "amosbird@gmail.com")
 '(visible-cursor nil)
 '(ws-butler-keep-whitespace-before-point nil)
 '(yas-triggers-in-field nil)
 '(yas-wrap-around-region 121)
 '(yasdcv-sdcv-command
   "sdcv --non-interactive --utf8-output --utf8-input \"%word\"")
 '(yasdcv-sdcv-dicts (quote (("jianminghy" "简明汉英词典" "powerword2007" t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t (:foreground "black" :height 1.0))))
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.0))))
 '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :height 1.0))))
 '(org-level-3 ((t (:foreground "#67b11d" :weight normal :height 1.0)))))
