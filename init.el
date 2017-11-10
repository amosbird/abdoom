;;; private/amos/init.el -*- lexical-binding: t; -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-revert-interval 0.3)
 '(bibtex-completion-browser-function 'browser-url-chromium)
 '(bibtex-completion-pdf-open-function (lambda (fpath) (call-process "zathura" nil 0 nil fpath)))
 '(browse-url-chrome-program (expand-file-name "~/scripts/vivaldi"))
 '(browse-url-firefox-arguments '("new"))
 '(browse-url-firefox-program (expand-file-name "~/scripts/vivaldi"))
 '(browse-url-mailto-function 'mu4e~compose-browse-url-mail)
 '(company-dabbrev-code-everywhere t)
 '(company-dabbrev-code-ignore-case t)
 '(counsel-org-goto-display-style 'path)
 '(counsel-org-goto-face-style 'org)
 '(counsel-org-goto-separator " ➜ ")
 '(dired-open-extensions
   '(("pdf" . "xdg-open")
     ("callgrind.out.*" . "kcachegrind")
     ("doc" . "xdg-open")
     ("docx" . "xdg-open")
     ("xlsx" . "xdg-open")
     ("xls" . "xdg-open")
     ("odt" . "xdg-open")
     ("ppt" . "xdg-open")
     ("mkv" . "xdg-open")
     ("pptx" . "xdg-open")
     ("torrent" . "xdg-open")
     ))
 '(dired-open-find-file-function #'+amos/find-file)
 '(dired-omit-verbose nil)
 '(evil-cjk-emacs-word-boundary t)
 '(evil-esc-delay 0.001)
 '(evil-ex-substitute-global t)
 '(evil-kill-on-visual-paste nil)
 '(evil-shift-round nil)
 '(evil-shift-width 4)
 '(explicit-shell-file-name "/bin/bash")
 '(find-file-visit-truename t)
 '(flycheck-pos-tip-mode nil)
 '(fringes-outside-margins t)
 '(helm-bibtex-bibliography '("~/zotero.bib"))
 '(helm-bibtex-notes-path "~/bibnotes.org")
 '(helm-bibtex-pdf-field "file")
 '(intent-tabs-mode t)
 '(ivy-use-virtual-buffers t)
 '(ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
 '(ivy-rich-abbreviate-paths t)
 '(ivy-rich-switch-buffer-delimiter "|")
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-virtual-abbreviate 'full)
 ;'(nav-flash-use-pulse t)
 '(mode-line-format nil)
 '(nav-flash-delay .3)
 '(org-M-RET-may-split-line '((default)))
 '(org-agenda-files '("~/org/todo.org"))
 '(org-goto-interface 'outline-path-completion)
 '(org-hugo-default-section-directory "post")
 '(org-outline-path-complete-in-steps nil)
 '(org-ref-bibliography-notes "~/Papers/notes.org")
 '(org-ref-default-bibliography '("~/Papers/references.bib"))
 '(org-ref-open-pdf-function (lambda (fpath) (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))
 '(org-ref-pdf-directory "~/Papers/")
 '(org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (dot . t)
     (gnuplot . t)
     (C . t)
     (sql . t)
     (awk . t)))
 '(org-beamer-frame-level 2)
 '(org-beamer-theme "metropolis")
 '(org-capture-templates
   '(("c" "code" entry
      (file+headline "~/org/code.org" "Triage")
      "** %a " :prepend t :empty-lines-before 1 :empty-lines-after 1)
     ("i" "idea" entry
      (file "~/org/idea.org")
      "* %u %?\n%i" :prepend t :empty-lines-before 1 :empty-lines-after 1)
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
      "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1)))
 '(org-html-text-markup-alist
   '((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<strong style=\"color : red;\">%s</strong>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>")))
 '(org-latex-compiler "xelatex")
 '(org-latex-custom-lang-environments nil)
 '(org-latex-default-packages-alist
   '(("AUTO" "inputenc" t
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
     ("" "hyperref" nil)))
 '(org-latex-tables-booktabs t)
 '(org-latex-text-markup-alist
   '((bold . "\\textbf{%s}")
     (code . protectedtexttt)
     (italic . "\\emph{%s}")
     (strike-through . "\\emph{%s}")
     (underline . "\\uline{%s}")
     (verbatim . protectedtexttt)))
 '(org-mime-beautify-quoted-mail t)
 '(org-preview-latex-default-process 'imagemagick)
 '(org-src-block-faces '(("c++" default)))
 '(org-src-tab-acts-natively t)
 '(org-twbs-text-markup-alist
   '((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<strong style=\"color : red;\">%s</strong>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>")))
 '(package-check-signature nil)
 '(password-cache-expiry nil)
 '(powerline-default-separator 'alternate)
 '(process-environment initial-environment)
 '(query-replace-skip-read-only t)
 '(reftex-default-bibliography '("~/zotero.bib"))
 '(recentf-max-saved-items 10000)
 '(require-final-newline t)
 '(shell-file-name "/bin/bash")
 '(show-paren-priority -50)
 '(show-trailing-whitespace t)
 '(sp-escape-quotes-after-insert nil)
 '(swiper-include-line-number-in-search t)
 '(tab-always-indent t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo-files")))
 '(user-full-name "Amos Bird")
 '(user-mail-address "amosbird@gmail.com")
 '(visible-cursor nil)
 '(ws-butler-keep-whitespace-before-point nil)
 '(yas-triggers-in-field nil)
 '(yas-wrap-around-region ?y)
 '(yasdcv-sdcv-command "sdcv --non-interactive --utf8-output --utf8-input \"%word\"")
 '(yasdcv-sdcv-dicts (quote (("jianminghy" "简明汉英词典" "powerword2007" t))))
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-hide ((t (:foreground "black" :height 1.0))))
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.0))))
 '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :height 1.0))))
 '(org-level-3 ((t (:foreground "#67b11d" :weight normal :height 1.0))))
 )

(setq save-interprogram-paste-before-kill t)

(add-hook! '(doom-post-init-hook minibuffer-setup-hook) (setq-local show-trailing-whitespace nil))

(add-hook! 'edebug-mode-hook #'evil-normalize-keymaps)

(add-hook! 'eval-expression-minibuffer-setup-hook
  (define-key minibuffer-local-map "\C-p" #'previous-line-or-history-element)
  (define-key minibuffer-local-map "\C-n" #'next-line-or-history-element))

(setq +org-dir (expand-file-name "~/org/"))
(add-hook! :append 'org-load-hook
  (setq org-agenda-files (directory-files (concat +org-dir "todos/") t "\\.org$" t)))

;; (advice-add #'unicode-fonts-setup :override #'ignore)
(advice-add #'nlinum-mode :override #'ignore)
(advice-add #'fringe-mode :override #'ignore)
(advice-add #'solaire-mode :override #'ignore)
(advice-add #'eldoc-mode :override #'ignore)
(advice-add #'+org|update-cookies :override #'ignore)
(advice-add #'dired-k--highlight-by-file-attribyte :override #'ignore)

(after! centered-window-mode
  (defun amos-special-window-p (window)
    (let* ((buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (equal buffname "*doom*")
          (equal (with-current-buffer buffer major-mode) 'pdf-view-mode))))
  (push #'amos-special-window-p cwm-ignore-window-predicates))

(def-package-hook! evil-anzu
  :pre-init
  (add-transient-hook! #'evil-ex-start-word-search (require 'evil-anzu))
  t)

(def-package-hook! nav-flash
  :pre-init
  (advice-add #'windmove-do-window-select :after #'+doom/blink-cursor)
  (advice-add #'recenter :after #'+doom/blink-cursor)
  (after! evil
    (advice-add #'evil--jumps-jump :after (lambda (&rest _) (recenter)))
    (advice-add #'evil-switch-to-windows-last-buffer :after (lambda (&rest _) (recenter))))
  nil)

(def-package-hook! visual-fill-column :pre-config nil)
(def-package-hook! flycheck-pos-tip :pre-config nil)
(def-package-hook! org-bullets :pre-config nil)
(def-package-hook! magit :pre-config nil)
(def-package-hook! stripe-buffer :pre-init nil)
(def-package-hook! ivy
  :post-config
  (ivy-set-display-transformer #'ivy-switch-buffer nil)
  (ivy-set-display-transformer #'ivy-switch-buffer-other-window nil)
  (ivy-set-display-transformer #'+ivy/switch-workspace-buffer nil)
  t)

(def-package-hook! racer
  :pre-config
  (set! :jump 'rust-mode :definition #'racer-find-definition)
  (unless (file-exists-p racer-cmd)
    (warn "rust-mode: racer binary can't be found; auto-completion is disabled"))
  nil)

(def-package-hook! org-mu4e
  :post-config
    (defadvice org~mu4e-mime-switch-headers-or-body (around +amos*org~mu4e-mime-switch-headers-or-body activate)
      (interactive)
      (let* ((sepapoint
               (save-excursion
                 (goto-char (point-min))
                 (search-forward-regexp mail-header-separator nil t))))
        ;; only do stuff when the sepapoint exist; note that after sending the
        ;; message, this function maybe called on a message with the sepapoint
        ;; stripped. This is why we don't use `message-point-in-header'.
        (when sepapoint
          (cond
            ;; we're in the body, but in mu4e-compose-mode?
            ;; if so, switch to org-mode
            ((and (> (point) sepapoint) (eq major-mode 'mu4e-compose-mode))
              (org-mode)
              (evil-change-to-previous-state)
              (add-hook 'before-save-hook
                (lambda ()
                  (mu4e-error "Switch to mu4e-compose-mode (M-,) before saving."))
                nil t)
              (org~mu4e-mime-decorate-headers)
              (local-set-key (kbd "M-,")
                (lambda (keyseq)
                  (interactive "kEnter mu4e-compose-mode key sequence: ")
                  (let ((func (lookup-key mu4e-compose-mode-map keyseq)))
                    (if func (funcall func) (insert keyseq))))))
            ;; we're in the headers, but in org-mode?
            ;; if so, switch to mu4e-compose-mode
            ((and (<= (point) sepapoint) (eq major-mode 'org-mode))
              (org~mu4e-mime-undecorate-headers)
              (mu4e-compose-mode)
              (evil-change-to-previous-state)
              (add-hook 'message-send-hook 'org~mu4e-mime-convert-to-html-maybe nil t)))
          ;; and add the hook
          (add-hook 'post-command-hook 'org~mu4e-mime-switch-headers-or-body t t))))

    (defun htmlize-and-send ()
      "When in an org-mu4e-compose-org-mode message, htmlize and send it."
      (interactive)
      (when (member 'org~mu4e-mime-switch-headers-or-body post-command-hook)
        (org-mime-htmlize nil)
        (org-mu4e-compose-org-mode)
        (mu4e-compose-mode)
        (message-send-and-exit))))

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


(defvar switch-buffer-functions
  nil
  "A list of functions to be called when the current buffer has been changed.
Each is passed two arguments, the previous buffer and the current buffer.")

(defvar switch-buffer-functions--last-buffer
  nil
  "The last current buffer.")

(defvar switch-buffer-functions--running-p
  nil
  "Non-nil if currently inside of run `switch-buffer-functions-run'.")

(defun switch-buffer-functions-run ()
  "Run `switch-buffer-functions' if needed.
This function checks the result of `current-buffer', and run
`switch-buffer-functions' when it has been changed from
the last buffer.
This function should be hooked to `buffer-list-update-hook'."
  (unless switch-buffer-functions--running-p
    (let ((switch-buffer-functions--running-p t)
          (current (current-buffer))
          (previous switch-buffer-functions--last-buffer))
      (unless (eq previous
                  current)
        (run-hook-with-args 'switch-buffer-functions
                            previous
                            current)
        (setq switch-buffer-functions--last-buffer
              (current-buffer))))))

(add-hook! 'buffer-list-update-hook #'switch-buffer-functions-run)

(defun +amos*dashboard/open (frame)
  (interactive (list (selected-frame)))
  (unless (run-hook-with-args-until-success '+doom-dashboard-inhibit-functions)
    (unless +doom-dashboard-inhibit-refresh
      (with-selected-frame frame
        (switch-to-buffer (doom-fallback-buffer))
        (setq-local show-trailing-whitespace nil)
        (+doom-dashboard-reload)))
    (setq +doom-dashboard-inhibit-refresh nil)))
(advice-add #'+doom-dashboard/open :override #'+amos*dashboard/open)

(require 'server)
(setq server-name (getenv "EMACS_SERVER_NAME"))
(if (not server-name) (setq server-name "server"))
(unless (server-running-p server-name)
  (server-start))

;; fix start-process xdg-open
(setq process-connection-type nil)
