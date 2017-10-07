;;; private/amos/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +amos-dir (file-name-directory load-file-name))
(defvar +amos-snippets-dir (expand-file-name "snippets/" +amos-dir))

(setq epa-file-encrypt-to user-mail-address
      auth-sources (list (expand-file-name ".authinfo.gpg" +amos-dir)))

(defun +amos*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+amos*no-authinfo-for-tramp)

(def-hydra! +amos@paste (:hint nil)
  "Paste"
  ("C-j" evil-paste-pop "Next Paste")
  ("C-k" evil-paste-pop-next "Prev Paste")
  ("p" evil-paste-after "Paste After")
  ("P" evil-paste-before "Paste Before"))

(after! smartparens
  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

(after! evil-mc
  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

;; Don't use default snippets, use mine.
(after! yasnippet
  (add-hook! 'yas-minor-mode-hook (yas-activate-extra-mode 'fundamental-mode))
  (setq yas-snippet-dirs
        (append (list '+amos-snippets-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

;; app/email
(after! mu4e
  '(set! :email "gmail.com"
    ((user-full-name         . "Amos Bird")
     (user-mail-address      . "amosbird@gmail.com")
     (mu4e-compose-signature . "Amos Bird\namosbird@gmail.com"))
    t)

  ;; (defun +amos*mu4e-popup-window (buf _height)
  ;;   (doom-popup-buffer buf '(:size 10 :noselect t :autoclose))
  ;;   buf)
  ;; (advice-add #'mu4e~temp-window :override #'+amos*mu4e-popup-window)
  )

(after! cus-edit (evil-set-initial-state 'Custom-mode 'normal))

(def-package! osc
  :demand
  :init
  (if window-system
    (progn
     (defun +amos/other-window ()
       (interactive)
       (i3-nav-right))
     (setq browse-url-browser-function 'browse-url-chrome))
    (progn
     (defun +amos/other-window ()
       (interactive)
       (osc-nav-right))
     (setq
      interprogram-cut-function 'osc-select-text
      browse-url-browser-function 'browse-url-osc))))

(def-package! evil-nerd-commenter
  :commands
  evilnc-invert-comment-line-by-line
  evilnc-copy-and-comment-lines
  evilnc-comment-or-uncomment-lines)

(setq recenter-redisplay nil)
(remove-hook! 'kill-emacs-query-functions #'doom-quit-p)
(remove-hook! 'doom-post-init-hook #'blink-cursor-mode)
(add-hook! 'doom-post-init-hook (centered-window-mode) (blink-cursor-mode -1) (setq-default truncate-lines nil))

(defun +amos*set-evil-cursors (&rest _)
  (let ((evil-cursors '(("normal" "DarkGoldenrod" box)
                        ("insert" "Chartreuse3" bar)
                        ("emacs" "SkyBlue2" box)
                        ("replace" "DarkOrange3" hbar)
                        ("visual" "Gray" hbar)
                        ("motion" "Plum3" box)
                        ("lisp" "HotPink1" box)
                        ("iedit" "firebrick1" box)
                        ("iedit-insert" "firebrick1" bar))))
    (cl-loop for (state color cursor) in evil-cursors
             do (set (intern (format "evil-%s-state-cursor" state)) (list color cursor)))))
(advice-add #'+evil*init-cursors :override #'+amos*set-evil-cursors)

;; may delete the real hyphens
(defadvice fill-delete-newlines (before my-before-fill-delete-newlines activate)
  "Replace -\\n with an empty string when calling `fill-paragraph'."
  (when (eq this-command 'unfill-paragraph)
    (goto-char (ad-get-arg 0))
    (while (search-forward "-\n" (ad-get-arg 1) t)
      (replace-match "")
      (ad-set-arg 1 (- (ad-get-arg 1) 2)))))

;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (null (string-match ".*exited abnormally.*" str))
;;             (delete-windows-on (get-buffer-create "*compilation*")))))

(def-package! narrow-reindent
  :demand
  :config
  (defun narrow-reindent-mode-maybe ()
    (if (not (minibufferp))
        (narrow-reindent-mode +1)))
  (define-global-minor-mode global-narrow-reindent-mode
    narrow-reindent-mode narrow-reindent-mode-maybe
    :group 'narrow-reindent)
  (global-narrow-reindent-mode +1))

(def-package! fcitx
  :if (eq (system-name) "t450s")
  :config
  (fcitx-aggressive-setup))

(def-package! pangu-spacing
  :demand
  :config
  (global-pangu-spacing-mode +1)
  ;; Always insert `real' space in org-mode.
  (add-hook! org-mode (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

(def-package! git-gutter
  :demand
  :config
  (global-git-gutter-mode +1)
  (advice-add #'git-gutter:set-window-margin :override #'ignore)
  (defun +amos*git-gutter:before-string (sign)
    (let ((gutter-sep (concat (make-string (- (car (window-margins (get-buffer-window))) 2) ? ) sign)))
      (propertize " " 'display `((margin left-margin) ,gutter-sep))))
  (advice-add #'git-gutter:before-string :override #'+amos*git-gutter:before-string)
  (add-hook! 'window-configuration-change-hook #'git-gutter:update-all-windows))

(def-package! evil-textobj-line
  :after evil)

(unless window-system
  (require 'evil-terminal-cursor-changer)
  (etcc-on))

(def-package! chinese-yasdcv
  :commands yasdcv-translate-at-point
  :init
  (custom-set-variables
   '(yasdcv-sdcv-dicts   '(("jianminghy" "简明汉英词典" "powerword2007" t)))
   '(yasdcv-sdcv-command "sdcv --non-interactive --utf8-output --utf8-input \"%word\"")))

(def-package! counsel-dash
  :commands counsel-dash
  :init
  (setq
   counsel-dash-docsets-path "~/.docsets"
   counsel-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"
   counsel-dash-min-length 2
   counsel-dash-candidate-format "%d %n (%t)"
   counsel-dash-enable-debugging nil
   counsel-dash-browser-func 'browse-url
   counsel-dash-ignored-docsets nil)
  (defun counsel-dash-at-point ()
    (interactive)
    (counsel-dash (thing-at-point 'symbol)))
  (add-hook! go-mode (setq-local helm-dash-common-docsets '("Go")))
  (add-hook! java-mode (setq-local helm-dash-common-docsets '("Java")))
  (add-hook! rust-mode (setq-local helm-dash-common-docsets '("Rust")))
  (add-hook! c-mode (setq-local helm-dash-common-docsets '("C" "Linux" "glibc")))
  (add-hook! c++-mode (setq-local helm-dash-common-docsets '("C++" "Linux" "glibc")))
  (add-hook! python-mode (setq-local helm-dash-common-docsets '("Python_3" "Python_2")))
  (add-hook! emacs-lisp-mode (setq-local helm-dash-common-docsets '("Emacs_Lisp"))))

(def-package! easy-hugo
  :commands easy-hugo
  :config
  (evil-set-initial-state 'easy-hugo-mode 'emacs)
  (defun advice-browse-url (ofun &rest candidate)
    (if (boundp 'amos-browse)
        (apply 'browse-url-firefox candidate)
      (apply ofun candidate)))
  (advice-add 'browse-url :around #'advice-browse-url)
  (add-hook! 'easy-hugo-mode-hook (setq-local amos-browse t))
  (setq
   easy-hugo-basedir "~/sites/blog"
   easy-hugo-url "https://wentropy.com"
   easy-hugo-sshdomain "blog"
   easy-hugo-root "/var/www/blog/"
   easy-hugo-previewtime "300"
   easy-hugo-default-ext ".org"))

(def-package! ox-hugo
  :after ox
  :config
  (custom-set-variables '(org-hugo-default-section-directory "post")))

(after! ox
  (nconc org-export-backends '(beamer odt)))

(after! org
  (custom-set-variables
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
   '(org-blank-before-new-entry (quote ((heading . t) (plain-list-item . t))))
   '(org-capture-templates
     (quote
      (("c" "code" entry
        (file+headline "~/org/code.org" "Triage")
        "** %a " :prepend t :empty-lines-before 1 :empty-lines-after 1)
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
        "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1))))
   '(org-confirm-babel-evaluate nil)
   '(org-default-notes-file "/home/amos/org/note.org")
   '(org-emphasis-alist
     (quote
      (("*" bold)
       ("/" italic)
       ("_" underline)
       ("=" org-verbatim verbatim)
       ("~" org-code verbatim)
       ("+" alert-urgent-face))))
   ;; '(org-export-backends (quote (ascii beamer html icalendar latex md odt)))
   '(org-file-apps
     (quote
      ((auto-mode . default)
       ("\\.mm\\'" . default)
       ("\\.x?html?\\'" . "vivaldi new %s")
       ("\\.pdf\\'" . "zathura %s")
       ("\\.odt\\'" . "winopen %s")
       ("\\.docx?\\'" . "winopen %s"))))
   '(org-highlight-latex-and-related (quote (latex)))
   '(org-html-head-extra "<style>.tag {border: 1px solid hotpink;}</style>")
   '(org-html-text-markup-alist
     (quote
      ((bold . "<b>%s</b>")
       (code . "<code>%s</code>")
       (italic . "<i>%s</i>")
       (strike-through . "<strong style=\"color : red;\">%s</strong>")
       (underline . "<span class=\"underline\">%s</span>")
       (verbatim . "<code>%s</code>"))))
   '(org-hugo-default-section-directory "post")
   '(org-image-actual-width (quote (200)))
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
   '(org-preview-latex-default-process (quote imagemagick))
   '(org-refile-targets (quote ((nil :level . 1))))
   '(org-reverse-note-order t)
   '(org-src-block-faces (quote (("c++" default))))
   '(org-src-tab-acts-natively t)
   '(org-startup-folded nil)
   '(org-twbs-text-markup-alist
     (quote
      ((bold . "<b>%s</b>")
       (code . "<code>%s</code>")
       (italic . "<i>%s</i>")
       (strike-through . "<strong style=\"color : red;\">%s</strong>")
       (underline . "<span class=\"underline\">%s</span>")
       (verbatim . "<code>%s</code>")))))

  (custom-set-faces
   '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.0))))
   '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :height 1.0))))
   '(org-level-3 ((t (:foreground "#67b11d" :weight normal :height 1.0)))))
  )

(def-package! org-autolist
  :after org
  :config
  (add-hook! org-mode (org-autolist-mode)))

(after! ox
  ;; remove comments from org document for use with export hook
  ;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
  (defun +amos|delete-org-comments (&optional backend)
    (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
                                                   'comment 'identity))
          do
          (setf (buffer-substring (org-element-property :begin comment)
                                  (org-element-property :end comment)) "")))
  ;; add to export hook
  (add-hook! 'org-export-before-processing-hook #'+amos|delete-org-comments)

  (defun +amos--clear-single-linebreak-in-cjk-string (string)
    "clear single line-break between cjk characters that is usually soft line-breaks"
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
           (start (string-match regexp string)))
      (while start
        (setq string (replace-match "\\1\\2" nil nil string)
              start (string-match regexp string start))))
    string)
  (defun +amos|ox-clear-single-linebreak-for-cjk (string backend info)
    (+amos--clear-single-linebreak-in-cjk-string string))
  (add-to-list 'org-export-filter-final-output-functions
               '+amos|ox-clear-single-linebreak-for-cjk))

(def-package! link-hint
  :commands link-hint-open-link
  :config
  (after! mu4e
    (defun +amos/mu4e-open-all-attachments ()
      "Open all visible mu4e attachments."
      (interactive)
      (let ((link-hint-ignore-types
             (remove 'mu4e-attachment link-hint-all-types))
            link-hint-act-on-all-ignore-types)
        (link-hint-open-all-links)))))

(def-package! lispyville
  :commands lispyville-mode)

(after! swiper
  (custom-set-variables '(swiper-include-line-number-in-search t)))

(def-package! ivy-rich
  :after ivy
  :init
  (setq ivy-virtual-abbreviate 'full
        ivy-use-virtual-buffers t
        ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(def-package! move-text
  :commands move-text-up move-text-down)

;;; config.el ends here
