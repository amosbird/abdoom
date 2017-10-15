;;; private/amos/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +amos-dir (file-name-directory load-file-name))
(defvar +amos-snippets-dir (expand-file-name "snippets/" +amos-dir))

(setq epa-file-encrypt-to user-mail-address
      c-tab-always-indent t
      auth-sources (list (expand-file-name ".authinfo.gpg" +amos-dir)))

(set! :popup "*Org Export Dispatcher*" :noselect t :size 0.5 :align 'right)
(set! :popup "*Stardict Output*" :size 0.6 :autoclose t :noselect t :autofit t)
(set! :popup " *mu4e-verify*" :size 0.1 :autoclose t :noselect t)

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

;; (after! dired
;;   (add-hook! 'dired-after-readin-hook 'hl-line-mode))

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


(after! cus-edit (evil-set-initial-state 'Custom-mode 'normal))

(def-package! evil-magit
  :after magit)

(after! magit
  (setq magit-bury-buffer-function #'magit-restore-window-configuration)
  (setq magit-display-buffer-function #'magit-display-buffer-traditional)

  (set! :popup "^\\*magit" :regexp t :align 'right :size 0.5 :noesc t :autokill t)
  (set! :popup 'magit-status-mode :select t :same t)
  (set! :popup 'magit-log-mode :select t :inhibit-window-quit t :same t)
  (set! :popup "COMMITMSG" :same t)
  (set! :popup "\\`\\*magit-diff: .*?\\'" :regexp t :noselect t :align 'left :size 0.5)
  )

(def-package! osc
  :demand
  :init
  (add-hook! 'after-make-frame-functions
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
         browse-url-browser-function 'browse-url-osc)))))

(defun +amos|init-fonts (&optional frame)
  (when (display-graphic-p frame)
    (select-frame frame)
    (require 'unicode-fonts)
    (unicode-fonts-setup)
    (remove-hook! 'after-make-frame-functions #'+amos|init-fonts)))

(add-hook! 'after-init-hook
  (if initial-window-system
      (+amos|init-fonts)
    (add-hook! 'after-make-frame-functions #'+amos|init-fonts)))

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

(when (equal (system-name) "t450s")
  (require 'fcitx)
  (fcitx-aggressive-setup))

(def-package! pangu-spacing
  :demand
  :config
  (push 'dired-mode pangu-spacing-inhibit-mode-alist)
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
  :commands yasdcv-translate-at-point)

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

(after! ivy
  (setq ivy-virtual-abbreviate 'full
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-use-virtual-buffers t))

;; (def-package! ivy-rich
;;   :after ivy
;;   :init
;;   (setq ivy-virtual-abbreviate 'full
;;         ivy-re-builders-alist '((t . ivy--regex-ignore-order))
;;         ivy-use-virtual-buffers t
;;         ivy-rich-switch-buffer-align-virtual-buffer t)
;;   (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(def-package! move-text
  :commands move-text-up move-text-down)

(def-package! fish-mode
  :mode "\\.fish")

(defun dired-list-exa (dir)
  "List all files in DIR managed by git and display results as a `dired' buffer."
  (interactive "DDirectory: ")
  (dired-list dir
              (concat "exa -alg --git " dir)
              (concat "exa -alg --git " dir)
              `(lambda (ignore-auto noconfirm) (dired-list-exa ,dir))))

(def-package! ws-butler
  :demand
  :config
  (ws-butler-global-mode))
;;; config.el ends here

(setq split-width-threshold nil)
(setq split-height-threshold 0)
(advice-add #'split-window-below :override #'split-window-right)

(setq company-idle-delay 0.1
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-show-numbers t
      company-require-match 'never
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-backends '(company-jedi company-nxml
                                      company-css company-capf
                                      (company-dabbrev-code company-keywords)
                                      company-files company-dabbrev)
      company-jedi-python-bin "python")
(defun my-company-abort ()
  (interactive)
  (company-abort)
  (when (and (bound-and-true-p evil-mode)
             (eq evil-state 'insert))
    (evil-force-normal-state)))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<escape>") 'my-company-abort)
  (define-key company-search-map (kbd "<escape>") 'company-search-abort))

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-echo-metadata-frontend
        company-preview-frontend)
      company-auto-complete t)

(with-eval-after-load 'smartparens
  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'minibuffer-inactive-mode "`" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (sp-local-pair 'emacs-lisp-mode "`" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
  (sp-local-pair 'lisp-interaction-mode "`" nil :actions nil)
  (sp-local-pair 'scheme-mode "'" nil :actions nil)
  (sp-local-pair 'scheme-mode "`" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "'" nil :actions nil)
  (sp-local-pair 'inferior-scheme-mode "`" nil :actions nil)

  (sp-local-pair 'LaTeX-mode "\"" nil :actions nil)
  (sp-local-pair 'LaTeX-mode "'" nil :actions nil)
  (sp-local-pair 'LaTeX-mode "`" nil :actions nil)
  (sp-local-pair 'latex-mode "\"" nil :actions nil)
  (sp-local-pair 'latex-mode "'" nil :actions nil)
  (sp-local-pair 'latex-mode "`" nil :actions nil)
  (sp-local-pair 'TeX-mode "\"" nil :actions nil)
  (sp-local-pair 'TeX-mode "'" nil :actions nil)
  (sp-local-pair 'TeX-mode "`" nil :actions nil)
  (sp-local-pair 'tex-mode "\"" nil :actions nil)
  (sp-local-pair 'tex-mode "'" nil :actions nil)
  (sp-local-pair 'tex-mode "`" nil :actions nil))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(with-eval-after-load 'smartparens
  (sp-local-pair 'c-mode "{" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'java-mode "{" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET"))))

(setq sp-message-width nil)
(setq sp-show-pair-from-inside t)
(setq sp-autoescape-string-quote nil)
(setq sp-cancel-autoskip-on-backward-movement nil)

(define-key emacs-lisp-mode-map (kbd "C-x e") 'macrostep-expand)

(add-hook 'visual-line-mode-hook 'visual-fill-column-mode)

(defadvice edebug-pop-to-buffer (around +amos*edebug-pop-to-buffer activate)
  (flet ((old-split-window (&optional window size side pixelwise) (split-window window size side pixelwise))
         (split-window (window) (old-split-window window nit 'right)))
    ad-do-it))

(after! shackle
  (setq shackle-rules
        '(
          ("*Help*" :size 0.3)
          )))

(defadvice hl-line-mode (after +amos*hl-line-mode activate)
  (set-face-background hl-line-face "Gray13"))

(after! hl-line
  (global-hl-line-mode +1))

;; (set-face-background 'show-paren-match (face-background 'default))
;; (set-face-foreground 'show-paren-match (face-foreground 'default))
;; (set-face-attribute 'show-paren-match nil :weight 'normal)
