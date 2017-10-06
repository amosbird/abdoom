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

(defvar +amos-evil-cursors '(("normal" "DarkGoldenrod2" box)
                             ("insert" "chartreuse3" (bar . 2))
                             ("emacs" "SkyBlue2" box)
                             ("replace" "chocolate" (hbar . 2))
                             ("visual" "gray" (hbar . 2))
                             ("motion" "plum3" box)
                             ("lisp" "HotPink1" box)
                             ("iedit" "firebrick1" box)
                             ("iedit-insert" "firebrick1" (bar . 2)))
  "Colors assigned to evil states with cursor definitions.")
(cl-loop for (state color cursor) in +amos-evil-cursors
         do (set (intern (format "evil-%s-state-cursor" state)) (list color cursor)))

(def-hydra! +amos@paste (:hint nil)
  "
         Paste:       p:after        P:before
  Cycle pastes:       C-j:next       C-l:prev
"
  ("C-j" evil-paste-pop)
  ("C-k" evil-paste-pop-next)
  ("p" evil-paste-after)
  ("P" evil-paste-before))

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
  (setq smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (set! :email "gmail.com"
    '((mu4e-sent-folder       . "/gmail.com/Sent Mail")
      (mu4e-drafts-folder     . "/gmail.com/Drafts")
      (mu4e-trash-folder      . "/gmail.com/Trash")
      (mu4e-refile-folder     . "/gmail.com/All Mail")
      (smtpmail-smtp-user     . "amosbird")
      (user-mail-address      . "amosbird@gmail.com")
      (mu4e-compose-signature . "---\nAmos Bird\namosbird@gmail.com"))))

(def-package! osc
  :demand
  :config
  (pcase (system-name)
    ("t450s"
     (defun +amos/other-window ()
       (interactive)
       (i3-nav-right))
     (setq browse-url-browser-function 'browse-url-chrome))
    (_
     (defun +amos/other-window ()
       (interactive)
       (osc-nav-right))
     (setq
      interprogram-cut-function 'osc-select-text
      browse-url-browser-function 'browse-url-osc))))

(def-package! evil-nerd-commenter)

(setq recenter-redisplay nil)
(centered-window-mode +1)
(blink-cursor-mode -1)
(remove-hook 'kill-emacs-query-functions #'doom-quit-p)
(evil-set-initial-state 'Custom-mode 'normal)

;; may delete the real hyphens
(defadvice +amos*fill-delete-newlines (before my-before-fill-delete-newlines)
  "Replace -\\n with an empty string when calling `fill-paragraph'."
  (when (eq this-command 'unfill-paragraph)
    (goto-char (ad-get-arg 0))
    (while (search-forward "-\n" (ad-get-arg 1) t)
      (replace-match "")
      (ad-set-arg 1 (- (ad-get-arg 1) 2)))))
(ad-activate '+amos*fill-delete-newlines)

;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (null (string-match ".*exited abnormally.*" str))
;;             (delete-windows-on (get-buffer-create "*compilation*")))))



;; (define-key key-translation-map (kbd "C-\\") (kbd "C-S-s"))
;; (define-key key-translation-map (kbd "C-^") (kbd "C-,"))
;; (define-key key-translation-map (kbd "C-_") (kbd "C-."))

(def-package! narrow-reindent
  :config
  (defun narrow-reindent-mode-maybe ()
    (if (not (minibufferp))
        (narrow-reindent-mode +1)))
  (define-global-minor-mode global-narrow-reindent-mode
    narrow-reindent-mode narrow-reindent-mode-maybe
    :group 'narrow-reindent)
  (global-narrow-reindent-mode +1))


(def-package! fcitx
  :config
  (fcitx-aggressive-setup))

(def-package! evil-terminal-cursor-changer
  :if (not (display-graphic-p))
  :init (setq evil-visual-state-cursor 'box
              evil-insert-state-cursor 'bar
              evil-emacs-state-cursor 'hbar
              etcc-use-color 't)
  :config (evil-terminal-cursor-changer-activate))

(def-package! sdcv
  :commands ab-search-word
  :init
  (defun ab-search-word ()
    (interactive)
    (sdcv-search-pointer+)))

(def-package! pangu-spacing
  :config
  (global-pangu-spacing-mode +1)
  ;; Always insert `real' space in org-mode.
  (add-hook! org-mode (set (make-local-variable 'pangu-spacing-real-insert-separtor) t)))

(def-package! counsel-dash
  :init
  (setq
   counsel-dash-docsets-path "~/.docset"
   counsel-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"
   counsel-dash-min-length 2
   counsel-dash-candidate-format "%d %n (%t)"
   counsel-dash-enable-debugging nil
   counsel-dash-browser-func 'browse-url
   counsel-dash-ignored-docsets nil)
  :config
  (add-hook! go-mode (setq-local helm-dash-common-docsets '("Go")))
  (add-hook! java-mode (setq-local helm-dash-common-docsets '("Java")))
  (add-hook! rust-mode (setq-local helm-dash-common-docsets '("Rust")))
  (add-hook! c-mode (setq-local helm-dash-common-docsets '("C" "Linux" "glibc")))
  (add-hook! c++-mode (setq-local helm-dash-common-docsets '("C++" "Linux" "glibc")))
  (add-hook! python-mode (setq-local helm-dash-common-docsets '("Python_3" "Python_2")))
  (add-hook! elisp-mode (setq-local helm-dash-common-docsets '("Emacs_Lisp"))))

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

