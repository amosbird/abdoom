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
      browse-url-browser-function 'browse-url-osc)))
  )

(setq recenter-redisplay nil)
(centered-window-mode)
(remove-hook 'kill-emacs-query-functions #'doom-quit-p)

;; may delete the real hyphens
(defadvice fill-delete-newlines (before my-before-fill-delete-newlines)
  "Replace -\\n with an empty string when calling `fill-paragraph'."
  (when (eq this-command 'unfill-paragraph)
    (goto-char (ad-get-arg 0))
    (while (search-forward "-\n" (ad-get-arg 1) t)
      (replace-match "")
      (ad-set-arg 1 (- (ad-get-arg 1) 2)))))
(ad-activate 'fill-delete-newlines)

(setq compilation-finish-function
      (lambda (buf str)
        (if (null (string-match ".*exited abnormally.*" str))
            (delete-windows-on (get-buffer-create "*compilation*")))))

(evil-set-initial-state 'Custom-mode 'normal)

(define-key key-translation-map (kbd "C-\\") (kbd "C-S-s"))
(define-key key-translation-map (kbd "C-^") (kbd "C-,"))
(define-key key-translation-map (kbd "C-_") (kbd "C-."))
(defun ab-package/post-init-ffap ()
  (use-package ffap
    :commands amos-evil-find-file-at-point-with-line
    :config
    (evil-define-command amos-evil-find-file-at-point-with-line ()
      "Opens the file at point and goes to line-number."
      (let ((fname (with-no-warnings (ffap-file-at-point))))
        (if fname
            (let ((line
                   (save-excursion
                     (goto-char (cadr ffap-string-at-point-region))
                     (and (re-search-backward ":\\([0-9]+\\)\\=" (line-beginning-position) t)
                          (string-to-number (match-string 1))))))
              (with-no-warnings (ffap))
              (when line
                (goto-char (point-min))
                (forward-line (1- line))))
          (user-error "File does not exist."))))
    (define-key evil-normal-state-map "gf" 'amos-evil-find-file-at-point-with-line)
    (define-key evil-motion-state-map "gf" 'amos-evil-find-file-at-point-with-line)))

(defun ab-package/init-narrow-reindent ()
  (use-package narrow-reindent
    :config
    (defun narrow-reindent-mode-maybe ()
      (if (not (minibufferp))
          (narrow-reindent-mode 1)))
    (define-global-minor-mode global-narrow-reindent-mode
      narrow-reindent-mode narrow-reindent-mode-maybe
      :group 'narrow-reindent)
    (global-narrow-reindent-mode)))

(defun ab-package/post-init-yasnippet ()
  (use-package yasnippet
    :config
    (add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))))
(defun ab-package/init-emamux ()
  (use-package emamux
    :config
    (global-set-key (kbd "C-x C-c") (lambda () (interactive) (emamux:tmux-run-command nil "detach-client")))
    (defmacro emamux:ensure-ssh-and-cd (&rest body) ,@body)))


(defun ab-package/init-fcitx ()
  (use-package fcitx
    :config
    (fcitx-aggressive-setup)))


(defun ab-package/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :init (setq evil-visual-state-cursor 'box
                evil-insert-state-cursor 'bar
                evil-emacs-state-cursor 'hbar
                etcc-use-color 't)
    :config (evil-terminal-cursor-changer-activate)))

(defun ab-package/init-sdcv ()
  (use-package sdcv
    :commands ab-search-word
    :defer t
    :config
    (defun ab-search-word ()
      (interactive)
      (sdcv-search-pointer+))))


(defun ab-package/init-pangu-spacing ()
  (use-package pangu-spacing
    :config
    (progn
      (global-pangu-spacing-mode 1)
      (spacemacs|hide-lighter pangu-spacing-mode)
      ;; Always insert `real' space in org-mode.
      (add-hook
       'org-mode-hook
       '(lambda ()
          (set (make-local-variable 'pangu-spacing-real-insert-separtor) t))))))

