;;; private/amos/config.el -*- lexical-binding: t; -*-

(when (featurep! :feature evil)
  (load! +bindings)  ; my key bindings
  (load! +commands)) ; my custom ex commands

(defvar +amos-dir (file-name-directory load-file-name))
(defvar +amos-snippets-dir (expand-file-name "snippets/" +amos-dir))

(setq epa-file-encrypt-to user-mail-address
      c-tab-always-indent t
      auth-sources (list (expand-file-name ".authinfo.gpg" +amos-dir)))

(defun +amos*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+amos*no-authinfo-for-tramp)

(defun special-indent-fn (pos state)
  (save-excursion
    (search-backward ":hint")
    (current-column)))
(put :hint 'lisp-indent-function 'special-indent-fn)
(def-hydra! +amos@paste (:hint nil
                          :foreign-keys nil
                          :pre (setq hydra-lv nil)
                          :after-exit (setq hydra-lv t))
  "Paste"
  ("C-j" evil-paste-pop "Next Paste")
  ("C-k" evil-paste-pop-next "Prev Paste")
  ("p" evil-paste-after "Paste After")
  ("P" evil-paste-before "Paste Before"))

(after! dired-x
  (setq dired-omit-files
        (concat dired-omit-files "\\|\\.directory$")))

(after! dired
  (add-hook! 'dired-mode-hook
    (let ((inhibit-message t))
      (toggle-truncate-lines +1)
      (dired-omit-mode)
      (+amos-store-jump-history))))

(define-advice dired-revert (:after (&rest _) +amos*dired-revert)
  "Call `recenter' after `dired-revert'."
  (condition-case nil
      (recenter)
    (error nil)))

(define-advice +jump-to (:after (&rest _) +amos*jump-to)
  "Call `recenter' after `+jump-to'."
  (condition-case nil
      (recenter)
    (error nil)))

(after! evil-multiedit
  (setq evil-multiedit-follow-matches t))

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

(defun col-at-point (point)
  (save-excursion (goto-char point) (current-column)))

(defun evil--mc-make-cursor-at-col-append (_startcol endcol orig-line)
  (end-of-line)
  (when (> endcol (current-column))
    (insert-char ?\s (- endcol (current-column))))
  (move-to-column endcol)
  (unless (= (line-number-at-pos) orig-line)
    (evil-mc-make-cursor-here)))

(defun evil--mc-make-cursor-at-col-insert (startcol _endcol orig-line)
  (end-of-line)
  (move-to-column startcol)
  (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
    (evil-mc-make-cursor-here)))

(defun evil--mc-make-vertical-cursors (beg end func)
  (evil-mc-pause-cursors)
  (apply-on-rectangle func
                      beg end (line-number-at-pos (point)))
  (evil-mc-resume-cursors)
  (evil-insert-state))

(defun evil-mc-insert-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-insert)
  (move-to-column (min (col-at-point beg) (col-at-point end))))

(defun evil-mc-append-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (when (and (evil-visual-state-p)
             (eq (evil-visual-type) 'line))
    (message "good")
    (let ((column (max (evil-column evil-visual-beginning)
                       (evil-column evil-visual-end))))
      (evil-visual-rotate 'upper-left)
      (move-to-column column t))
    )
  (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-append)
  (move-to-column (max (col-at-point beg) (col-at-point end))))

(after! evil-mc
  (nconc evil-mc-known-commands
         '((+amos:evil-delete-backward-symbol . ((:default . evil-mc-execute-default-call)))
           (+amos:evil-delete-word . ((:default . evil-mc-execute-default-call)))
           (+amos/replace-last-sexp . ((:default . evil-mc-execute-default-call)))
           (+amos:evil-backward-symbol-begin . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))))

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
(after! wdired (evil-set-initial-state 'wdired-mode 'normal))
(after! ivy (evil-set-initial-state 'ivy-occur-grep-mode 'normal))
(after! compile (evil-set-initial-state 'compilation-mode 'normal))

(def-package! evil-magit
  :after magit
  :config
  (def-hydra! +amos@paste (:hint nil)
    "Paste"
    ("0" evil-digit-argument-or-evil-beginning-of-line "bol" :exit t)
    ("C-j" evil-paste-pop "Next Paste")
    ("C-k" evil-paste-pop-next "Prev Paste")
    ("p" evil-paste-after "Paste After")
    ("P" evil-paste-before "Paste Before"))

  (def-hydra! +amos@git-blame (:hint nil
                                :title "Git Blame Transient State"
                                :doc "
Press [_b_] again to blame further in the history, [_q_] to go up or quit."
                                :on-enter (let (golden-ratio-mode)
                                            (unless (bound-and-true-p magit-blame-mode)
                                              (call-interactively 'magit-blame)))
                                :foreign-keys run)
    ("b" magit-blame)
    ;; here we use the :exit keyword because we should exit the
    ;; micro-state only if the magit-blame-quit effectively disable
    ;; the magit-blame mode.
    ("q" nil :exit (progn (when (bound-and-true-p magit-blame-mode)
                            (magit-blame-quit))
                          (not (bound-and-true-p magit-blame-mode)))))

  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1)
  (setq magit-display-buffer-noselect t)
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (add-hook! 'magit-mode-hook (evil-vimish-fold-mode -1))
  (add-hook! 'git-commit-mode-hook 'fci-mode))

(defun +amos|init-frame (&optional frame)
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      ;; (dolist (ft (fontset-list))
      ;;   (set-fontset-font ft 'unicode (font-spec :name "Ubuntu Mono"))
      ;;   (set-fontset-font ft 'unicode (font-spec :name "Symbola monospacified for Ubuntu Mono") nil 'append))
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font t charset
                          (font-spec :family "WenQuanYi Micro Hei" :size 18)))
      (remove-hook! 'after-make-frame-functions #'+amos|init-frame))))

(add-hook! 'after-init-hook
  (if initial-window-system
      (+amos|init-frame)
    (add-hook! 'after-make-frame-functions #'+amos|init-frame)))

(use-package osc
  :demand
  :init
  (defun +amos/other-window ()
    (interactive)
    (if (display-graphic-p)
        (i3-nav-right)
      (osc-nav-right)))
  (setq interprogram-cut-function 'osc-select-text
        browse-url-browser-function (lambda (url &optional _new-window)
                                      (if (display-graphic-p)
                                          (if _new-window
                                              (browse-url-chrome url)
                                            (browse-url-firefox url))
                                        (browse-url-osc url _new-window)))))

(def-package! evil-nerd-commenter
  :commands
  evilnc-invert-comment-line-by-line
  evilnc-copy-and-comment-lines
  evilnc-comment-or-uncomment-lines)

(def-package! realign-mode
  :commands realign-mode realign-windows
  :config
  (defun amos-special-window-p (window)
    (let* ((buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (equal buffname "*doom*")
          (equal (with-current-buffer buffer major-mode) 'pdf-view-mode))))
  (push #'amos-special-window-p realign-ignore-window-predicates))

(setq recenter-redisplay nil)
(remove-hook! 'kill-emacs-query-functions #'doom-quit-p)
(remove-hook! 'doom-post-init-hook #'blink-cursor-mode)
;; (remove-hook! 'doom-init-ui-hook #'show-paren-mode)
(add-hook! 'doom-post-init-hook (realign-mode) (blink-cursor-mode -1) (setq-default truncate-lines nil) )

(defun +amos*set-evil-cursors (&rest _)
  (let ((evil-cursors '(("normal" "#b8860b" box)
                        ("insert" "#66cd00" bar)
                        ("emacs" "#7ec0ee" box)
                        ("replace" "#cd6600" hbar)
                        ("visual" "#808080" hbar)
                        ("motion" "#cd96cd" box)
                        ("lisp" "#ff6eb4" box)
                        ("iedit" "#ff3030" box)
                        ("iedit-insert" "#ff3030" bar))))
    (cl-loop for (state color cursor) in evil-cursors
             do (set (intern (format "evil-%s-state-cursor" state)) (list color cursor)))))
(advice-add #'+evil*init-cursors :override #'+amos*set-evil-cursors)

;; may delete the real hyphens
(defadvice fill-delete-newlines (before *amos+fill-delete-newlines activate)
  "Replace -\\n with an empty string when calling `fill-paragraph'."
  (when (eq this-command 'unfill-paragraph)
    (goto-char (ad-get-arg 0))
    (while (search-forward "-\n" (ad-get-arg 1) t)
      (replace-match "")
      (ad-set-arg 1 (- (ad-get-arg 1) 2)))))

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

;; (def-package! pangu-spacing
;;   :demand
;;   :config
;;   (push 'dired-mode pangu-spacing-inhibit-mode-alist)
;;   ;; Always insert `real' space in org-mode.
;;   (add-hook! org-mode (set (make-local-variable 'pangu-spacing-real-insert-separtor) t) (pangu-spacing-mode +1)))

(def-package! git-gutter
  :demand
  :config
  (defface +amos:modified
    '((t (:foreground "chocolate" :weight bold :inherit default)))
    "Face of modified")

  (defface +amos:added
    '((t (:foreground "ForestGreen" :weight bold :inherit default)))
    "Face of added")

  (defface +amos:deleted
    '((t (:foreground "DarkRed" :weight bold :inherit default)))
    "Face of deleted")

  (global-git-gutter-mode +1)
  (advice-add #'git-gutter:set-window-margin :override #'ignore)
  (defun +amos*git-gutter:before-string (sign)
    (let* ((gutter-sep (concat " " (make-string (- (car (window-margins (get-buffer-window))) 2) ? ) sign))
           (face (pcase sign
                   ("=" '+amos:modified)
                   ("+" '+amos:added)
                   ("-" '+amos:deleted)))
           (ovstring (propertize gutter-sep 'face face)))
      (propertize " " 'display `((margin left-margin) ,ovstring))))
  (advice-add #'git-gutter:before-string :override #'+amos*git-gutter:before-string)
  (add-hook! 'window-configuration-change-hook #'git-gutter:update-all-windows))

(def-package! evil-textobj-line
  :after evil)

(unless window-system
  (require 'evil-terminal-cursor-changer)
  (xterm-mouse-mode +1)
  ;; enable terminal scroll
  (global-set-key (kbd "<mouse-6>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-column-left 3)))
  (global-set-key (kbd "<mouse-7>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-column-right 3)))
  (global-set-key (kbd "<mouse-4>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-line-up 3)))
  (global-set-key (kbd "<mouse-5>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-line-down 3)))
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
   counsel-dash-enable-debugging t
   counsel-dash-browser-func (lambda (url) (interactive) (browse-url url t))
   counsel-dash-ignored-docsets nil)
  (defun counsel-dash-at-point ()
    (interactive)
    (counsel-dash (thing-at-point 'symbol)))
  (add-hook! fish-mode (setq-local helm-dash-docsets '("fish" "Linux_Man_Pages")))
  (add-hook! sh-mode (setq-local helm-dash-docsets '("Bash" "Linux_Man_Pages")))
  (add-hook! go-mode (setq-local helm-dash-docsets '("Go")))
  (add-hook! cmake-mode (setq-local helm-dash-docsets '("CMake")))
  (add-hook! java-mode (setq-local helm-dash-docsets '("Java")))
  (add-hook! rust-mode (setq-local helm-dash-docsets '("Rust")))
  (add-hook! lua-mode (setq-local helm-dash-docsets '("Lua_5.1")))
  (add-hook! c-mode (setq-local helm-dash-docsets '("C")))
  (add-hook! c++-mode (setq-local helm-dash-docsets '("C++" "Boost")))
  (add-hook! python-mode (setq-local helm-dash-docsets '("Python_3" "Python_2")))
  (add-hook! emacs-lisp-mode (setq-local helm-dash-docsets '("Emacs_Lisp"))))

(defun advice-browse-url (ofun &rest candidate)
  (if (boundp 'amos-browse)
      (apply 'browse-url-firefox candidate)
    (apply ofun candidate)))
(advice-add 'browse-url :around #'advice-browse-url)

(defadvice run-skewer (around +amos*run-skewer activate)
  (setq-local amos-browse t)
  ad-do-it)

(after! ediff
  (add-hook! 'ediff-keymap-setup-hook
    (define-key ediff-mode-map "k" 'ediff-previous-difference)
    (define-key ediff-mode-map "j" 'ediff-next-difference)))

(def-package! easy-hugo
  :commands easy-hugo
  :config
  (evil-set-initial-state 'easy-hugo-mode 'emacs)
  (add-hook! 'easy-hugo-mode-hook (setq-local amos-browse t))
  (setq
   easy-hugo-basedir "~/sites/blog"
   easy-hugo-url "https://wentropy.com"
   easy-hugo-sshdomain "blog"
   easy-hugo-root "/var/www/blog/"
   easy-hugo-previewtime "300"
   easy-hugo-default-ext ".org"))


(def-package! link-hint
  :commands link-hint-open-link link-hint-open-all-links
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

(def-package! move-text
  :commands move-text-up move-text-down)

(def-package! fish-mode
  :mode "\\.fish")

(def-package! ws-butler
  :demand
  :config
  (ws-butler-global-mode))

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


;; stole from https://emacs.stackexchange.com/a/16495/16662
(defmacro doom-with-advice (args &rest body)
  (declare (indent 3))
  (let ((fun-name (car args))
        (advice   (cadr args))
        (orig-sym (make-symbol "orig")))
    `(cl-letf* ((,orig-sym  (symbol-function ',fun-name))
                ((symbol-function ',fun-name)
                 (lambda (&rest args)
                   (apply ,advice ,orig-sym args))))
       ,@body)))

(defadvice edebug-pop-to-buffer (around +amos*edebug-pop-to-buffer activate)
  (doom-with-advice (split-window (lambda (orig-fun window) (funcall orig-fun window nil 'right)))
      ad-do-it))

(defadvice message-insert-signature (around +amos*message-insert-signature activate)
  (let ((old-insert (symbol-function 'insert)))
    (flet ((insert (str) (eval-when-compile (require 'subr-x)) (funcall old-insert (string-trim-right str))))
      ad-do-it)))

(setq shackle-default-alignment 'right
      shackle-default-size 0.5
      shackle-rules
      '(("*Messages*" :noselect t :autoclose t)
        ("*Warnings*" :noselect t :autoclose t)
        (" *Marked Files*" :noselect t :autoclose t :align below) ; fix dired multi file commands hiding ivy minibuffer
        ("*compilation*" :autoclose t)
        ("^\\*eww" :regexp t :size 0.5 :select t :autokill t :noesc t)
        ("^\\*ftp " :noselect t :autokill t :noesc t)
        ;; doom
        ("^\\*doom:scratch" :regexp t :noesc t :select t :modeline t :autokill t :static t)
        ("^\\*doom:" :regexp t :size 0.35 :noesc t :select t)
        ("^ ?\\*doom " :regexp t :noselect t :autokill t :autoclose t :autofit t)
        ;; built-in (emacs)
        ("*ert*" :same t :modeline t)
        ("*info*" :select t :autokill t)
        ("*Backtrace*" :noselect t)
        ("*Warnings*"  :noselect t :autofit t)
        ("^\\*.*Shell Command.*\\*$" :regexp t :noselect t :autokill t :autoclose t)
        (apropos-mode :autokill t :autoclose t)
        (Buffer-menu-mode :autokill t)
        (comint-mode :noesc t)
        (grep-mode :noselect t :autokill t)
        (profiler-report-mode :regexp t :autokill t :modeline minimal)
        (tabulated-list-mode :noesc t)
        ("^ ?\\*" :regexp t :noselect t :autokill t :autoclose t)

        ("*Pp Eval Output*" :noselect t :autoclose t)
        ("*Org Export Dispatcher*" :noselect t)
        ("*Stardict Output*" :autoclose t :noselect t :autofit t)
        (" *mu4e-verify*" :size 0.1 :autoclose t :noselect t :align below)
        (" *mu4e-update*" :size 0.1 :autoclose t :noselect t :align below)
        ("*Help*" :noselect t :autoclose t)
        ("*xref*" :noselect t :autoclose t)
        ("^ ?\\*doom " :regexp t :noselect t :autokill t :autoclose t :autofit t)))

(defadvice hl-line-mode (after +amos*hl-line-mode activate)
  (set-face-background hl-line-face "Gray13"))

(defadvice +tmux/run (after +amos*tmux-run activate)
  "Run COMMAND in tmux. If NORETURN is non-nil, send the commands as keypresses
but do not execute them."
  (interactive
   (list (read-string "tmux $ ")
         current-prefix-arg))
  (+tmux (shell-quote-argument command)
         (unless noreturn " Enter")))

;; (set-face-background 'show-paren-match (face-background 'default))
;; (set-face-foreground 'show-paren-match (face-foreground 'default))
;; (set-face-attribute 'show-paren-match nil :weight 'normal)

(def-package! dired-quick-sort
  :after dired
  :config
  (dired-quick-sort-setup))

(defun xah-display-minor-mode-key-priority  ()
  "Print out minor mode's key priority.
URL `http://ergoemacs.org/emacs/minor_mode_key_priority.html'
Version 2017-01-27"
  (interactive)
  (mapc
   (lambda (x) (prin1 (car x)) (terpri))
   minor-mode-map-alist))

(defun +amos/evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(def-package! dired-open
  :after dired
  :config
  (push #'+amos/dired-open-callgrind dired-open-functions))

;; (def-package! dired-avfs
;;   :after dired)

(def-package! org-mime
  :after org-mu4e)

(def-package! ivy-rich
  :after ivy
  :config
  (push (lambda (buf)
          (and (get-buffer buf)
               (with-current-buffer buf
                 (memq major-mode '(dired-mode wdired-mode)))))
        ivy-ignore-buffers)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer))

(defun +amos*switch-buffer-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip buffers that match `ivy-ignore-buffers'."
  (let ((res (ivy--re-filter regexp candidates)))
    (if (or (null ivy-use-ignore)
            (null ivy-ignore-buffers)
            (string-match "\\`\\." ivy-text))
        res
      (or (cl-remove-if
           (lambda (buf)
             (cl-find-if
              (lambda (f-or-r)
                (if (functionp f-or-r)
                    (funcall f-or-r buf)
                  (string-match-p f-or-r buf)))
              ivy-ignore-buffers))
           res)
          (and (eq ivy-use-ignore t)
               res)))))
(advice-add #'ivy--switch-buffer-matcher :override #'+amos*switch-buffer-matcher)

(def-package! hl-line+
  :demand
  :config
  (global-hl-line-mode +1))

(def-package! rainbow-blocks
  :commands rainbow-blocks-mode)

(after! evil
  (defadvice evil-ret-gen (around amos*evil-ret-gen activate)
    (let ((url (thing-at-point 'url)))
      (if url (goto-address-at-point)
        ad-do-it))))

(def-package! unfill
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle))


;; recenter buffer when switching windows
(defun +amos|update-window-buffer-list ()
  (walk-window-tree
   (lambda (window)
     (let ((old-buffer (window-parameter window 'my-last-buffer))
           (new-buffer (window-buffer window)))
       (unless (eq old-buffer new-buffer)
         ;; The buffer of a previously existing window has changed or
         ;; a new window has been added to this frame.
         (recenter)
         (setf (window-parameter window 'my-last-buffer) new-buffer))))))

(add-hook! 'window-configuration-change-hook #'+amos|update-window-buffer-list)

(defun +amos/counsel-rg-cur-dir ()
  (interactive)
  (counsel-rg nil default-directory))

(def-package! yapfify
  :after python)

(defun +amos/dired-open-callgrind ()
  "Open callgrind files according to its name."
  (interactive)
  (let ((file (ignore-errors (dired-get-file-for-visit)))
        process)
    (when (and file
               (not (file-directory-p file)))
      (when (string-match-p "$[cachegrind|callgrind].out" file)
        (setq process (dired-open--start-process file "kcachegrind"))))
    process))

(def-package! ag
  :after projectile)

(def-package! helm-make
  :after ivy
  :config
  (setq helm-make-completion-method 'ivy))

;; from spacemacs
(defun +amos/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (and (featurep 'projectile)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name) 'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?") new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

;; BEGIN align functions

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun +amos/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))

    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))

    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun +amos/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro +amos-create-align-repeat-x! (name regexp &optional justify-right default-after)
  (let ((new-func (intern (concat "+amos/align-repeat-" name))))
    `(defun ,new-func (start end switch)
       (interactive "r\nP")
       (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
         (+amos/align-repeat start end ,regexp ,justify-right after)))))

(+amos-create-align-repeat-x! "comma" "," nil t)
(+amos-create-align-repeat-x! "semicolon" ";" nil t)
(+amos-create-align-repeat-x! "colon" ":" nil t)
(+amos-create-align-repeat-x! "equal" "=")
(+amos-create-align-repeat-x! "math-oper" "[+\\-*/]")
(+amos-create-align-repeat-x! "ampersand" "&")
(+amos-create-align-repeat-x! "bar" "|")
(+amos-create-align-repeat-x! "left-paren" "(")
(+amos-create-align-repeat-x! "right-paren" ")" t)
(+amos-create-align-repeat-x! "left-curly-brace" "{")
(+amos-create-align-repeat-x! "right-curly-brace" "}" t)
(+amos-create-align-repeat-x! "left-square-brace" "\\[")
(+amos-create-align-repeat-x! "right-square-brace" "\\]" t)
(+amos-create-align-repeat-x! "backslash" "\\\\")

;; END align functions

(defun +amos/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun +amos/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; from https://www.emacswiki.org/emacs/CopyingWholeLines
(defun +amos/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                             ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun +amos/uniquify-lines ()
  "Remove duplicate adjacent lines in a region or the current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun +amos/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun +amos/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (+amos/sort-lines -1))

(defun +amos/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column,
using a visual block/rectangle selection.
A non-nil argument sorts in REVERSE order."
  (interactive "P")
  (if (and
       ;; is there an active selection
       (or (region-active-p) (evil-visual-state-p))
       ;; is it a block or rectangle selection
       (or (eq evil-visual-selection 'block) (eq rectangle-mark-mode t))
       ;; is the selection height 2 or more lines
       (>= (1+ (- (line-number-at-pos (region-end))
                  (line-number-at-pos (region-beginning)))) 2))
      (sort-columns reverse (region-beginning) (region-end))
    (error "Sorting by column requires a block/rect selection on 2 or more lines.")))

(defun +amos/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order,
using a visual block/rectangle selection."
  (interactive)
  (+amos/sort-lines-by-column -1))

(defun +amos/select-current-block ()
  "Select the current block of text between blank lines."
  (interactive)
  (let (p1)
    (when (re-search-backward "\n[ \t]*\n" nil "move")
      (re-search-forward "\n[ \t]*\n"))
    (setq p1 (point))
    (if (re-search-forward "\n[ \t]*\n" nil "move")
        (re-search-backward "\n[ \t]*\n"))
    (set-mark p1)))

;; From http://xugx2007.blogspot.ca/2007/06/benjamin-rutts-emacs-c-development-tips.html
(setq compilation-finish-function
      (lambda (buf str)

        (let ((case-fold-search nil))
          (if (or (string-match "exited abnormally" str)
                  (string-match "FAILED" (buffer-string)))

              ;; there were errors
              (message "There were errors. SPC-e-n to visit.")
            (unless (or (string-match "Grep finished" (buffer-string))
                        (string-match "Ag finished" (buffer-string))
                        (string-match "nosetests" (buffer-name)))

              ;; no errors
              (message "compilation ok."))))))

(defun +amos/close-compilation-window ()
  "Close the window containing the '*compilation*' buffer."
  (interactive)
  (when compilation-last-buffer
    (delete-windows-on compilation-last-buffer)))

(defun +amos/switch-to-scratch-buffer (&optional arg)
  "Switch to the `*scratch*' buffer, creating it first if needed.
if prefix argument ARG is given, switch to it in an other, possibly new window."
  (interactive "P")
  (let ((exists (get-buffer "*scratch*")))
    (if arg
        (switch-to-buffer-other-window (get-buffer-create "*scratch*"))
      (switch-to-buffer (get-buffer-create "*scratch*")))
    (when (and (not exists)
               (not (eq major-mode dotspacemacs-scratch-mode))
               (fboundp dotspacemacs-scratch-mode))
      (funcall dotspacemacs-scratch-mode))))

(defun swap-args (fun)
  (if (not (equal (interactive-form fun)
                  '(interactive "P")))
      (error "Unexpected")
    (advice-add
     fun
     :around
     (lambda (x &rest args)
       "Swap the meaning the universal prefix argument"
       (if (called-interactively-p 'any)
           (apply x (cons (not (car args)) (cdr args)))
         (apply x args))))))

(after! evil-surround
  (setq-default evil-surround-pairs-alist (append '((?` . ("`" . "`")) (?~ . ("~" . "~"))) evil-surround-pairs-alist)))

(defadvice dired-clean-up-after-deletion (around +amos*dired-clean-up-after-deletion activate)
  (doom-with-advice (y-or-n-p (lambda (&rest _) t))
      ad-do-it))

(after! counsel-projectile
  (defun +amos/counsel-projectile-switch-project ()
    (interactive)
    (ivy-read (projectile-prepend-project-name "Switch to project: ")
              projectile-known-projects
              :preselect (and (projectile-project-p)
                              (abbreviate-file-name (projectile-project-root)))
              :action #'+amos/find-file
              :require-match t
              :caller #'+amos/counsel-projectile-switch-project)))

(def-package! git-timemachine
  :commands +amos@time-machine/body
  :config
  (defhydra +amos@time-machine (:hint nil
                                 :pre (let (golden-ratio-mode)
                                        (unless (bound-and-true-p git-timemachine-mode)
                                          (call-interactively 'git-timemachine)))
                                 :post (when (bound-and-true-p git-timemachine-mode)
                                         (git-timemachine-quit))
                                 :foreign-keys run)
    "
[_p_/_N_] previous [_n_] next [_c_] current [_g_] goto nth rev [_Y_] copy hash [_q_] quit"
    ("c" git-timemachine-show-current-revision)
    ("g" git-timemachine-show-nth-revision)
    ("p" git-timemachine-show-previous-revision)
    ("n" git-timemachine-show-next-revision)
    ("N" git-timemachine-show-previous-revision)
    ("Y" git-timemachine-kill-revision)
    ("q" nil :exit t)))

(def-package! gitattributes-mode
  :defer t)

(def-package! gitconfig-mode
  :defer t)

(def-package! gitignore-mode
  :defer t)

(def-package! magit-svn
  :commands turn-on-magit-svn
  :init (add-hook 'magit-mode-hook 'turn-on-magit-svn))

(def-package! smeargle
  :defer t
  :commands smeargle smeargle-commits smeargle-clear)

(def-package! fill-column-indicator
  :commands fci-mode)

(def-package! page-break-lines
  :commands global-page-break-lines-mode
  :init
  (global-page-break-lines-mode +1))

(def-package! adoc-mode
  :mode "\\.adoc$")

(defun +amos/toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun +amos*ivy-rich-switch-buffer-pad (str len &optional left)
  "Improved version of `ivy-rich-switch-buffer-pad' that truncates long inputs."
  (let ((real-len (length str)))
    (cond
     ((< real-len len) (if left
                           (concat (make-string (- len real-len) ? ) str)
                         (concat str (make-string (- len real-len) ? ))))
     ((= len real-len) str)
     ((< len 1) str)
     (t (concat (substring str 0 (- len 1)) "…")))))

;; Override the original function using advice
(advice-add 'ivy-rich-switch-buffer-pad :override #'+amos*ivy-rich-switch-buffer-pad)

(evil-define-motion +amos*evil-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (doom/backward-to-bol-or-indent))
(advice-add 'evil-beginning-of-line :override #'+amos*evil-beginning-of-line)

(defun +amos/save-buffer-without-dtw ()
  (interactive)
  (let ((b (current-buffer)))   ; memorize the buffer
    (with-temp-buffer ; new temp buffer to bind the global value of before-save-hook
      (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
        (with-current-buffer b  ; go back to the current buffer, before-save-hook is now buffer-local
          (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
            (save-buffer)))))))

(defun +amos/projectile-current-project-files ()
  "Return a list of files for the current project."
  (let ((files (and projectile-enable-caching
                    (gethash (projectile-project-root) projectile-projects-cache))))
    ;; nothing is cached
    (unless files
      (when projectile-enable-caching
        (message "Empty cache. Projectile is initializing cache..."))
      (setq files
            (split-string
             (shell-command-to-string
              (concat
               "fd '' --hidden "
               (directory-file-name (projectile-project-root))))))
      ;; cache the resulting list of files
      (when projectile-enable-caching
        (projectile-cache-project (projectile-project-root) files)))
    (projectile-sort-files files)))

(advice-add #'projectile-current-project-files :override #'+amos/projectile-current-project-files)
(advice-add #'projectile-cache-files-find-file-hook :override #'ignore)

(after! xref
  (defun ivy-xref-make-collection (xrefs)
    "Transform XREFS into a collection for display via `ivy-read'."
    (let ((collection nil))
      (dolist (xref xrefs)
        (with-slots (summary location) xref
          (let ((line (xref-location-line location))
                (file (xref-location-group location))
                (candidate nil))
            (setq candidate (concat
                             ;; use file name only
                             (car (reverse (split-string file "\\/")))
                             (when (string= "integer" (type-of line))
                               (concat ":" (int-to-string line) ": "))
                             ;; (propertize summary 'font-lock-face 'font-lock-string-face)))
                             summary))
            (push `(,candidate . ,location) collection))))
      collection))

  (defun ivy-xref-select (candidate)
    "Select CANDIDATE."
    (let* ((marker (xref-location-marker (cdr candidate)))
           (buf (marker-buffer marker))
           (offset (marker-position marker)))
      (with-current-buffer buf
        (goto-char offset)
        (switch-to-buffer buf))))

  (defun ivy-xref-show-xrefs (xrefs _alist)
    "Show the list of XREFS via ivy."
    (ivy-read "xref: " (ivy-xref-make-collection xrefs)
              :require-match t
              :action #'ivy-xref-select))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

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

(defun endless/sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(define-key emacs-lisp-mode-map "#" #'endless/sharp)

(add-hook! 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(defun +file-templates-append (args)
  (cl-destructuring-bind (regexp trigger &optional mode project-only-p) args
    (define-auto-insert
      regexp
      (if trigger
          (vector
           `(lambda () (+file-templates--expand ,trigger ',mode ,project-only-p)))
        #'ignore) 'after)))

(add-to-list 'auto-mode-alist '("/home/amos/git/serverconfig/scripts/.+" . sh-mode) 'append)
(+file-templates-append '("/home/amos/git/serverconfig/scripts/.+"   "__"   sh-mode))
(+file-templates-add '("/home/amos/git/serverconfig/.config/fish/functions/.+" "__func" fish-mode))

(defmacro +amos--def-browse-in! (name dir)
  `(defun ,(intern (format "+amos/browse-%s" name)) ()
     (interactive)
     (doom-project-browse ,dir)))

(+amos--def-browse-in! script "/home/amos/scripts/")
(+amos--def-browse-in! note "/home/amos/notes/")
(+amos--def-browse-in! org "/home/amos/org/")

(def-package! go-playground
  :commands (go-playground go-playground-mode)
  :bind (:map go-playground-mode-map
          ([S-return] . go-playground-rm)))

(def-package! rust-playground
  :commands (rust-playground rust-playground-mode)
  :bind (:map rust-playground-mode-map
          ([S-return] . rust-playground-rm)))

(def-package! cc-playground
  :commands cc-playground cc-playground-mode cc-playground-find-snippet
  :load-path (lambda () (interactive) (if (equal (system-name) "t450s") "~/git/cc-playground"))
  :bind (:map cc-playground-mode-map
          ("C-c r" . cc-playground-add-or-modify-tag)
          ("C-c b" . cc-playground-bench)
          ("C-c d" . cc-playground-debug)
          ("C-c t" . cc-playground-debug-test)
          ("C-c l" . cc-playground-ivy-add-library-link)
          ("C-c c" . cc-playground-change-compiler)
          ("C-c o" . cc-playground-switch-optimization-flag)
          ("C-c f" . cc-playground-add-compilation-flags)))

(put :hint  'lisp-indent-function 1)
(put :color 'lisp-indent-function 'defun)
(put :pre   'lisp-indent-function 'defun)
(put :post  'lisp-indent-function 'defun)

(def-package! pdf-tools
  :if (string= (getenv "GUI") "t")
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :init (load "pdf-tools-autoloads" nil t)
  :config
  (pdf-tools-install)


  ;; | color    | toggle                     | meaning      |
  ;; |----------+----------------------------+--------------|
  ;; | red      |                            | persist      |
  ;; | blue     | :exit t                    | transient    |
  ;; | amaranth | :foreign-keys warn         | persist w    |
  ;; | teal     | :foreign-keys warn :exit t | transient w  |
  ;; | pink     | :foreign-keys run          | nested       |
  (defhydra +amos@pdf-tools (:hint nil
                              :color amaranth
                              :pre (setq which-key-inhibit t)
                              :post (setq which-key-inhibit nil))
    "
 Navigation^^^^                Scale/Fit^^                    Annotations^^       Actions^^           Other^^
 ----------^^^^--------------- ---------^^------------------  -----------^^------ -------^^---------- -----^^---
 [_j_/_k_] scroll down/up      [_W_] fit to width             [_al_] list         [_s_] search         [_q_] quit
 [_h_/_l_] scroll left/right   [_H_] fit to height            [_at_] text         [_O_] outline
 [_d_/_u_] pg down/up          [_P_] fit to page              [_aD_] delete       [_p_] print
 [_J_/_K_] next/prev pg        [_m_] slice using mouse        [_am_] markup       [_o_] open link
 [_0_/_$_] full scroll l/r     [_b_] slice from bounding box  ^^                  [_r_] revert
 ^^^^                          [_R_] reset slice              ^^                  [_t_] attachments
 ^^^^                          [_zr_] reset zoom              ^^                  [_n_] night mode
 "
    ;; Navigation
    ("j"  pdf-view-next-line-or-next-page)
    ("k"  pdf-view-previous-line-or-previous-page)
    ("l"  image-forward-hscroll)
    ("h"  image-backward-hscroll)
    ("J"  pdf-view-next-page)
    ("K"  pdf-view-previous-page)
    ("u"  pdf-view-scroll-down-or-previous-page)
    ("d"  pdf-view-scroll-up-or-next-page)
    ("0"  image-bol)
    ("$"  image-eol)
    ;; Scale/Fit
    ("W"  pdf-view-fit-width-to-window)
    ("H"  pdf-view-fit-height-to-window)
    ("P"  pdf-view-fit-page-to-window)
    ("m"  pdf-view-set-slice-using-mouse)
    ("b"  pdf-view-set-slice-from-bounding-box)
    ("R"  pdf-view-reset-slice)
    ("zr" pdf-view-scale-reset)
    ;; Annotations
    ("aD" pdf-annot-delete)
    ("at" pdf-annot-attachment-dired :exit t)
    ("al" pdf-annot-list-annotations :exit t)
    ("am" pdf-annot-add-markup-annotation)
    ;; Actions
    ("s" pdf-occur :exit t)
    ("O" pdf-outline :exit t)
    ("p" pdf-misc-print-document :exit t)
    ("o" pdf-links-action-perform :exit t)
    ("r" pdf-view-revert-buffer)
    ("t" pdf-annot-attachment-dired :exit t)
    ("n" pdf-view-midnight-minor-mode)
    ;; Other
    ("q" nil :exit t)))


(defvar +amos--ivy-regex-hash
  (make-hash-table :test #'equal)
  "Store pre-computed regex.")

(defun +amos*ivy-regex-half-quote (str &optional greedy)
  "Re-build regex pattern from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str +amos--ivy-regex-hash))))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
        (setq str (substring str 0 -1)))
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons (setq ivy--subexps 0) (regexp-quote (car subs)))
                        (cons (setq ivy--subexps (length subs))
                              (mapconcat (lambda (s) (format "\\(%s\\)" (regexp-quote s))) subs (if greedy ".*" ".*?")))))
                    +amos--ivy-regex-hash)))))

(defun +amos--old-ivy-regex (str &optional greedy)
  "Re-build regex pattern from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str ivy--regex-hash))))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
        (setq str (substring str 0 -1)))
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons
                           (setq ivy--subexps 0)
                           (car subs))
                        (cons
                         (setq ivy--subexps (length subs))
                         (mapconcat
                          (lambda (x)
                            (if (string-match "\\`\\\\([^?].*\\\\)\\'" x)
                                x
                              (format "\\(%s\\)" x)))
                          subs
                          (if greedy
                              ".*"
                            ".*?")))))
                    ivy--regex-hash)))))

(defvar +amos--old-ivy-regex-function '+amos--old-ivy-regex)

(defun +amos*ivy-toggle-regexp-quote ()
  "Toggle the regexp quoting."
  (interactive)
  (setq ivy--old-re nil)
  (cl-rotatef ivy--regex-function +amos--old-ivy-regex-function ivy--regexp-quote))

;; (add-hook! 'minibuffer-setup-hook (setq-local truncate-lines t) (ivy-toggle-regexp-quote))

(advice-add #'ivy-toggle-regexp-quote :override #'+amos*ivy-toggle-regexp-quote)
(advice-add #'ivy--regex :override #'+amos*ivy-regex-half-quote)

(defun +amos*yas--modes-to-activate (&optional mode)
  "Compute list of mode symbols that are active for `yas-expand' and friends."
  (defvar yas--dfs)        ;We rely on dynbind.  We could use `letrec' instead!
  (let* ((explored (if mode (list mode) ; Building up list in reverse.
                     (reverse (cons major-mode yas--extra-modes))))
         (yas--dfs
          (lambda (mode)
            (cl-loop for neighbour
                     in (cl-list* (get mode 'derived-mode-parent)
                                  ;; NOTE: `fboundp' check is redundant
                                  ;; since Emacs 24.4.
                                  (and (fboundp mode) (symbol-function mode))
                                  (gethash mode yas--parents))
                     when (and neighbour
                               (not (memq neighbour explored))
                               (symbolp neighbour))
                     do (push neighbour explored)
                     (funcall yas--dfs neighbour)))))
    (mapc yas--dfs explored)
    (nreverse explored)))

(advice-add #'yas--modes-to-activate :override #'+amos*yas--modes-to-activate)

(def-package! dired-ranger
  :after dired)

(def-package! rainbow-mode)

(def-package! google-translate
  :commands google-translate-at-point google-translate-query-translate)

(def-package! kurecolor
  :after rainbow-mode
  :config
  ;; | color    | toggle                     | meaning      |
  ;; |----------+----------------------------+--------------|
  ;; | red      |                            | persist      |
  ;; | blue     | :exit t                    | transient    |
  ;; | amaranth | :foreign-keys warn         | persist w    |
  ;; | teal     | :foreign-keys warn :exit t | transient w  |
  ;; | pink     | :foreign-keys run          | nested       |
  (def-hydra! +rgb@kurecolor (:color red :hint nil)
    "
Inc/Dec      _w_/_W_ brightness      _d_/_D_ saturation      _e_/_E_ hue    "
    ("w" kurecolor-decrease-brightness-by-step)
    ("W" kurecolor-increase-brightness-by-step)
    ("d" kurecolor-decrease-saturation-by-step)
    ("D" kurecolor-increase-saturation-by-step)
    ("e" kurecolor-decrease-hue-by-step)
    ("E" kurecolor-increase-hue-by-step)
    ("q" nil "cancel" :color blue)))

(defun ab-char-inc ()
  (interactive)
  (save-excursion
    (let ((chr  (1+ (char-after))))
      (unless (characterp chr) (error "Cannot increment char by one"))
      (delete-char 1)
      (insert chr))))

(def-hydra! +amos@perf-stat (:color blue :hint nil)
  "
# CPU counter statistics for the specified command:
_a_:perf stat command

# Detailed CPU counter statistics (includes extras) for the specified command:
_b_:perf stat -d command

# CPU counter statistics for the specified PID, until Ctrl-C:
_c_:perf stat -p PID

# CPU counter statistics for the entire system, for 5 seconds:
_d_:perf stat -a sleep 5

# Various basic CPU statistics, system wide, for 10 seconds:
_e_:perf stat -e cycles,instructions,cache-references,cache-misses,bus-cycles -a sleep 10

# Various CPU level 1 data cache statistics for the specified command:
_f_:perf stat -e L1-dcache-loads,L1-dcache-load-misses,L1-dcache-stores command

# Various CPU data TLB statistics for the specified command:
_g_:perf stat -e dTLB-loads,dTLB-load-misses,dTLB-prefetch-misses command

# Various CPU last level cache statistics for the specified command:
_h_:perf stat -e LLC-loads,LLC-load-misses,LLC-stores,LLC-prefetches command

# Using raw PMC counters, eg, unhalted core cycles:
_i_:perf stat -e r003c -a sleep 5

# PMCs: cycles and frontend stalls via raw specification
_j_:perf stat -e cycles -e cpu/event=0x0e,umask=0x01,inv,cmask=0x01/ -a sleep 5

# Count syscalls per-second system-wide (this should be in /proc)
_k_:perf stat -e rawsyscalls:sysenter -I 1000 -a

# Count system calls by type for the specified PID, until Ctrl-C
_l_:perf stat -e 'syscalls:sysenter*' -p PID

# Count system calls by type for the entire system, for 5 seconds
_m_:perf stat -e 'syscalls:sysenter*' -a sleep 5

# Count scheduler events for the specified PID, for 10 seconds
_o_:perf stat -e 'sched:*' -p PID sleep 10

# Count ext4 events for the entire system, for 10 seconds
_p_:perf stat -e 'ext4:*' -a sleep 10

# Count block device I/O events for the entire system, for 10 seconds
_q_:perf stat -e 'block:*' -a sleep 10"

  ;; TODO
    ("a" kurecolor-decrease-brightness-by-step)
    ("b" kurecolor-decrease-brightness-by-step)
    ("c" kurecolor-decrease-brightness-by-step)
    ("d" kurecolor-decrease-brightness-by-step)
    ("e" kurecolor-decrease-brightness-by-step)
    ("f" kurecolor-decrease-brightness-by-step)
    ("g" kurecolor-decrease-brightness-by-step)
    ("h" kurecolor-decrease-brightness-by-step)
    ("i" kurecolor-decrease-brightness-by-step)
    ("j" kurecolor-decrease-brightness-by-step)
    ("k" kurecolor-decrease-brightness-by-step)
    ("l" kurecolor-decrease-brightness-by-step)
    ("m" kurecolor-decrease-brightness-by-step)
    ("n" kurecolor-decrease-brightness-by-step)
    ("o" kurecolor-decrease-brightness-by-step)
    ("p" kurecolor-decrease-brightness-by-step)
    ("q" kurecolor-increase-brightness-by-step)
    ("r" kurecolor-decrease-saturation-by-step))

(defun +amos/replace-last-sexp ()
  (interactive)
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(defun +amos/new-empty-elisp-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (emacs-lisp-mode)
    (setq buffer-offer-save t)
    buf))


(defun +amos/ivy-complete-dir ()
  "Enter a recursive `ivy-read' session using the current history.
The selected history element will be inserted into the minibuffer."
  (interactive)
  (let ((enable-recursive-minibuffers t)
        (history (+amos--get-all-jump-dirs))
        (old-last ivy-last)
        (ivy-recursive-restore nil))
    (ivy-read "Choose-directory: "
              history
              :action (lambda (x)
                        (setq x (concat x "/"))
                        (ivy--reset-state
                         (setq ivy-last old-last))
                        (delete-minibuffer-contents)
                        (insert (substring-no-properties x))
                        (ivy--cd-maybe)))))

(defun +amos/smart-jumper-backward ()
  (interactive)
  (+amos/smart-jumper t))

(defun +amos/smart-jumper-forward ()
  (interactive)
  (+amos/smart-jumper))

(defun +amos/smart-jumper (&optional f)
  (unless (or (eq last-command '+amos/smart-jumper-backward)
              (eq last-command '+amos/smart-jumper-forward))
    (evil-set-jump))
  (let* ((dir (if f -1 (forward-char) 1))
         (c (point))
         quote
         delim)
    (setq quote (save-excursion
                  (let ((b (progn (forward-evil-quote -1) (point)))
                        (e (progn (forward-evil-quote 1) (point))))
                    (if (and (< b c) (< c e))
                        (if (< 0 dir) e b)
                      -1))))
    (if (< 0 quote)
        (goto-char quote)
      (let* ((paren (save-excursion (if (= 0 (evil-up-paren ?( ?) dir)) (point) nil)))
             (bracket (save-excursion (if (= 0 (evil-up-paren ?[ ?] dir)) (point) nil)))
             (brace (save-excursion (if (= 0 (evil-up-paren ?{ ?} dir)) (point) nil))))
        (setq delim (condition-case nil
                        (if (< dir 0)
                            (-max (--filter it (list paren bracket brace)))
                          (-min (--filter it (list paren bracket brace))))
                      (error nil))))
      (if delim (goto-char delim)))
    (if (< 0 dir) (backward-char))))

(defun +amos/complete ()
  (interactive)
  (require 'thingatpt)
  (require 'company)
  (if (/= (point)
          (save-excursion
            (end-of-thing 'symbol)
            (point)))
      (save-excursion (insert " ")))
  (when (and (company-manual-begin)
             (= company-candidates-length 1))
    (company-complete-common)))

(defvar my-kill-ring nil)
(defmacro mkr! (&rest body)
  `(let (interprogram-cut-function
         (kill-ring my-kill-ring))
     ,@body
     (setq my-kill-ring kill-ring)))

(defun +amos/delete-char()
  (interactive)
  (mkr! (delete-char 1 1)))

(defun +amos/backward-delete-char()
  (interactive)
  (mkr! (backward-delete-char-untabify 1 1)))

(defun +amos/kill-line ()
  (interactive)
  (mkr! (kill-region (point) (point-at-eol))))

(defun +amos/backward-kill-to-bol-and-indent ()
  (interactive)
  (let ((empty-line-p (save-excursion (beginning-of-line)
                                      (looking-at-p "[ \t]*$"))))
    (mkr! (kill-region (point-at-bol) (point)))
    (unless empty-line-p
      (indent-according-to-mode))))

(defun +amos/forward-delete-word (&optional subword);
  (interactive)
  (evil-signal-at-bob-or-eob 1)
  (unless (or (evil-insert-state-p) (active-minibuffer-window))
    (evil-insert-state 1))
  (if subword (subword-mode +1))
  (mkr! (kill-region (point)
                     (max
                      (save-excursion
                        (if (looking-at "[ \t\r\n\v\f]")
                            (progn
                              (re-search-forward "[^ \t\r\n\v\f]")
                              (backward-char))
                          (forward-thing 'evil-word 1))
                        (point))
                      (line-beginning-position))))
  (if subword (subword-mode -1)))

(defun +amos/backward-delete-word (&optional subword)
  (interactive)
  (evil-signal-at-bob-or-eob -1)
  (unless (or (eolp) (evil-insert-state-p) (active-minibuffer-window))
    (evil-insert-state 1)
    (forward-char))
  (if subword (subword-mode +1))
  (mkr! (kill-region (point)
                 (min
                  (save-excursion
                    (if (looking-back "[ \t\r\n\v\f]")
                        (progn
                          (re-search-backward "[^ \t\r\n\v\f]")
                          (forward-char))
                      (forward-thing 'evil-word -1))
                    (point))
                  (line-end-position))))
  (if subword (subword-mode -1)))

(defun +amos/backward-word-insert (&optional subword)
  (interactive)
  (evil-signal-at-bob-or-eob -1)
  (if subword (subword-mode +1))
  (unless (or (eolp) (evil-insert-state-p) (active-minibuffer-window))
    (evil-insert-state 1)
    (forward-char))
  (if (looking-back "[ \t\r\n\v\f]")
      (progn
        (re-search-backward "[^ \t\r\n\v\f]")
        (forward-char))
    (forward-thing 'evil-word -1))
  (if subword (subword-mode -1)))

(defun +amos/forward-word-insert (&optional subword)
  (interactive)
  (evil-signal-at-bob-or-eob 1)
  (if subword (subword-mode +1))
  (unless (or (active-minibuffer-window) (evil-insert-state-p))
    (evil-insert-state 1))
  (if (looking-at "[ \t\r\n\v\f]")
      (progn
        (re-search-forward "[^ \t\r\n\v\f]")
        (backward-char))
    (forward-thing 'evil-word 1))
  (if subword (subword-mode -1)))

(after! subword
  (progn
    (define-category ?U "Uppercase")
    (define-category ?u "Lowercase")
    (modify-category-entry (cons ?A ?Z) ?U)
    (modify-category-entry (cons ?a ?z) ?u)
    (make-variable-buffer-local 'evil-cjk-word-separating-categories)
    (add-hook 'subword-mode-hook (lambda! (if subword-mode (push '(?u . ?U) evil-cjk-word-separating-categories)
                                        (setq evil-cjk-word-separating-categories (default-value 'evil-cjk-word-separating-categories)))))))

(defun +amos*subword-backward-internal ()
  (if superword-mode
      (forward-symbol -1)
    (if (save-excursion
          (let ((case-fold-search nil))
            (with-syntax-table (make-syntax-table (syntax-table))
              (modify-syntax-entry ?_ "_")
              (re-search-backward "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([[:upper:]]+\\W*\\)\\|\\W\\w+\\)" nil t))))
        (goto-char
         (cond
          ((and (match-end 3)
                (< 1 (- (match-end 3) (match-beginning 3)))
                (not (eq (point) (match-end 3))))
           (1- (match-end 3)))
          (t
           (1+ (match-beginning 0)))))
      (backward-word 1))))
(advice-add #'subword-backward-internal :override #'+amos*subword-backward-internal)

(def-package! evil-textobj-anyblock
  :commands
  evil-textobj-anyparen-inner-block
  evil-textobj-anyparen-a-block
  evil-textobj-anyquote-inner-block
  evil-textobj-anyquote-a-block
  :config
  (evil-define-text-object evil-textobj-anyparen-inner-block
    (count &optional beg end type)
    "Select the closest inner anyparen block."
    (let ((evil-textobj-anyblock-blocks '(("(" . ")")
                                          ("{" . "}")
                                          ("\\[" . "\\]")
                                          ("<" . ">")
                                          ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object evil-textobj-anyparen-a-block (count &optional beg end type)
    "Select the closest outer anyparen block."
    (let ((evil-textobj-anyblock-blocks '(("(" . ")")
                                          ("{" . "}")
                                          ("\\[" . "\\]")
                                          ("<" . ">")
                                          ("“" . "”"))))
      (evil-textobj-anyblock--make-textobj beg end type count t)))

  (evil-define-text-object evil-textobj-anyquote-inner-block
    (count &optional beg end type)
    "Select the closest inner anyquote block."
    (let ((evil-textobj-anyblock-blocks '(("'" . "'")
                                          ("\"" . "\"")
                                          ("`" . "`"))))
      (evil-textobj-anyblock--make-textobj beg end type count nil)))

  (evil-define-text-object evil-textobj-anyquote-a-block (count &optional beg end type)
    "Select the closest outer anyquote block."
    (let ((evil-textobj-anyblock-blocks '(("'" . "'")
                                          ("\"" . "\"")
                                          ("`" . "`"))))
      (evil-textobj-anyblock--make-textobj beg end type count t))))

(def-package! subword
  :commands subword-forward subword-backward)

(def-package! company-lsp
  :after company
  :init
  ;; Language servers have better idea filtering and sorting,
  ;; don't filter results on the client side.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil)
  (push 'company-lsp company-backends))

(after! xref
  (add-to-list 'xref-prompt-for-identifier '+jump/definition :append)
  (add-to-list 'xref-prompt-for-identifier '+jump/references :append)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references :append))

(def-package! lsp-mode
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (setq lsp-enable-flycheck nil))

(add-hook! (c-mode c++-mode) (flycheck-mode +1))

(def-package! lsp-ui
  :after lsp-mode
  :config
  (flycheck-add-mode 'lsp-ui 'c++-mode))

(require 'cl-lib)
(require 'subr-x)

(defvar my-cquery-blacklist nil
  "List of paths that should not enable lsp-cquery")

(defvar my-cquery-whitelist '("Dev/llvm")
  "List of paths that should enable lsp-cquery. Takes priority over my-cquery-blacklist")

(def-package! cquery
  :after lsp-mode
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  ;; (setq cquery-sem-highlight-method 'overlay)
  ;; or WAIT for https://lists.gnu.org/archive/html/emacs-devel/2017-05/msg00084.html
  ;; (setq cquery-enable-sem-highlight t)
  ;; (cquery-use-default-rainbow-sem-highlight)

  ;; (setq cquery-extra-args '("--log-stdin-stdout-to-stderr" "--log-file=/tmp/cq.log"))
  (setq cquery-extra-init-params '(:cacheFormat "msgpack" :index (:builtin_types t :comments 0)))
  (add-hook 'c-mode-common-hook #'my-cquery//enable))


(defun my-cquery//enable ()
  (when
      (and buffer-file-name
           (not (and (boundp 'lsp-mode) lsp-mode))
           (or
            (cl-some (lambda (x) (string-match-p x buffer-file-name)) my-cquery-whitelist)
            (cl-notany (lambda (x) (string-match-p x buffer-file-name)) my-cquery-blacklist))
           (or (locate-dominating-file default-directory "compile_commands.json")
               (locate-dominating-file default-directory ".cquery")))
    (setq eldoc-idle-delay 0.2)
    (lsp-cquery-enable)))


;; xref-find-apropos (workspace/symbol)

(defun my/highlight-pattern-in-text (pattern line)
  (when (> (length pattern) 0)
    (let ((i 0))
      (while (string-match pattern line i)
        (setq i (match-end 0))
        (add-face-text-property (match-beginning 0) (match-end 0) 'highlight t line)
        )
      line)))

(with-eval-after-load 'lsp-methods
  ;;; Override
  ;; This deviated from the original in that it highlights pattern appeared in symbol
  (defun lsp--symbol-information-to-xref (pattern symbol)
    "Return a `xref-item' from SYMBOL information."
    (let* ((location (gethash "location" symbol))
           (uri (gethash "uri" location))
           (range (gethash "range" location))
           (start (gethash "start" range))
           (name (gethash "name" symbol)))
      (xref-make (format "[%s] %s"
                         (alist-get (gethash "kind" symbol) lsp--symbol-kind)
                         (my/highlight-pattern-in-text (regexp-quote pattern) name))
                 (xref-make-file-location (string-remove-prefix "file://" uri)
                                          (1+ (gethash "line" start))
                                          (gethash "character" start)))))

  (cl-defmethod xref-backend-apropos ((_backend (eql xref-lsp)) pattern)
    (let ((symbols (lsp--send-request (lsp--make-request
                                       "workspace/symbol"
                                       `(:query ,pattern)))))
      (mapcar (lambda (x) (lsp--symbol-information-to-xref pattern x)) symbols))))

(set!
  :jump 'c-mode
  :definition #'lsp-ui-peek-find-definitions
  :references #'xref-find-references
  :documentation #'counsel-dash-at-point)
(set!
  :jump 'c++-mode
  :definition #'lsp-ui-peek-find-definitions
  :references #'xref-find-references
  :documentation #'counsel-dash-at-point)

;; lsp-ui-peek-find-{definitions,references}
;; (lsp-ui-peek-jump-backward)
;; (lsp-ui-peek-jump-forward)
;; (cquery-xref-find-custom "$cquery/base")
;; (cquery-xref-find-custom "$cquery/callers")
;; (cquery-xref-find-custom "$cquery/derived")
;; (cquery-xref-find-custom "$cquery/vars")

;; (defun cquery/base () (interactive) (lsp-ui-peek-find-custom 'base "$cquery/base"))
;; (defun cquery/callers () (interactive) (lsp-ui-peek-find-custom 'callers "$cquery/callers"))
;; (defun cquery/derived () (interactive) (lsp-ui-peek-find-custom 'derived "$cquery/derived"))
;; (defun cquery/vars () (interactive) (lsp-ui-peek-find-custom 'vars "$cquery/vars"))
(defun cquery/base () (interactive) (cquery-xref-find-custom "$cquery/base"))
(defun cquery/callers () (interactive) (cquery-xref-find-custom "$cquery/callers"))
(defun cquery/derived () (interactive) (cquery-xref-find-custom "$cquery/derived"))
(defun cquery/vars () (interactive) (cquery-xref-find-custom "$cquery/vars"))

(evil-define-command evil-wipeout-buffer (buffer &optional bang)
  "Deletes a buffer. Also clears related jump marks.
All windows currently showing this buffer will be closed except
for the last window in each frame."
  (interactive "<b><!>")
  (let* ((ring (make-ring evil-jumps-max-length))
         (jump-struct (evil--jumps-get-current))
         (idx (evil-jumps-struct-idx jump-struct))
         (i 0))
    (cl-loop for jump in (ring-elements (evil--jumps-get-window-jump-list))
             do (let* ((file-name (cadr jump)))
                  (if (or (string= file-name (buffer-file-name))
                          (string-match-p evil--jumps-buffer-targets (buffer-name)))
                      (if (<= i idx) (setq idx (1- idx)))
                    ;; else
                    (ring-insert ring jump)
                    (setq i (1+ i)))))
    (setf (evil-jumps-struct-ring jump-struct) ring)
    (setf (evil-jumps-struct-idx jump-struct) idx))
  (evil-delete-buffer buffer bang))

;; causes undo-tree-canary an infinite loop when doing evil-repeat
;; (add-hook! 'evil-insert-state-exit-hook (if buffer-file-name (save-buffer)))

(defun +amos/create-fish-function (name)
  (interactive "sNew function's name: ")
  (let ((full-name (expand-file-name (concat name ".fish") "/home/amos/.config/fish/functions/")))
    (if (file-exists-p full-name)
        (user-error "Function with the same name already exists!"))
    (find-file full-name)
    (evil-initialize-state 'insert)))

(def-package! persistent-scratch
  :commands (persistent-scratch-setup-default)
  :init
  (persistent-scratch-setup-default))

(with-eval-after-load 'org-src
  (add-hook 'org-src-mode-hook
            'editorconfig-mode-apply t))


(defvar zygospore-spore-formation-register-name
  "zygospore-windows-time-machine"
  "Name of the register that zygospore uses to reverse `zygospore-delete-other-windows'.")

(defvar zygospore-last-full-frame-window
  nil
  "Last window that was full-frame'd.")

(defvar zygospore-last-full-frame-buffer
  nil
  "Last buffer that was full-frame'd.")

(defun zygospore-delete-other-window ()
  "Save current window-buffer configuration and full-frame the current buffer."
  (setq zygospore-last-full-frame-window (selected-window))
  (setq zygospore-last-full-frame-buffer (current-buffer))
  (window-configuration-to-register zygospore-spore-formation-register-name)
  (delete-other-windows))

(defun zygospore-restore-other-windows ()
  "Restore the window configuration to prior to full-framing."
  (jump-to-register zygospore-spore-formation-register-name))

(defun zygospore-toggle-delete-other-windows ()
  "Main zygospore func.
If the current frame has several windows, it will act as `delete-other-windows'.
If the current frame has one window,
  and it is the one that was last full-frame'd,
  and the buffer remained the same,
it will restore the window configuration to prior to full-framing."
  (interactive)
  (if (and (equal (selected-window) (next-window))
           (equal (selected-window) zygospore-last-full-frame-window)
           (equal (current-buffer) zygospore-last-full-frame-buffer))
      (zygospore-restore-other-windows)
    (zygospore-delete-other-window)))

(defun save-buffer-maybe ()
  (when (and (buffer-file-name)
             (buffer-modified-p))
    (save-buffer)))
(add-hook '+evil-esc-hook #'save-buffer-maybe)

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))

(advice-remove #'counsel-ag-function #'+ivy*counsel-ag-function)

(unless (equal (system-name) "t450s")
  (def-modeline-segment! tmux
    (let ((full-string (shell-command-to-string "tmux list-windows | awk -F: 'BEGIN{printf \"|\"} {printf \" %d |\", $1}'"))
          (highlight-string (shell-command-to-string "printf ' %d ' $(tmux display-message -p '#I')")))
      (string-match highlight-string full-string)
      (add-face-text-property (match-beginning 0) (match-end 0) '(:background "darkred") nil full-string)
      full-string))

  (def-modeline-segment! host
    (let ((full-string (string-trim-right (shell-command-to-string "hostname"))))
      (propertize full-string 'face 'doom-modeline-highlight)))

  (def-modeline! main
    (bar matches " " buffer-info "  %l:%c %p  " selection-info tmux)
    (host "   " buffer-encoding major-mode vcs flycheck)))

(defun setup-input-decode-map ()
  (map!
   (:map input-decode-map
     "\e[77~" [(control shift j)]
     "\e[76~" [(control shift d)]
     "\e[75~" [(control shift s)]
     "\e[74~" (kbd "C-.")
     "\e[73~" (kbd "C-,")
     "\e[72~" [S-return]
     "\e[71~" [M-S-backspace]
     "\e[70~" [C-return])))

(add-hook 'tty-setup-hook #'setup-input-decode-map)

(require 'yasnippet)
(require 'company)
(add-hook 'evil-insert-state-exit-hook #'yas-abort-snippet)
;; (add-hook 'evil-insert-state-exit-hook #'company-complete-selection)

(put 'cc-exec 'safe-local-variable #'stringp)
(put 'cc-flags 'safe-local-variable #'stringp)
(put 'cc-links 'safe-local-variable #'stringp)

(defun +amos*helm-dash-result-url (docset-name filename &optional anchor)
  "Return the full, absolute URL to documentation.
Either a file:/// URL joining DOCSET-NAME, FILENAME & ANCHOR with sanitization
 of spaces or a http(s):// URL formed as-is if FILENAME is a full HTTP(S) URL."
  (let* ((clean-filename (replace-regexp-in-string "<dash_entry_.*>" "" filename))
         (path (format "%s%s" clean-filename (if anchor (replace-regexp-in-string "%2E" "%252E" (format "#%s" anchor)) ""))))
    (if (string-match-p "^https?://" path)
        path
      (replace-regexp-in-string
       " "
       "%20"
       (concat
        "file:///"
        (expand-file-name "Contents/Resources/Documents/" (helm-dash-docset-path docset-name))
        path)))))
(advice-add #'helm-dash-result-url :override #'+amos*helm-dash-result-url)

(unless (string= (getenv "GUI") "t")
    (advice-add #'switch-to-buffer-other-frame :override #'+amos/switch-to-buffer-other-frame))

(after! ivy
  (dolist (cmd '(counsel-find-file +amos/counsel-projectile-switch-project))
    (ivy-add-actions
     cmd
     '(("f" find-file-other-frame "other frame")))))

(after! ivy
  (dolist (cmd '(ivy-switch-buffer))
    (ivy-add-actions
     cmd
     '(("f" switch-to-buffer-other-frame "other frame")))))

(add-hook! 'doom-init-ui-hook
  (set-face-background 'vertical-border "#282c34"))

(dolist (x '(cc-playground-exec cc-playground-debug cc-playground-exec-test cc-playground-bench))
  (advice-add x :before #'evil-force-normal-state))

(defun +amos/redisplay-and-recenter ()
  (interactive)
  (redraw-display)
  (recenter))

(defun +amos/evil-find-file-at-point-with-line ()
  (interactive)
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

(defun +amos*set-jump (&rest _)
  (evil-set-jump))
(advice-add 'counsel-git-grep-action :before #'+amos*set-jump)
