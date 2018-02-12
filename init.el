;;; private/amos/init.el -*- lexical-binding: t; -*-

(setq gnutls-verify-error nil
      doom-line-numbers-style 'relative
      tls-checktrust nil)

(add-hook! '(doom-post-init-hook minibuffer-setup-hook) (setq-local show-trailing-whitespace nil))
(add-hook! 'edebug-mode-hook #'evil-normalize-keymaps)
(add-hook! 'minibuffer-setup-hook (setq-local truncate-lines t))
(add-hook! 'eval-expression-minibuffer-setup-hook
  (define-key minibuffer-local-map "\C-p" #'previous-line-or-history-element)
  (define-key minibuffer-local-map "\C-n" #'next-line-or-history-element))

(remove-hook! 'doom-popup-mode-hook #'doom|hide-modeline-in-popup)

(advice-add #'nlinum-mode :override #'ignore)
(advice-add #'doom-hide-modeline-mode :override #'ignore)
(advice-add #'fringe-mode :override #'ignore)
(advice-add #'eldoc-mode :override #'ignore)
(advice-add #'dired-k--highlight-by-file-attribyte :override #'ignore)
(advice-add #'recenter-top-bottom :override #'recenter)
(advice-add #'git-gutter:next-hunk :after (lambda (arg) (recenter)))

(def-package-hook! nav-flash
  :pre-init
  (advice-add #'windmove-do-window-select :after #'+doom/blink-cursor)
  (advice-add #'recenter :after #'+doom/blink-cursor)
  (after! evil
    (advice-add #'evil--jumps-jump :after (lambda (&rest _) (recenter)))
    (advice-add #'evil-switch-to-windows-last-buffer :after (lambda (&rest _) (recenter))))
  nil)

;; (advice-add #'solaire-mode :override #'ignore)
;; (def-package-hook! solaire-mode :pre-init nil)
;; (def-package-hook! flycheck-pos-tip :disable)
(def-package-hook! stripe-buffer :disable)
(def-package-hook! visual-fill-column :disable)
;; (def-package-hook! magit :pre-config nil)
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

(pcase (getenv "GUI")
  ("t"
   ;; gui
   (setq helm-dash-browser-func 'browse-url-firefox))
  (_
   ;; terminal
   ))

(require 'server)
(setq server-name (getenv "EMACS_SERVER_NAME"))
(if (not server-name) (setq server-name "server"))
(unless (server-running-p server-name)
  (server-start))

(setq save-interprogram-paste-before-kill t
      split-height-threshold nil
      split-width-threshold 0
      doom-font (font-spec :family "Ubuntu Mono" :size 18)
      ;; fix start-process xdg-open
      process-connection-type nil)
