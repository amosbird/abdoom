;;; private/amos/init.el -*- lexical-binding: t; -*-

(setq doom-line-numbers-style 'relative
      doom-font (font-spec :family "Ubuntu Mono" :size 18)
      ;; fix start-process xdg-open
      process-connection-type nil)

(add-hook! '(doom-post-init-hook minibuffer-setup-hook) (setq-local show-trailing-whitespace nil))
(add-hook! 'edebug-mode-hook #'evil-normalize-keymaps)
;; (add-hook! 'minibuffer-setup-hook (setq-local truncate-lines t))
(add-hook! 'eval-expression-minibuffer-setup-hook
  (define-key minibuffer-local-map "\C-p" #'previous-line-or-history-element)
  (define-key minibuffer-local-map "\C-n" #'next-line-or-history-element))

(remove-hook! 'doom-popup-mode-hook #'doom|hide-modeline-in-popup)
(advice-add #'nlinum-mode :override #'ignore)
(advice-add #'doom-hide-modeline-mode :override #'ignore)
(advice-add #'fringe-mode :override #'ignore)
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

(after! subword
  (progn
    (define-category ?U "Uppercase")
    (define-category ?u "Lowercase")
    (modify-category-entry (cons ?A ?Z) ?U)
    (modify-category-entry (cons ?a ?z) ?u)
    (make-variable-buffer-local 'evil-cjk-word-separating-categories)
    (add-hook 'subword-mode-hook (lambda! (if subword-mode (push '(?u . ?U) evil-cjk-word-separating-categories)
                                        (setq evil-cjk-word-separating-categories (default-value 'evil-cjk-word-separating-categories)))))))

(remove-hook! 'doom-popup-mode-hook #'doom|hide-modeline-in-popup)
(advice-add #'nlinum-mode :override #'ignore)
(advice-add #'doom-hide-modeline-mode :override #'ignore)
(advice-add #'fringe-mode :override #'ignore)
(advice-add #'dired-k--highlight-by-file-attribyte :override #'ignore)
(advice-add #'recenter-top-bottom :override #'recenter)
(advice-add #'git-gutter:next-hunk :after (lambda (arg) (recenter)))
(def-package-hook! smerge-mode :disable)
(def-package-hook! solaire-mode :disable)
(def-package-hook! stripe-buffer :disable)
(def-package-hook! visual-fill-column :disable)
(def-package-hook! ivy
  :post-config
  (ivy-set-display-transformer #'ivy-switch-buffer nil)
  (ivy-set-display-transformer #'ivy-switch-buffer-other-window nil)
  (ivy-set-display-transformer #'+ivy/switch-workspace-buffer nil)
  t)

(def-package-hook! company
  :post-config
  (require 'company-tng)
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case t
        company-auto-complete nil
        company-dabbrev-code-everywhere t
        company-jedi-python-bin "python"
        company-frontends '(company-tng-frontend company-pseudo-tooltip-frontend company-echo-metadata-frontend)
        company-auto-complete nil)
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  t)

(require 'server)
(setq server-name (getenv "EMACS_SERVER_NAME"))
(if (not server-name) (setq server-name "server"))
(unless (server-running-p server-name)
  (server-start))
;; disable this fucking stupid feature by masking
(provide 'smartparens-lua)
