;;; private/amos/init.el -*- lexical-binding: t; -*-
(setq-default
 counsel-org-goto-display-style 'path
 counsel-org-goto-separator " ➜ "
 counsel-org-goto-face-style 'org
 show-trailing-whitespace t
 evil-ex-substitute-global t
 intent-tabs-mode t
 tab-always-indent t
 powerline-default-separator 'alternate
 find-file-visit-truename t
 fringes-outside-margins t
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
 ws-butler-keep-whitespace-before-point nil
 org-ref-open-pdf-function
 (lambda (fpath)
   (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))


(add-hook! '(doom-post-init-hook minibuffer-setup-hook) (setq-local show-trailing-whitespace nil))

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)
(setq +org-dir (expand-file-name "~/org/"))

(add-hook! :append 'org-load-hook (setq org-agenda-files (directory-files (concat +org-dir "todos/") t "\\.org$" t)))


(advice-add #'nlinum-mode :override #'ignore)
(advice-add #'eldoc-mode :override #'ignore)
(advice-add #'+org|update-cookies :override #'ignore)
(fset 'fringe-mode nil)

(after! centered-window-mode
  (defun amos-special-window-p (window)
    (let* ((buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (equal buffname "*doom*")
          (equal (with-current-buffer buffer major-mode) 'pdf-view-mode))))
  (push #'amos-special-window-p cwm-ignore-window-predicates))

(def-package-hook! org-bullets
  :pre-config nil)

(def-package-hook! cc-mode
  :post-config
  (setq c-tab-always-indent t)
  t)

(def-package-hook! magit
  :pre-config nil)

(def-package-hook! racer
  :pre-config
  (set! :jump 'rust-mode :definition #'racer-find-definition)
  (unless (file-exists-p racer-cmd)
    (warn "rust-mode: racer binary can't be found; auto-completion is disabled"))
  nil)

(def-package-hook! stripe-buffer
  :pre-init nil)

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
