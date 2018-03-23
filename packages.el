;; -*- no-byte-compile: t; -*-
;;; private/amos/packages.el
(package! vi-tilde-fringe :ignore t)
(package! stripe-buffer :ignore t)
(package! git-gutter-fringe :ignore t)
(package! visual-fill-column :ignore t)
(package! evil-numbers :ignore t)
(package! centered-window-mode :ignore t)

(package! chinese-yasdcv)
(package! git-gutter)
(package! narrow-reindent)
(package! fcitx)
(package! sdcv)
(package! counsel-dash)
(package! easy-hugo)
(package! link-hint)
(package! lispyville)
(package! move-text)
(package! ws-butler)
(package! fish-mode)
(package! evil-magit)
(package! dired-quick-sort)
(package! ov)
(package! unfill)
(package! yapfify)
(package! helm-make)
(package! gitignore-mode)
(package! magit-svn)
(package! smeargle)
(package! gitconfig-mode)
(package! gitattributes-mode)
(package! git-timemachine)
(package! adoc-mode)
(package! page-break-lines)
(package! go-playground)
(package! rust-playground)
(package! pdf-tools)
(package! bind-map)
(package! kurecolor)
(package! cquery)
(package! lsp-mode)
(package! company-lsp)
(package! esup)

(package! realign-mode :recipe (:fetcher github :repo "amosbird/realign-mode.el"))
(package! rainbow-mode :recipe (:fetcher github :repo "amosbird/rainbow-mode"))
(package! evil-terminal-cursor-changer :recipe (:fetcher github :repo "amosbird/evil-terminal-cursor-changer"))
(package! evil-textobj-line :recipe (:fetcher github :repo "syohex/evil-textobj-line"))
(package! font-lock+ :recipe (:fetcher github :repo "emacsmirror/font-lock-plus"))
(package! help-fns+ :recipe (:fetcher github :repo "emacsmirror/help-fns-plus"))
(package! hl-line+ :recipe (:fetcher github :repo "emacsmirror/hl-line-plus"))
(package! osc :recipe (:fetcher github :repo "amosbird/osc.el"))
(package! ivy-rich :recipe (:fetcher github :repo "Yevgnen/ivy-rich"))
(package! dired-hacks :recipe (:fetcher github :repo "Fuco1/dired-hacks"))
(package! dired-k :recipe (:fetcher github :repo "amosbird/emacs-dired-k"))
(package! cc-playground :recipe (:fetcher github
                                          :repo "amosbird/cc-playground"
                                          :files ("*.el" "templates")))
(package! google-translate :recipe (:fetcher github :repo "atykhonov/google-translate"))
(package! direnv :recipe (:fetcher github :repo "wbolster/emacs-direnv"))
