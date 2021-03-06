;;; ../.local/@lab/etc/custom.el -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Zathura")
     (output-html "xdg-open")))
 '(auto-revert-interval 0.3)
 '(auto-revert-verbose nil)
 '(auto-save-visited-interval 5)
 '(auto-save-visited-mode nil)
 '(bibtex-completion-browser-function 'browser-url-chromium)
 '(bibtex-completion-pdf-open-function (lambda (fpath) (call-process "zathura" nil 0 nil fpath)))
 '(browse-url-chrome-program (expand-file-name "~/scripts/vivaldi"))
 '(browse-url-firefox-program "luakit")
 '(browse-url-mailto-function 'mu4e~compose-browse-url-mail)
 '(counsel-org-goto-display-style 'path)
 '(counsel-org-goto-face-style 'org)
 '(counsel-org-goto-separator " ➜ ")
 '(counsel-org-headline-display-style 'path)
 '(counsel-org-headline-path-separator " ➜ ")
 '(cquery-cache-dir "/home/amos/.cache/.cquery_cached_index/")
 '(cquery-executable "/home/amos/git/cquery/build/release/bin/cquery")
 '(cquery-extra-args '("--log-file=/tmp/cq.log"))
 '(cquery-extra-init-params
   '(:client
     (:snippetSupport t)
     :index
     (:comments 0)
     (:whitelist
      ("./dbms" "./libs"))
     (:blacklist
      ("/home/amos/git/ClickHouse/.*"))))
 '(cquery-project-root-matchers
   '(cquery-project-roots-matcher ".cquery" projectile-project-root "compile_commands.json"))
 '(cquery-sem-highlight-method 'overlay)
 '(custom-safe-themes
   '("554b7f0439155d6eb648d4837ef03902f51124cacee021217e76f39e9dd314c2" "d0404bd38534a00ee72a4f887a987d6bff87f4cf8d8f85149e32849b262465a5" "73e35ffa5ca98b57a9923954f296c3854ce6d8736b31fdbdda3d27502d4b4d69" "0a3a41085c19d8121ed0ad3eb658a475ccb948a70a83604641ee7d4c3575a4d5" "a7e7804313dbf827a441c86a8109ef5b64b03011383322cbdbf646eb02692f76" "77bddca0879cb3b0ecdf071d9635c818827c57d69164291cb27268ae324efa84" "2e1d19424153d41462ad31144549efa41f55dacda9b76571f73904612b15fd0a" default))
 '(dired-omit-verbose nil)
 '(dired-open-extensions
   '(("pdf" . "xdg-open")
     ("ps" . "xdg-open")
     ("jpg" . "xdg-open")
     ("iso" . "xdg-open")
     ("callgrind.out.*" . "kcachegrind")
     ("doc" . "xdg-open")
     ("docx" . "xdg-open")
     ("html" . "xdg-open")
     ("xlsx" . "xdg-open")
     ("xls" . "xdg-open")
     ("odt" . "xdg-open")
     ("ppt" . "xdg-open")
     ("mkv" . "xdg-open")
     ("pptx" . "xdg-open")
     ("torrent" . "xdg-open")))
 '(dired-open-find-file-function #'+amos/find-file)
 '(dired-recursive-copies 'always)
 '(display-line-numbers-current-absolute nil)
 '(display-line-numbers-type 'visual)
 '(evil-cjk-emacs-word-boundary t)
 '(evil-esc-delay 0.001)
 '(evil-ex-substitute-global t)
 '(evil-kill-on-visual-paste nil)
 '(evil-shift-round nil)
 '(evil-shift-width 4)
 '(evil-snipe-auto-scroll nil)
 '(evil-snipe-scope 'visible)
 '(explicit-shell-file-name "/bin/bash")
 '(find-file-visit-truename t)
 '(flycheck-checkers
   '(ledger ada-gnat asciidoctor asciidoc cfengine chef-foodcritic coffee coffee-coffeelint coq css-csslint css-stylelint cwl d-dmd dockerfile-hadolint elixir-dogma emacs-lisp emacs-lisp-checkdoc erlang-rebar3 erlang eruby-erubis fortran-gfortran go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck groovy haml handlebars haskell-stack-ghc haskell-ghc haskell-hlint html-tidy javascript-eslint javascript-jshint javascript-standard json-jsonlint json-python-json jsonnet less less-stylelint llvm-llc lsp-ui lua-luacheck lua perl perl-perlcritic php php-phpmd php-phpcs processing proselint protobuf-protoc pug puppet-parser puppet-lint python-flake8 python-pylint python-pycompile r-lintr racket rpm-rpmlint markdown-markdownlint-cli markdown-mdl nix rst-sphinx rst ruby-rubocop ruby-reek ruby-rubylint ruby ruby-jruby rust-cargo rust rust-clippy scala scala-scalastyle scheme-chicken scss-lint scss-stylelint sass/scss-sass-lint sass scss sh-bash sh-posix-dash sh-posix-bash sh-zsh sh-shellcheck slim slim-lint sql-sqlint systemd-analyze tcl-nagelfar tex-chktex tex-lacheck texinfo typescript-tslint verilog-verilator vhdl-ghdl xml-xmlstarlet xml-xmllint yaml-jsyaml yaml-ruby))
 '(flycheck-pos-tip-mode nil)
 '(fringes-outside-margins t t)
 '(global-auto-revert-non-file-buffers t)
 '(helm-bibtex-bibliography '("~/zotero.bib"))
 '(helm-bibtex-notes-path "~/bibnotes.org")
 '(helm-bibtex-pdf-field "file")
 '(initial-buffer-choice t)
 '(intent-tabs-mode t)
 '(ivy-do-completion-in-region nil)
 '(ivy-fixed-height-minibuffer t)
 '(ivy-format-function 'ivy-format-function-line)
 '(ivy-height 12)
 '(ivy-magic-slash-non-match-action nil)
 '(ivy-mode t)
 '(ivy-rich-abbreviate-paths t)
 '(ivy-rich-switch-buffer-align-virtual-buffer t)
 '(ivy-rich-switch-buffer-delimiter "|")
 '(ivy-use-selectable-prompt t)
 '(ivy-use-virtual-buffers nil)
 '(ivy-virtual-abbreviate 'full)
 '(lsp-project-whitelist '("^/home/amos/git/ClickHouse/$"))
 '(lsp-ui-peek-force-fontify t)
 '(magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
 '(mode-line-format nil)
 '(nav-flash-delay 0.3)
 '(org-M-RET-may-split-line '((default)))
 '(org-agenda-files '("~/org/todo.org"))
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
      "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1)) t)
 '(org-goto-interface 'outline-path-completion)
 '(org-html-text-markup-alist
   '((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<strong style=\"color : red;\">%s</strong>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>")))
 '(org-hugo-default-section-directory "post")
 '(org-latex-compiler "xelatex" t)
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
 '(org-outline-path-complete-in-steps nil)
 '(org-preview-latex-default-process 'imagemagick)
 '(org-ref-bibliography-notes "~/Papers/notes.org")
 '(org-ref-default-bibliography '("~/Papers/references.bib"))
 '(org-ref-open-pdf-function
   (lambda
     (fpath)
     (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))
 '(org-ref-pdf-directory "~/Papers/")
 '(org-src-block-faces '(("c++" default)))
 '(org-src-tab-acts-natively t)
 '(org-twbs-text-markup-alist
   '((bold . "<b>%s</b>")
     (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<strong style=\"color : red;\">%s</strong>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>")))
 '(osc-http-addr "172.26.72.26:8866")
 '(package-check-signature nil)
 '(package-selected-packages
   '(yard-mode yapfify yaml-mode xref-js2 ws-butler which-key wgrep web-mode web-beautify vimrc-mode use-package unicode-fonts unfill toml-mode toc-org tide stylus-mode solaire-mode smex smeargle smartparens smart-forward slime skewer-mode shrink-path shackle sdcv sass-mode rust-playground ruby-refactor rspec-mode rotate-text rjsx-mode rake rainbow-mode rainbow-delimiters rainbow-blocks racer quickrun quelpa purescript-mode pug-mode psc-ide pip-requirements phpunit php-refactor-mode php-extras php-boris persp-mode perl6-mode pdf-tools pcre2el pangu-spacing page-break-lines ox-twbs ox-pandoc ox-hugo overseer ov osc org-plus-contrib org-mime org-bullets org-autolist opencl-mode notmuch nose nodejs-repl nlinum-relative nlinum-hl nav-flash nasm-mode narrow-reindent mu4e-maildirs-extension move-text moonscript modern-cpp-font-lock mips-mode markdown-toc makefile-executor magit-svn lispyville link-hint less-css-mode ledger-mode kurecolor julia-mode json-mode js2-refactor ivy-xref ivy-rich ivy-hydra ivy-bibtex hy-mode hl-todo hl-line+ highlight-quoted highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-make haxor-mode gorepl-mode google-translate go-playground go-guru go-eldoc glsl-mode gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-link git-gutter fringe-helper flyspell-correct-ivy flycheck-rust flycheck-pos-tip flycheck-plantuml flycheck-perl6 flycheck-ledger flycheck-elm flycheck-crystal flycheck-cask fish-mode fill-column-indicator fcitx eyebrowse evil-visualstar evil-vimish-fold evil-textobj-line evil-textobj-anyblock evil-terminal-cursor-changer evil-snipe evil-org evil-numbers evil-nerd-commenter evil-multiedit evil-mc evil-matchit evil-magit evil-ledger evil-indent-plus evil-exchange evil-escape evil-embrace evil-ediff evil-easymotion evil-args evil-anzu eslintd-fix ensime emmet-mode emacsql-sqlite elm-mode elfeed-org eldoc-eval editorconfig easy-hugo dumb-jump doom-themes dockerfile-mode disaster dired-quick-sort dired-k dired-hacks demangle-mode cuda-mode crystal-mode crux counsel-projectile counsel-dash counsel-css company-web company-tern company-statistics company-shell company-restclient company-racer company-quickhelp company-php company-lua company-lsp company-inf-ruby company-go company-glsl company-dict company-auctex company-anaconda command-log-mode coffee-mode cnfonts cmake-mode clj-refactor clang-format chinese-yasdcv centered-window-mode cc-playground bind-map auto-yasnippet auto-compile ag adoc-mode ace-link))
 '(password-cache-expiry nil)
 '(powerline-default-separator 'alternate)
 '(process-environment initial-environment t)
 '(projectile-find-dir-includes-top-level t)
 '(projectile-globally-ignored-directories
   '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "~/.emacs.d/.local/" ".sync" ".cquery_cached_index"))
 '(projectile-require-project-root t)
 '(projectile-sort-order 'recentf)
 '(query-replace-skip-read-only t)
 '(recentf-exclude
   '("^/tmp/" "^/ssh:" "\\.?ido\\.last$" "\\.revive$" "/TAGS$" "^/var/folders/.+$" "^/home/amos/Mail/" "^/home/amos/.emacs.d/.local/"))
 '(recentf-max-saved-items 10000)
 '(reftex-default-bibliography '("~/zotero.bib") t)
 '(require-final-newline t)
 '(safe-local-variable-values
   '((rpm-change-log-uses-utc . t)
     (eval c-set-offset 'arglist-close 0)
     (eval c-set-offset 'arglist-intro '++)
     (eval c-set-offset 'case-label 0)
     (eval c-set-offset 'statement-case-open 0)
     (eval c-set-offset 'substatement-open 0)
     (eval when
           (fboundp 'rainbow-mode)
           (rainbow-mode 1))
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (eval add-hook 'text-mode-hook
           (lambda nil
             (setq-local fill-column 70))
           :local)
     (eval progn
           (put 'defendpoint 'clojure-doc-string-elt 3)
           (put 'api/defendpoint 'clojure-doc-string-elt 3)
           (put 'defsetting 'clojure-doc-string-elt 2)
           (put 'setting/defsetting 'clojure-doc-string-elt 2)
           (put 's/defn 'clojure-doc-string-elt 2)
           (define-clojure-indent
             (api-let 2)
             (assert 1)
             (assoc 1)
             (auto-parse 1)
             (catch-api-exceptions 0)
             (check 1)
             (checkp 1)
             (context 2)
             (create-database-definition 1)
             (ex-info 1)
             (execute-query 1)
             (execute-sql! 2)
             (expect 0)
             (expect-with-all-engines 0)
             (expect-with-engine 1)
             (expect-with-engines 1)
             (let-400 1)
             (let-404 1)
             (let-500 1)
             (match 1)
             (match-$ 1)
             (merge-with 1)
             (post-select 1)
             (pre-delete 1)
             (pre-insert 1)
             (pre-update 1)
             (project 1)
             (qp-expect-with-engines 1)
             (render-file 1)
             (resolve-private-vars 1)
             (select 1)
             (sync-in-context 2)
             (when-testing-engine 1)
             (with-redefs-fn 1)))
     (mode . c++-mode)
     (mode . cc-playground-mode)))
 '(save-interprogram-paste-before-kill t)
 '(shell-file-name "/bin/bash")
 '(show-paren-priority -50)
 '(show-trailing-whitespace t)
 '(sp-escape-quotes-after-insert nil)
 '(split-height-threshold nil)
 '(split-width-threshold 0)
 '(swiper-include-line-number-in-search t)
 '(tab-always-indent t)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist '((".*" . "~/.emacs.d/undo-files")))
 '(user-full-name "Amos Bird")
 '(user-mail-address "amosbird@gmail.com")
 '(visible-cursor nil)
 '(warning-suppress-types '((yasnippet backquote-change)))
 '(ws-butler-keep-whitespace-before-point nil)
 '(yas-snippet-dirs
   '(+amos-snippets-dir +file-templates-dir "/home/amos/.emacs.d/snippets"))
 '(yas-triggers-in-field nil)
 '(yas-wrap-around-region 121)
 '(yasdcv-sdcv-command
   "sdcv --non-interactive --utf8-output --utf8-input \"%word\"")
 '(yasdcv-sdcv-dicts '(("jianminghy" "简明汉英词典" "powerword2007" t))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-minibuffer-match-face-1 ((t (:background "#1B2229" :weight bold))))
 '(ivy-virtual ((t (:inherit italic :foreground "white"))))
 '(org-hide ((t (:foreground "black" :height 1.0))))
 '(org-level-1 ((t (:inherit bold :foreground "#4f97d7" :height 1.0))))
 '(org-level-2 ((t (:inherit bold :foreground "#2d9574" :height 1.0))))
 '(org-level-3 ((t (:foreground "#67b11d" :weight normal :height 1.0)))))
