;;; private/amos/+bindings.el -*- lexical-binding: t; -*-

(map!
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil)

(mapc #'evil-declare-change-repeat
      '(
        ;; +amos/forward-word
        ;; +amos/backward-word
        ;; +amos/backward-subword
        ;; +amos/forward-subword
        ;; +amos/delete-word
        ;; +amos/delete-subword
        delete-char
        company-dabbrev-code
        +company/complete))

(defun doom/forward-to-last-non-comment-or-eol-insert ()
  (interactive)
  (if (eolp)
      (if (looking-back ";" 1)
          (funcall-interactively (key-binding (kbd "RET")))
      (insert ?\;))
    (doom/forward-to-last-non-comment-or-eol)))
(defun doom/backward-to-bol-or-indent-insert ()
  (interactive)
  (doom/backward-to-bol-or-indent))

(mapc #'evil-declare-ignore-repeat
      '(execute-extended-command
        text-scale-increase
        test-scale-reset
        text-scale-decrease
        doom/forward-to-last-non-comment-or-eol
        doom/backward-to-bol-or-indent
        doom/kill-this-buffer
        +workspace/close-workspace-or-frame
        +workspace/switch-to-1
        +workspace/switch-to-2
        +workspace/switch-to-3
        +workspace/switch-to-4
        +workspace/switch-to-5
        +workspace/switch-to-6
        +workspace/switch-to-7
        +workspace/switch-to-8
        +workspace/switch-to-9
        +workspace/switch-to-last
        +eval/buffer
        +eval/region-and-replace
        evil-switch-to-windows-last-buffer
        mark-whole-buffer
        +amos/counsel-jumpdir-function
        +amos:multi-next-line
        +amos:multi-previous-line
        yas-insert-snippet
        counsel-dash-at-point
        yasdcv-translate-at-point
        +amos/evil-visual-insert-snippet
        evil-window-left
        evil-window-down
        evil-window-up
        evil-window-right
        evil-multiedit-match-symbol-and-next
        evil-multiedit-match-symbol-and-prev
        evil-multiedit-match-and-next
        evil-multiedit-match-and-prev
        +amos/counsel-projectile-switch-project
        +amos:redisplay-and-recenter
        swiper
        counsel-projectile-rg
        +amos/counsel-rg-cur-dir
        evilem--motion-evil-find-char
        evilem--motion-evil-find-char-backward
        +amos/yank-buffer-filename-with-line-position
        bury-buffer
        evil-delete-line
        delete-char
        move-text-down
        move-text-up
        +amos/other-window
        evil-multiedit-match-all
        rotate-text
        undo-tree-undo
        undo-tree-redo
        er/expand-region
        er/contract-region
        next-error
        previous-error
        +amos@paste/evil-paste-after
        +amos@paste/evil-paste-before
        +amos:previous-open-delim
        +amos:next-close-delim
        +evil/visual-dedent
        +evil/visual-indent
        lsp-ui-mode
        +evil:macro-on-all-lines
        +amos:evil-find-file-at-point-with-line
        +jump/definition
        +amos/copy-and-comment-lines-inverse
        +amos/copy-and-comment-lines
        +jump/documentation
        +evil/reselect-paste
        +jump/references
        counsel-imenu
        +eval/buffer
        +amos/escape
        projectile-find-other-file))

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun +workspace/switch-to-1 () (interactive) (+workspace/switch-to 1))
(defun +workspace/switch-to-2 () (interactive) (+workspace/switch-to 2))
(defun +workspace/switch-to-3 () (interactive) (+workspace/switch-to 3))
(defun +workspace/switch-to-4 () (interactive) (+workspace/switch-to 4))
(defun +workspace/switch-to-5 () (interactive) (+workspace/switch-to 5))
(defun +workspace/switch-to-6 () (interactive) (+workspace/switch-to 6))
(defun +workspace/switch-to-7 () (interactive) (+workspace/switch-to 7))
(defun +workspace/switch-to-8 () (interactive) (+workspace/switch-to 8))
(defun +workspace/switch-to-9 () (interactive) (+workspace/switch-to 9))

(defun +amos/escape () (interactive)
       (evil-force-normal-state)
       (if buffer-file-name
           (save-buffer)))

(map!
 :n [escape]         #'+amos/escape
 :nvime "M-x"        #'execute-extended-command
 "M-+"               #'text-scale-increase
 "M-="               #'text-scale-reset
 "M-*"               #'text-scale-decrease
 "M-w"               #'evil-wipeout-buffer
 "M-W"               #'+workspace/close-workspace-or-frame
 "M-1"               #'+workspace/switch-to-1
 "M-2"               #'+workspace/switch-to-2
 "M-3"               #'+workspace/switch-to-3
 "M-4"               #'+workspace/switch-to-4
 "M-5"               #'+workspace/switch-to-5
 "M-6"               #'+workspace/switch-to-6
 "M-7"               #'+workspace/switch-to-7
 "M-8"               #'+workspace/switch-to-8
 "M-9"               #'+workspace/switch-to-9
 "M-0"               #'+workspace/switch-to-last
 "M-r"               #'+eval/buffer
 "M-R"               #'+eval/region-and-replace
 "M-m"               #'evil-switch-to-windows-last-buffer
 :nvmei "M-m"        #'evil-switch-to-windows-last-buffer
 :nvme "M-a"         #'mark-whole-buffer
 :ne "M-g"           #'+amos/counsel-jumpdir-function
 :i "M-i"            #'yas-insert-snippet
 :i "M-,"            (lambda! (insert ?:))
 :i "M-."            (lambda! (insert ?\;))
 :n  "M-n"           #'evil-multiedit-match-symbol-and-next
 :n  "M-N"           #'evil-multiedit-match-symbol-and-prev
 :v  "M-n"           #'evil-multiedit-match-and-next
 :v  "M-N"           #'evil-multiedit-match-and-prev
 :i  "M-n"           #'next-line
 :i  "M-p"           #'previous-line
 :n  "M-o"           #'lsp-ui-mode
 :vn "E"             #'+amos/forward-subword
 :vn "B"             #'+amos/backward-subword
 :ni "M-b"           (lambda! (if (not (eq evil-state 'insert)) (evil-insert 1)) (+amos/backward-word-insert))
 :ni "M-B"           (lambda! (if (not (eq evil-state 'insert)) (evil-insert 1)) (+amos/backward-subword-insert))
 :ni "M-f"           (lambda! (if (not (eq evil-state 'insert)) (evil-append 1)) (+amos/forward-word-insert))
 :ni "M-F"           (lambda! (if (not (eq evil-state 'insert)) (evil-append 1)) (+amos/forward-subword-insert))
 :ni "M-d"           (lambda! (if (not (eq evil-state 'insert)) (evil-insert 1)) (+amos/delete-word))
 :ni "M-D"           (lambda! (if (not (eq evil-state 'insert)) (evil-insert 1)) (+amos/delete-subword))
 :ni [M-backspace]   (lambda! (if (not (eq evil-state 'insert)) (evil-append 1)) (+amos/backward-delete-word))
 :ni [134217855]     (lambda! (if (not (eq evil-state 'insert)) (evil-append 1)) (+amos/backward-delete-word)) ; M-DEL
 :ni [M-S-backspace] (lambda! (if (not (eq evil-state 'insert)) (evil-append 1)) (+amos/backward-delete-subword))
 :i "M-r"            #'sp-slurp-hybrid-sexp
 :i "M-R"            #'sp-forward-barf-sexp
 :n "M-e"            #'counsel-dash-at-point
 :n "M-i"            #'yasdcv-translate-at-point
 :v "M-i"            #'+amos/evil-visual-insert-snippet
 :env "M-h"          #'evil-window-left
 :env "M-j"          #'evil-window-down
 :env "M-k"          #'evil-window-up
 :env "M-l"          #'evil-window-right
 "C-x 1"             #'zygospore-toggle-delete-other-windows
 "C-x e"             #'pp-eval-last-sexp
 "C-x C-r"           #'+amos/replace-last-sexp
 :env "C-p"          #'+amos/counsel-projectile-switch-project
 "C-l"               nil
 :nvm "C-l"          #'+amos:redisplay-and-recenter
 "C-s"               #'swiper
 :env "C-s"          #'swiper
 "C-S-s"             #'counsel-projectile-rg
 "C-S-d"             #'+amos/counsel-rg-cur-dir
 :m "C-f"            #'evilem--motion-evil-find-char
 :m "C-b"            #'evilem--motion-evil-find-char-backward
 :m "C-y"            #'+amos/yank-buffer-filename-with-line-position
 :m "C-w"            #'bury-buffer
 :nvm "0"            #'doom/backward-to-bol-or-indent
 :nvm "$"            #'doom/forward-to-last-non-comment-or-eol
 :nvm "-"            #'doom/forward-to-last-non-comment-or-eol
 :i "C-a"            #'doom/backward-to-bol-or-indent-insert
 :i "M-a"            #'doom/backward-to-bol-or-indent-insert
 :vn "C-a"           #'evil-numbers/inc-at-pt
 :v "g C-a"          #'evil-numbers/inc-at-pt-incremental
 :i "C-e"            #'doom/forward-to-last-non-comment-or-eol-insert
 :i "M-e"            #'doom/forward-to-last-non-comment-or-eol-insert
 :i "C-u"            #'doom/backward-kill-to-bol-and-indent
 :i [remap newline]  #'doom/newline-and-indent
 :i "C-o"            #'evil-delete-line
 :i "C-n"            #'next-line
 :i "C-p"            #'previous-line
 :i "C-d"            #'delete-char
 :i "C-j"            #'company-dabbrev-code
 :n "C-t"            nil
 :n "C-j"            #'move-text-down
 :n "C-k"            #'move-text-up
 :nv "C-SPC"         #'+amos/other-window
 :i "C-SPC"          #'+company/complete
 :v  "R"             #'evil-multiedit-match-all
 :n  "!"             #'rotate-text
 :v "u"              #'undo-tree-undo
 :v "C-r"            #'undo-tree-redo
 :n  "s"             #'evil-substitute
 :n  "S"             #'evil-change-whole-line
 :v  "s"             #'evil-surround-region
 :v  "S"             #'evil-substitute
 :o  "s"             #'evil-surround-edit
 :v  "v"             #'er/expand-region
 :v  "V"             #'er/contract-region
 :nm  "C-."          #'next-error
 :nm  "C-,"          #'previous-error
 :n "p"              #'+amos@paste/evil-paste-after
 :n "P"              #'+amos@paste/evil-paste-before
 :m "("              #'+amos:previous-open-delim
 :m ")"              #'+amos:next-close-delim
 :v  "<"             #'+evil/visual-dedent
 :v  ">"             #'+evil/visual-indent
 :v  "@"             #'+evil:macro-on-all-lines
 :n  "g@"            #'+evil:macro-on-all-lines
 :n  "gc"            #'evilnc-comment-or-uncomment-lines
 :n  "gx"            #'evil-exchange
 :n  "gl"            #'counsel-imenu
 :n  "gh"            #'lsp-ui-peek-find-references
 :n  "gf"            #'+amos:evil-find-file-at-point-with-line
 :m  "gd"            #'+jump/definition
 :m  "gy"            #'+amos/copy-and-comment-lines-inverse
 :m  "gY"            #'+amos/copy-and-comment-lines
 :m  "gh"            #'+jump/documentation
 :n  "go"            #'+amos/evil-insert-line-below
 :n  "gO"            #'+amos/evil-insert-line-above
 :n  "gp"            #'+evil/reselect-paste
 :n  "gr"            #'+jump/references
 :n  "gR"            #'+eval/buffer
 :v  "gR"            #'+eval:replace-region
 :n ",,"             #'projectile-find-other-file

 (:prefix "C-x"
   :nvime "u" #'link-hint-open-link
   "C-c"      #'+amos/tmux-detach
   "p"        #'doom/other-popup)

 (:prefix "C-c"
   "C-SPC" #'easy-hugo)

 ;; --- <leader> -------------------------------------
 (:leader
   ;; Most commonly used
   :desc "Find file in project"     :en "SPC" #'+ivy/switch-workspace-buffer
   :desc "Switch buffer"            :en "."   #'projectile-find-file
   :desc "Toggle last popup"        :en ","   #'counsel-recentf
   :desc "Toggle last popup"        :en "e"   #'shell-command
   :desc "Blink cursor line"        :en "DEL" #'doom/open-scratch-buffer
   :desc "Jump to bookmark"         :en "RET" #'eval-expression
   :desc "Ivy resume"               :en "r" #'ivy-resume

   ;; C-u is used by evil
   :desc "Universal argument"       :en "u"  #'universal-argument
   :desc "Save current file"        :en "w"  #'save-buffer
   :desc "Next diff hunk"           :env "j" #'git-gutter:next-hunk
   :desc "Previous diff hunk"       :env "k" #'git-gutter:previous-hunk
   :desc "Switch workspace buffer"  :en "b" #'switch-to-buffer

   (:desc "file" :prefix "f"
     :desc "File file"                 :en "f" #'find-file
     :desc "Sudo find file"            :en "F" #'doom/sudo-find-file
     :desc "Find file in project"      :en "/" #'projectile-find-file
     :desc "Find file from here"       :en "?" #'counsel-file-jump
     :desc "Find other file"           :en "a" #'projectile-find-other-file
     :desc "Open project editorconfig" :en "c" #'editorconfig-find-current-editorconfig
     :desc "Find file in dotfiles"     :en "d" #'+amos/find-in-dotfiles
     :desc "Delete current file"       :en "D" #'+evil:delete-this-file
     :desc "Find file in emacs.d"      :en "e" #'+amos/find-in-emacsd
     :desc "Browse emacs.d"            :en "E" #'doom/sudo-this-file
     :desc "Recent files"              :en "r" #'recentf-open-files
     :desc "Recent project files"      :en "R" #'+amos/rename-current-buffer-file
     :desc "Yank filename"             :en "y" #'+amos/yank-buffer-filename
     :desc "Yank filename"             :en "Y" #'+amos/yank-buffer-filename-nondir)

   (:desc "git" :prefix "g"
     :desc "Git status"        :en  "s" #'magit-status
     :desc "Git blame"         :en  "b" #'magit-blame
     :desc "Git time machine"  :en  "t" #'git-timemachine-toggle
     :desc "Git revert hunk"   :en  "r" #'git-gutter:revert-hunk
     :desc "Git revert buffer" :en  "R" #'vc-revert
     :desc "List gists"        :en  "g" #'+gist:list
     :desc "Next hunk"         :env "]" #'git-gutter:next-hunk
     :desc "Previous hunk"     :env "[" #'git-gutter:previous-hunk)

   (:desc "help" :prefix "h"
     :n "h" help-map
     :desc "Apropos"               :en "a" #'apropos
     :desc "Reload theme"          :en "R" #'doom/reload-theme
     :desc "Find library"          :en "l" #'find-library
     :desc "Toggle Emacs log"      :en "m" #'doom/popup-toggle-messages
     :desc "Command log"           :en "L" #'global-command-log-mode
     :desc "Describe function"     :en "f" #'describe-function
     :desc "Describe key"          :en "k" #'describe-key
     :desc "Describe char"         :en "c" #'describe-char
     :desc "Describe mode"         :en "M" #'describe-mode
     :desc "Describe variable"     :en "v" #'describe-variable
     :desc "Describe face"         :en "F" #'describe-face
     :desc "Describe DOOM setting" :en "s" #'doom/describe-setting
     :desc "Describe DOOM module"  :en "d" #'doom/describe-module
     :desc "Find definition"       :en "." #'+jump/definition
     :desc "Find references"       :en "/" #'+jump/references
     :desc "Find documentation"    :en "h" #'+jump/documentation
     :desc "What face"             :en "'" #'doom/what-face
     :desc "What minor modes"      :en ";" #'doom/what-minor-mode
     :desc "Info"                  :en "i" #'info
     :desc "Toggle profiler"       :en "p" #'doom/toggle-profiler)

   (:desc "notes" :prefix "n"
     :desc "Rust playground"       :en "r" #'rust-playground
     :desc "Go playground"         :en "g" #'go-playground
     :desc "C++ playground"        :en "c" #'cc-playground
     :desc "Elisp playground"      :en "e" #'+amos/new-empty-elisp-buffer
     :desc "Browse script"         :en "s" #'+amos/browse-script
     :desc "Browse org"            :en "o" #'+amos/browse-org
     :desc "Browse note"           :en "n" #'+amos/browse-note
     :desc "Browse mode notes"     :en "m" #'+org/browse-notes-for-major-mode
     :desc "Browse project notes"  :en "p" #'+org/browse-notes-for-project)

   (:desc "open" :prefix "o"
     :desc "Default browser"     :en  "b" #'browse-url-of-file
     :desc "Dired"               :en  "d" #'+amos/dired-jump
     :desc "REPL"                :en  "r" #'+eval/open-repl
     :v  "r" #'+eval:repl
     ;; applications
     :desc "APP: elfeed"  :en "E" #'=rss
     :desc "APP: email"   :en "m" #'=email
     :desc "APP: regex"   :en "X" #'=regex)

   (:desc "project" :prefix "p"
     :desc "Find file in project"    :en  "/" #'projectile-find-file
     :desc "Run cmd in project root" :env "!" #'projectile-run-shell-command-in-root
     :desc "Switch project"          :en  "p" #'projectile-switch-project
     :desc "Recent project files"    :en  "r" #'projectile-recentf
     :desc "List project tasks"      :en  "t" #'+ivy/tasks
     :desc "Pop scratch in project"  :en  "o" #'doom/open-project-scratch-buffer
     :desc "Invalidate cache"        :en  "x" #'projectile-invalidate-cache)

   (:desc "quit" :prefix "q"
     :desc "Quit"                    :en "q" #'+amos/prompt-kill-emacs
     :desc "Quit (forget session)"   :en "Q" #'+workspace/kill-session-and-quit)

   (:desc "snippets" :prefix "s"
     :desc "New snippet"           :en  "n" #'yas-new-snippet
     :desc "Insert snippet"        :env "i" #'yas-insert-snippet
     :desc "Find snippet for mode" :en  "s" #'yas-visit-snippet-file
     :desc "Find snippet"          :en  "S" #'+amos/find-in-snippets)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"               :en "s" #'flyspell-mode
     :desc "Flycheck"               :en "f" #'flycheck-mode
     :desc "Rainbow"                :en "r" #'rainbow-mode
     :desc "Truncate lines"         :en "l" #'toggle-truncate-lines
     :desc "Fullscreen"             :en "w" #'whitespace-mode
     :desc "Fullscreen"             :en "f" #'doom/toggle-fullscreen
     :desc "Indent guides"          :en "i" #'highlight-indentation-mode
     :desc "Indent guides (column)" :en "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"         :en "h" #'+impatient-mode/toggle
     :desc "Big mode"               :en "b" #'doom-big-font-mode
     :desc "Evil goggles"           :en "g" #'+evil-goggles/toggle))

 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "s"     #'company-ispell
   :i "C-s"   #'company-yasnippet
   :i "C-o"   #'company-capf
   :i "C-n"   #'company-dabbrev-code
   :i "C-p"   #'+company/dabbrev-code-previous)

 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"        nil
     "C-o"        #'company-search-kill-others
     "C-j"        #'company-select-next
     "C-k"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     "C-SPC"      #'company-complete-common
     "C-l"        #'company-complete-selection
     "C-h"        #'company-quickhelp-manual-begin
     [tab]        #'company-complete-common-or-cycle
     [backtab]    #'company-select-previous
     [escape]     (lambda! (company-abort) (evil-normal-state 1)))
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-j"        #'company-search-repeat-forward
     "C-k"        #'company-search-repeat-backward
     "C-s"        (lambda! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))

 (:after ivy
   (:map ivy-mode-map
     "C-o" #'evil-delete-line))

 (:after swiper
   (:map swiper-map
     "C-c o"    #'+ivy/wgrep-occur))

 ;; counsel
 (:after counsel
   (:map counsel-ag-map
     "C-c o"    #'+ivy/wgrep-occur  ; search/replace on results
     "C-i"      #'ivy-call-and-recenter
     "C-SPC"    #'counsel-git-grep-recenter   ; preview
     "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action)))

 ;; dired
 (:after dired
   (:map dired-mode-map
     "SPC"     nil
     "G"       nil
     "g"       nil
     "e"       nil
     "v"       nil
     "b"       nil
     "n"       nil
     "N"       nil
     "y"       nil
     "C-o"     nil
     "C-i"     nil
     :n "d"   #'dired-flag-file-deletion
     :n "y"   (lambda! (dired-ranger-copy nil))
     :n "c"   (lambda! (dired-ranger-copy t))
     :n "p"   #'dired-ranger-paste
     :n "E"   #'wdired-change-to-wdired-mode
     :n "r"   #'dired-ranger-move
     "f"      #'counsel-find-file
     "S"      #'hydra-dired-quick-sort/body
     :n "I"   #'dired-kill-subdir
     "j"      #'dired-next-line
     "k"      #'dired-previous-line
     "W"      (lambda! (dired-copy-filename-as-kill 0))
     :n "C-i" #'peep-dired-toggle
     :n "C-v" #'peep-dired-scroll-page-down
     "M-v"    #'peep-dired-scroll-page-up
     :n "Y"   #'+amos/dired-rsync
     :n "S"   #'hydra-dired-quick-sort/body
     :n "j"   #'+amos/evil-undefine
     :n "k"   #'+amos/evil-undefine
     :n "M-n" #'+amos/counsel-jumpfile-function
     :n "M-o" #'+amos/prev-history
     :n "M-i" #'+amos/next-history
     :n "h"   #'+amos/up-directory
     :n "l"   #'dired-open-file))

 ;; evil-magit
 (:after evil-magit
   :map (magit-status-mode-map magit-revision-mode-map)
   "SPC" nil
   :n "C-j" nil
   :n [tab] #'magit-section-toggle
   :n "C-k" nil)

 ;; evil-mc
 (:prefix "gz"
   :nv "m" #'evil-mc-make-all-cursors
   :nv "u" #'evil-mc-undo-all-cursors
   :nv "z" #'+evil/mc-make-cursor-here
   :nv "t" #'+evil/mc-toggle-cursors
   :nv "n" #'evil-mc-make-and-goto-next-cursor
   :nv "p" #'evil-mc-make-and-goto-prev-cursor
   :nv "N" #'evil-mc-make-and-goto-last-cursor
   :nv "P" #'evil-mc-make-and-goto-first-cursor
   :nv "d" #'evil-mc-make-and-goto-next-match
   :nv "D" #'evil-mc-make-and-goto-prev-match)
 (:after evil-mc
   :map evil-mc-key-map
   :nv "C-n" #'evil-mc-make-and-goto-next-cursor
   :nv "C-N" #'evil-mc-make-and-goto-last-cursor
   :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
   :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

 (:after evil-multiedit
   (:map evil-multiedit-state-map
     "M-d" #'evil-multiedit-match-and-next
     "M-D" #'evil-multiedit-match-and-prev
     "RET" #'evil-multiedit-toggle-or-restrict-region)
   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
     "C-n" #'evil-multiedit-next
     "C-p" #'evil-multiedit-prev))

 (:after flycheck
   :map flycheck-error-list-mode-map
   :n "C-j" #'flycheck-error-list-next-error
   :n "C-k" #'flycheck-error-list-previous-error
   :n "j"   #'flycheck-error-list-next-error
   :n "k"   #'flycheck-error-list-previous-error
   :n "RET" #'flycheck-error-list-goto-error
   :n "C-l" #'flycheck-error-list-goto-error)

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape]        #'keyboard-escape-quit
   ;; "C-c C-o"       #'+amos/swiper-replace
   "TAB"           #'ivy-call-and-recenter
   "M-z"           #'undo
   "M-j"           #'ivy-immediate-done
   "M-g"           #'+amos/ivy-complete-dir
   "C-k"           #'ivy-previous-line
   "C-j"           #'ivy-next-line
   "C-l"           #'ivy-alt-done
   "C-w"           #'ivy-yank-word
   "<M-backspace>" #'ivy-backward-kill-word
   "C-u"           #'ivy-kill-line
   "M-b"           #'backward-word
   "M-f"           #'forward-word
   :map ivy-occur-grep-mode-map
   "C-d" nil
   "d"   #'ivy-occur-delete-candidate)

 ;; yasnippet
 (:after yasnippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     "C-l"           #'yas-next-field
     [escape]        #'evil-normal-state
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     :i "C-l" yas-maybe-expand
     :v "<tab>" #'+snippets/expand-on-region))

 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "j" #'evil-textobj-anyparen-inner-block #'evil-textobj-anyparen-a-block
 :textobj "u" #'evil-textobj-anyquote-inner-block #'evil-textobj-anyquote-a-block
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down


 ;; --- Built-in plugins -----------------------------
 (:after debug
   ;; For elisp debugging
   :map debugger-mode-map
   :n "q"   (lambda! (top-level) (doom/kill-this-buffer))
   :n "RET" #'debug-help-follow
   :n "e"   #'debugger-eval-expression
   :n "n"   #'debugger-step-through
   :n "c"   #'debugger-continue)

 (:after edebug
   :map edebug-mode-map
   "SPC" nil
   "h"   nil
   :n "s"   #'edebug-step-mode
   :n "n"   #'edebug-next-mode
   :n "g"   #'edebug-go-mode
   :n "G"   #'edebug-Go-nonstop-mode
   :n "t"   #'edebug-trace-mode
   :n "T"   #'edebug-Trace-fast-mode
   :n "c"   #'edebug-continue-mode
   :n "C"   #'edebug-Continue-fast-mode

   ;;:n "f" 'edebug-forward not implemented
   :n "f"   #'edebug-forward-sexp
   :n "N"   #'edebug-goto-here

   :n "I"   #'edebug-instrument-callee
   :n "i"   #'edebug-step-in
   :n "o"   #'edebug-step-out

   ;; quitting and stopping
   :n "q"   #'top-level
   :n "Q"   #'edebug-top-level-nonstop
   :n "a"   #'abort-recursive-edit
   :n "S"   #'edebug-stop

   ;; breakpoints
   :n "b"   #'edebug-set-breakpoint
   :n "u"   #'edebug-unset-breakpoint
   :n "B"   #'edebug-next-breakpoint
   :n "x"   #'edebug-set-conditional-breakpoint
   :n "X"   #'edebug-set-global-break-condition

   ;; evaluation
   :n "r"        #'edebug-previous-result
   :n "e"        #'edebug-eval-expression
   :n "\C-x\C-e" #'edebug-eval-last-sexp
   :n "E"        #'edebug-visit-eval-list

   ;; views
   :n "w"        #'edebug-where
   :n "v"        #'edebug-view-outside ;; maybe obsolete??
   :n "p"        #'edebug-bounce-point
   :n "P"        #'edebug-view-outside ;; same as v
   :n "W"        #'edebug-toggle-save-windows

   ;; misc
   :n "?"        #'edebug-help
   :n "d"        #'edebug-backtrace
   :n "-"        #'negative-argument

   ;; statistics
   :n "="        #'edebug-temp-display-freq-count)

 (:map help-mode-map
   :n "C-o" #'help-go-back
   :n "C-i" #'help-go-forward
   :n "o"   #'ace-link-help
   :n "q"   #'quit-window
   :n "Q"   #'+ivy-quit-and-resume)

 (:after vc-annotate
   :map vc-annotate-mode-map
   :n "q"   #'kill-this-buffer
   :n "d"   #'vc-annotate-show-diff-revision-at-line
   :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
   :n "SPC" #'vc-annotate-show-log-revision-at-line
   :n "C-j" #'vc-annotate-next-revision
   :n "C-k" #'vc-annotate-prev-revision
   :n "TAB" #'vc-annotate-toggle-annotation-visibility
   :n "RET" #'vc-annotate-find-revision-at-line))

(after! profiler
  (map!
   (:map profiler-report-mode-map
     :nm "RET" #'profiler-report-expand-entry)))

(map! (:map input-decode-map
        "\035"    [escape]
        [S-iso-lefttab] [backtab]
        "\e[1;5B" [(control shift j)]
        "\e[1;5A" [(control shift d)]
        "\e[1;5C" [S-return]
        "\e[1;5D" [M-S-backspace]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      (:after cus-edit
        (:map custom-mode-map
          :n "q"    #'Custom-buffer-done))

      (:after view
        (:map view-mode-map
          :n "q"    #'View-quit))

      (:after image-mode
        (:map image-mode-map
          :n "q" #'quit-window))

      (:map key-translation-map
        "C-RET" [C-return]
        "C-1"   (kbd "1")
        "C-2"   (kbd "2")
        "C-3"   (kbd "3")
        "C-4"   (kbd "4")
        "C-5"   (kbd "5")
        "C-6"   (kbd "6")
        "C-7"   (kbd "7")
        "C-8"   (kbd "8")
        "C-9"   (kbd "9")
        "C-0"   (kbd "0")
        "M-`"   (kbd "C-S-s")
        "C-@"   (kbd "C-SPC")
        "C-^"   (kbd "C-,")
        "C-_"   (kbd "C-."))

      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             evil-ex-completion-map
             evil-ex-search-keymap
             read-expression-map)
        [escape] #'abort-recursive-edit
        "C-r" #'evil-paste-from-register
        "C-a" #'move-beginning-of-line
        "C-w" #'doom/minibuffer-kill-word
        "C-u" #'doom/minibuffer-kill-line
        "C-d" #'delete-char
        "M-b" #'backward-word
        "M-f" #'forward-word
        "M-d" #'kill-word
        "M-z" #'doom/minibuffer-undo)

      (:map messages-buffer-mode-map
        "M-;" #'eval-expression
        "A-;" #'eval-expression)

      (:map tabulated-list-mode-map
        [remap evil-record-macro] #'doom/popup-close-maybe))
