;;; private/amos/+bindings.el -*- lexical-binding: t; -*-

(mapc #'evil-declare-change-repeat
      '(company-dabbrev-code
        +company/complete))

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
        +amos/smart-jumper-backward
        +amos/smart-jumper-forward
        +amos/redisplay-and-recenter
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
        +amos/evil-find-file-at-point-with-line
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
        flycheck-next-error
        flycheck-previous-error
        +amos/projectile-find-other-file))

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun +workspace/switch-to-1 () (interactive) (+amos/tmux-select-window 1))
(defun +workspace/switch-to-2 () (interactive) (+amos/tmux-select-window 2))
(defun +workspace/switch-to-3 () (interactive) (+amos/tmux-select-window 3))
(defun +workspace/switch-to-4 () (interactive) (+amos/tmux-select-window 4))
(defun +workspace/switch-to-5 () (interactive) (+amos/tmux-select-window 5))
(defun +workspace/switch-to-6 () (interactive) (+amos/tmux-select-window 6))
(defun +workspace/switch-to-7 () (interactive) (+amos/tmux-select-window 7))
(defun +workspace/switch-to-8 () (interactive) (+amos/tmux-select-window 8))
(defun +workspace/switch-to-9 () (interactive) (+amos/tmux-select-window 9))

(defun +amos/tmux-switch-window-previous () (interactive) (+amos/tmux-switch-window))
(defun +amos/tmux-switch-window-next () (interactive) (+amos/tmux-switch-window t))

(defun +amos/projectile-find-other-file ()
  (interactive)
  (if (and (boundp 'cc-playground-mode) cc-playground-mode)
      (cc-switch-between-src-and-test)
    (projectile-find-other-file)))

(defun nop ()
  (interactive))

(map!
 :gme "<f12>"        (lambda! (evil-refresh-cursor)) ; also used to refresh terminal frames
 :g "M-x"            #'execute-extended-command
 :g "<f2>"           #'text-scale-increase
 :g "<f1>"           #'text-scale-reset
 :g "<f3>"           #'text-scale-decrease
 :gme "M-w"          #'evil-wipeout-buffer
 :g "M-W"            #'+workspace/close-workspace-or-frame
 :g "M-1"            #'+workspace/switch-to-1
 :g "M-2"            #'+workspace/switch-to-2
 :g "M-3"            #'+workspace/switch-to-3
 :g "M-4"            #'+workspace/switch-to-4
 :g "M-5"            #'+workspace/switch-to-5
 :g "M-6"            #'+workspace/switch-to-6
 :g "M-7"            #'+workspace/switch-to-7
 :g "M-8"            #'+workspace/switch-to-8
 :g "M-9"            #'+workspace/switch-to-9
 :g "M-r"            #'+eval/buffer
 :g "M-R"            #'+eval/region-and-replace
 :g "M-m"            #'evil-switch-to-windows-last-buffer
 :g "M-m"            #'evil-switch-to-windows-last-buffer
 :g "M-a"            #'+amos/mark-whole-buffer
 :ne "M-g"           #'+amos/counsel-jumpdir-function
 :i "M-i"            #'yas-insert-snippet
 :nm "M-,"           #'flycheck-previous-error
 :nm "M-."           #'flycheck-next-error
 :m  "M-p"           #'evil-multiedit-match-symbol-and-prev
 :m  "M-n"           #'evil-multiedit-match-symbol-and-next
 :i  "M-n"           #'next-line
 :i  "M-p"           #'previous-line
 :n  "M-o"           #'lsp-ui-mode
 :m  "N"             #'evil-ex-search-previous
 :m  "E"             #'+amos/evil-forward-subword-end
 :m  "B"             #'+amos/evil-backward-subword-begin
 :ni "M-b"           #'+amos/backward-word-insert
 :ni "M-B"           (lambda! (+amos/backward-word-insert t))
 :ni "M-f"           #'+amos/forward-word-insert
 :ni "M-F"           (lambda! (+amos/forward-word-insert t))
 :ni "M-d"           #'+amos/forward-delete-word
 :ni "M-D"           (lambda! (+amos/forward-delete-word t))
 :i "DEL"            #'+amos/backward-delete-char
 :ni [M-backspace]   #'+amos/backward-delete-word
 :i "C-w"           #'+amos/backward-delete-word
 :ni [134217855]     #'+amos/backward-delete-word ; M-DEL
 :ni "<f7>"          (lambda! (+amos/backward-delete-word t))
 :i "M-r"            #'sp-slurp-hybrid-sexp
 :i "M-R"            #'sp-forward-barf-sexp
 :n "M-e"            #'counsel-dash-at-point
 :n "M-i"            #'yasdcv-translate-at-point
 :v "M-i"            #'+amos/evil-visual-insert-snippet
 :gme "M-h"          #'evil-window-left
 :gme "M-j"          #'evil-window-down
 :gme "M-k"          #'evil-window-up
 :gme "M-l"          #'evil-window-right
 :g "C-x 1"          #'zygospore-toggle-delete-other-windows
 :g "C-x e"          #'pp-eval-last-sexp
 :g "C-x C-r"        #'+amos/replace-last-sexp
 :env "C-p"          #'+amos/counsel-projectile-switch-project
 :g "C-l"            nil
 :nvm "C-l"          #'+amos/redisplay-and-recenter
 :g "C-s"            #'swiper
 :env "C-s"          #'swiper
 :g "<f4>"           #'counsel-projectile-rg
 :g "C-S-s"          #'counsel-projectile-rg
 :g "<f5>"           #'+amos/counsel-rg-cur-dir
 :g "C-S-d"          #'+amos/counsel-rg-cur-dir
 :m "C-f"            #'evilem--motion-evil-find-char
 :m "C-b"            #'evilem--motion-evil-find-char-backward
 :m "C-y"            #'+amos/yank-buffer-filename-with-line-position
 :i "C-y"            (lambda! (let ((kill-ring my-kill-ring)) (yank)))
 :i "M-y"            (lambda! (let ((kill-ring my-kill-ring)) (yank-pop)))
 :m "C-w"            #'bury-buffer
 :i "C-a"            #'evil-beginning-of-line
 :n "C-a"            #'evil-numbers/inc-at-pt
 :v "C-a"            #'+amos/ca
 :v "g C-a"          #'+amos/gca
 :i "C-e"            #'+amos/smart-eol-insert
 :i "M-e"            #'+amos/smart-eol-insert
 :i "C-u"            #'+amos/backward-kill-to-bol-and-indent
 :i [remap newline]  #'doom/newline-and-indent
 :i "C-o"            #'+amos/kill-line
 :i "C-n"            #'next-line
 :i "C-p"            #'previous-line
 :i "C-d"            #'+amos/delete-char
 :i "C-j"            #'company-dabbrev-code
 :n "C-t"            nil
 :n "C-j"            #'move-text-down
 :n "C-k"            #'move-text-up
 :nv "C-SPC"         #'+amos/other-window
 :i "C-SPC"          #'+amos/complete
 :v "R"              #'evil-multiedit-match-all
 :n "!"              #'rotate-text
 :v "H"              #'+amos/align-repeat-left
 :v "L"              #'+amos/align-repeat-right
 :v "u"              #'undo-tree-undo
 :v "C-r"            #'undo-tree-redo
 :n "s"              #'evil-substitute
 :n "S"              #'evil-change-whole-line
 :v "s"              #'evil-surround-region
 :v "S"              #'evil-substitute
 :o "s"              #'evil-surround-edit
 :v "v"              #'er/expand-region
 :v "V"              #'er/contract-region
 :nm "C-."           #'+amos/tmux-switch-window-next
 :nm "C-,"           #'+amos/tmux-switch-window-previous
 :n "p"              #'+amos@paste/evil-paste-after
 :n "P"              #'+amos@paste/evil-paste-before
 :m "("              #'+amos/smart-jumper-backward
 :m ")"              #'+amos/smart-jumper-forward
 :v "<"              #'+evil/visual-dedent
 :v ">"              #'+evil/visual-indent
 :v "@"              #'+evil:macro-on-all-lines
 :n "g@"             #'+evil:macro-on-all-lines
 :n "gx"             #'evil-exchange
 :n "gf"             #'+amos/evil-find-file-at-point-with-line
 :m "gd"             #'+jump/definition
 :m "gh"             #'+jump/documentation
 :n "go"             #'+amos/evil-insert-line-below
 :n "gO"             #'+amos/evil-insert-line-above
 :n "gp"             #'+evil/reselect-paste
 :n "gr"             #'+jump/references
 :n "gR"             #'cquery/callers
 :v "gR"             #'+eval:replace-region
 :m "gy"             #'evil-commentary-yank
 :m "gc"             #'evil-commentary
 :m "gl"             #'evil-commentary-line
 :n ",,"             #'+amos/projectile-find-other-file

 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "s"     #'company-ispell
   :i "C-s"   #'company-yasnippet
   :i "C-o"   #'company-capf
   :i "C-n"   #'company-dabbrev-code
   :i "C-p"   #'+company/dabbrev-code-previous
   :nvime "u" #'link-hint-open-link
   "c"        #'+amos/tmux-new-window
   "k"        #'+amos/tmux-kill-window
   "o"        #'+amos/tmux-fork-window
   :nvime "r" #'+amos/tmux-source
   "C-c"      #'+amos/tmux-detach
   "p"        #'doom/other-popup)

 (:prefix "C-c"
   "C-SPC" #'easy-hugo)

 (:prefix "SPC"
   :desc "Switch buffer"                   :en "SPC" #'+ivy/switch-workspace-buffer
   :desc "Find file in project"            :en "."   #'+amos/projectile-find-file
   :desc "Find file in project (no cache)" :en ">"   (lambda! (projectile-invalidate-cache nil) (projectile-find-file))
   :desc "Find recent file"                :en ","   #'counsel-recentf
   :desc "Find recent file (no cache)"     :en "<"   (lambda! (recentf-cleanup) (counsel-recentf))
   :desc "Shell command"                   :en "e"   #'shell-command
   :desc "Blink cursor line"               :en "DEL" #'doom/open-scratch-buffer
   :desc "Elisp command"                   :en "RET" #'eval-expression
   :desc "Ivy resume"                      :en "r"   #'ivy-resume
   :desc "Universal argument"              :en "u"   #'universal-argument
   :desc "Save current file"               :en "w"   #'save-buffer
   :desc "Next diff hunk"                  :env "j"  #'git-gutter:next-hunk
   :desc "Previous diff hunk"              :env "k"  #'git-gutter:previous-hunk
   :desc "Switch workspace buffer"         :en "b"   #'switch-to-buffer

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
     :desc "C++ playground"        :en "c" (lambda! (+amos/tmux-new-window 'cc-playground))
     :desc "C++ playground"        :en "l" #'cc-playground-find-snippet
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

 (:after company
   (:map company-active-map
     ;; Don't interfere with `evil-delete-backward-word' in insert mode
     "C-w"        nil
     "C-v"        #'company-next-page
     "M-v"        #'company-previous-page
     "C-o"        #'company-search-kill-others
     "C-j"        #'company-select-next
     "C-k"        #'company-select-previous
     "C-h"        #'company-quickhelp-manual-begin
     "C-S-h"      #'company-show-doc-buffer
     "C-S-s"      #'company-search-candidates
     "C-s"        #'company-filter-candidates
     "C-SPC"      #'company-complete-common
     "C-h"        #'company-quickhelp-manual-begin
     "C-i"        #'company-complete-selection
     "RET"        nil
     "SPC"        nil
     [return]     nil
     [tab]        nil
     [backtab]    nil)
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-j"        #'company-search-repeat-forward
     "C-k"        #'company-search-repeat-backward
     "C-s"        (lambda! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))

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
     :g "SPC" nil
     :g "G"   nil
     :g "g"   nil
     :g "e"   nil
     :g "v"   nil
     :g "b"   nil
     :g "n"   nil
     :g "N"   nil
     :g "y"   nil
     :g "C-o" nil
     :g "C-i" nil
     :n "d"   #'dired-flag-file-deletion
     :n "y"   (lambda! (dired-ranger-copy nil))
     :n "c"   (lambda! (dired-ranger-copy t))
     :n "p"   #'dired-ranger-paste
     :n "E"   #'wdired-change-to-wdired-mode
     :n "r"   #'dired-ranger-move
     :g "f"   #'counsel-find-file
     :g "S"   #'hydra-dired-quick-sort/body
     :n "I"   #'dired-kill-subdir
     :g "j"   #'dired-next-line
     :g "k"   #'dired-previous-line
     :g "W"   (lambda! (dired-copy-filename-as-kill 0))
     :g "C-f" #'dired-omit-mode
     :n "C-i" #'peep-dired-toggle
     :n "C-v" #'peep-dired-scroll-page-down
     :g "M-v" #'peep-dired-scroll-page-up
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
   :g "SPC" nil
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
   :map ivy-mode-map
   [remap find-file-other-frame] #'+amos/find-file-other-frame
   "C-y"                         (lambda! (let ((kill-ring my-kill-ring)) (yank)))
   "M-y"                         (lambda! (let ((kill-ring my-kill-ring)) (yank-pop)))
   "C-r"                         #'evil-paste-from-register
   "C-a"                         #'move-beginning-of-line
   "M-b"                         #'+amos/backward-word-insert
   "M-B"                         (lambda! (+amos/backward-word-insert t))
   "M-f"                         #'+amos/forward-word-insert
   "M-F"                         (lambda! (+amos/forward-word-insert t))
   "M-d"                         #'+amos/forward-delete-word
   "M-D"                         (lambda! (+amos/forward-delete-word t))
   "C-w"                         #'ivy-yank-word
   "C-u"                         #'+amos/backward-kill-to-bol-and-indent
   "C-d"                         #'+amos/delete-char
   "C-o"                         #'+amos/kill-line

   :map ivy-minibuffer-map
   [escape]                      #'keyboard-escape-quit
   "C-o"                         #'+amos/kill-line
   "TAB"                         #'ivy-call-and-recenter
   "M-z"                         #'undo
   "M-j"                         #'ivy-immediate-done
   "M-g"                         #'+amos/ivy-complete-dir
   "C-k"                         #'ivy-previous-line
   "C-j"                         #'ivy-next-line
   "C-l"                         #'ivy-alt-done
   "C-w"                         #'ivy-yank-word
   "M-r"                         #'ivy-toggle-fuzzy
   "C-u"                         #'ivy-kill-line
   "M-b"                         #'backward-word
   "M-f"                         #'forward-word

   :map ivy-occur-grep-mode-map
   "C-d" nil
   "d"                           #'ivy-occur-delete-candidate)

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


 ;; --- Built-in plugins -----------------------------
 (:after debug
   ;; For elisp debugging
   :map debugger-mode-map
   :n "q"   (lambda! (top-level) (doom/kill-this-buffer))
   :n "RET" #'debug-help-follow
   :n "e"   #'debugger-eval-expression
   :n "n"   #'debugger-step-through
   :n "c"   #'debugger-continue)

 (:after compile
   :map compilation-mode-map
   "SPC" nil)

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

 (:after org
   (:map org-mode-map
     :vn "RET"   #'org-open-at-point
     :en "M-h"   #'evil-window-left
     :en "M-j"   #'evil-window-down
     :en "M-k"   #'evil-window-up
     :en "M-l"   #'evil-window-right
     :en "C-j"   #'org-metadown
     :en "C-k"   #'org-metaup
     :i  "C-d"   #'delete-char
     :i  "DEL"   #'org-delete-backward-char
     :n  "gj"   #'evil-next-visual-line
     :n  "gk"   #'evil-previous-visual-line
     :n  "M-a"   #'+amos/mark-whole-buffer
     :g "C-c e"     #'+amos/org-babel-edit
     :g "C-c C-j"   #'counsel-org-goto
     :g "C-c C-S-l" #'+org/remove-link))

 (:after org-agenda
   (:map org-agenda-mode-map
     :e "<escape>" #'org-agenda-Quit
     :e "m"   #'org-agenda-month-view
     :e "C-j" #'org-agenda-next-item
     :e "C-k" #'org-agenda-previous-item
     :e "C-n" #'org-agenda-next-item
     :e "C-p" #'org-agenda-previous-item))

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
   :n "RET" #'vc-annotate-find-revision-at-line)

 (:after pdf-tools
   :map pdf-view-mode-map
   "0"         #'image-bol
   "$"         #'image-eol
   "j"         #'pdf-view-next-line-or-next-page
   "k"         #'pdf-view-previous-line-or-previous-page
   "l"         #'image-forward-hscroll
   "h"         #'image-backward-hscroll
   "J"         #'pdf-view-next-page
   "K"         #'pdf-view-previous-page
   "gg"        #'pdf-view-first-page
   "G"         #'pdf-view-last-page
   "gt"        #'pdf-view-goto-page
   "gl"        #'pdf-view-goto-label
   "u"         #'pdf-view-scroll-down-or-previous-page
   "d"         #'pdf-view-scroll-up-or-next-page
   "C-u"       #'pdf-view-scroll-down-or-previous-page
   "C-d"       #'pdf-view-scroll-up-or-next-page
   "``"        #'pdf-history-backward
   "/"         #'isearch-forward
   "?"         #'isearch-backward
   "r"         #'pdf-view-revert-buffer
   "o"         #'pdf-links-action-perform
   "O"         #'pdf-outline
   "zr"        #'pdf-view-scale-reset

   :map    pdf-outline-buffer-mode-map
   "-"         #'negative-argument
   "j"         #'next-line
   "k"         #'previous-line
   "gk"        #'outline-backward-same-level
   "gj"        #'outline-forward-same-level
   "<backtab>" #'show-all
   "gh"        #'pdf-outline-up-heading
   "gg"        #'beginning-of-buffer
   "G"         #'pdf-outline-end-of-buffer
   "TAB"       #'outline-toggle-children
   "RET"       #'pdf-outline-follow-link
   "M-RET"     #'pdf-outline-follow-link-and-quit
   "f"         #'pdf-outline-display-link
   [mouse-1]   #'pdf-outline-mouse-display-link
   "o"         #'pdf-outline-select-pdf-window
   "``"        #'pdf-outline-move-to-current-page
   "''"        #'pdf-outline-move-to-current-page
   "Q"         #'pdf-outline-quit-and-kill
   "q"         #'quit-window
   "F"         #'pdf-outline-follow-mode

   :map   pdf-annot-list-mode-map
   "f"         #'pdf-annot-list-display-annotation-from-id
   "d"         #'tablist-flag-forward
   "x"         #'tablist-do-flagged-delete
   "u"         #'tablist-unmark-forward
   "q"         #'tablist-quit)

 (:after  profiler
   (:map profiler-report-mode-map
     :nm "RET" #'profiler-report-expand-entry))

 (:after cus-edit
   (:map custom-mode-map
     :n "q" #'Custom-buffer-done))

 (:after view
   (:map view-mode-map
     :n "q" #'View-quit))

 (:after image-mode
   (:map image-mode-map
     :n "q" #'quit-window))

 (:after evil-snipe
   (:map evil-snipe-parent-transient-map
     :g "n" #'evil-snipe-repeat
     :g "j" #'evil-snipe-repeat
     :g "k" #'evil-snipe-repeat-reverse
     :g "N" #'evil-snipe-repeat-reverse
     :g "p" #'evil-snipe-repeat-reverse))

 (:map key-translation-map
   "\035"    [escape]
   [S-iso-lefttab] [backtab]
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
   "C-@"   (kbd "C-SPC"))

 (:map (minibuffer-local-map
        minibuffer-local-ns-map
        minibuffer-local-completion-map
        minibuffer-local-must-match-map
        minibuffer-local-isearch-map
        evil-ex-completion-map
        evil-ex-search-keymap
        read-expression-map)
   [escape]      #'abort-recursive-edit
   "C-y"         (lambda! (let ((kill-ring my-kill-ring)) (yank)))
   "M-y"         (lambda! (let ((kill-ring my-kill-ring)) (yank-pop)))
   "C-r"         #'evil-paste-from-register
   "C-a"         #'move-beginning-of-line
   "M-b"         #'+amos/backward-word-insert
   "M-B"         (lambda! (+amos/backward-word-insert t))
   "M-f"         #'+amos/forward-word-insert
   "M-F"         (lambda! (+amos/forward-word-insert t))
   "M-d"         #'+amos/forward-delete-word
   "M-D"         (lambda! (+amos/forward-delete-word t))
   "DEL"         #'+amos/backward-delete-char
   [M-backspace] #'+amos/backward-delete-word
   [134217855]   #'+amos/backward-delete-word ; M-DEL
   "C-w"         #'ivy-yank-word
   "C-u"         #'+amos/backward-kill-to-bol-and-indent
   "C-k"         #'+amos/kill-line
   "C-o"         #'+amos/kill-line
   "C-d"         #'+amos/delete-char
   "M-z"         #'doom/minibuffer-undo)

 (:map messages-buffer-mode-map
   "M-;" #'eval-expression
   "A-;" #'eval-expression)

 (:map tabulated-list-mode-map
   [remap evil-record-macro] #'doom/popup-close-maybe)

 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "j" #'+amos/any-object-inner            #'+amos/any-object-outer
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down)
