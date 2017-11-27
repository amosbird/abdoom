;;; private/amos/+bindings.el -*- lexical-binding: t; -*-

(defmacro find-file-in! (path &optional project-p)
  "Returns an interactive function for searching files."
  `(lambda () (interactive)
     (let ((default-directory ,path))
       (call-interactively
        ',(command-remapping
           (if project-p
               #'projectile-find-file
             #'find-file))))))

(map!
 [remap evil-jump-to-tag] #'projectile-find-tag
 [remap find-tag]         #'projectile-find-tag
 ;; ensure there are no conflicts
 :nmvo doom-leader-key nil
 :nmvo doom-localleader-key nil)

(map!
 ;; --- Global keybindings ---------------------------
 ;; Make M-x available everywhere
 :nvime "M-x"       #'execute-extended-command
 :nvime "A-x"       #'execute-extended-command
 ;; Emacs debug utilities
 "M-;"              #'eval-expression
 :nvime "M-;"       #'eval-expression
 "M-:"              #'doom/open-scratch-buffer
 :nvime "M-:"       #'doom/open-scratch-buffer
 ;; Text-scaling
 "M-+"              (λ! (text-scale-set 0))
 "M-="              #'text-scale-increase
 "M--"              #'text-scale-decrease
 ;; Simple window navigation/manipulation
 "C-`"              #'doom/popup-toggle
 "C-~"              #'doom/popup-raise
 "M-w"              #'delete-window
 "M-W"              #'+workspace/close-workspace-or-frame
 "M-n"              #'evil-buffer-new
 "M-N"              #'make-frame
 "M-1"              (λ! (+workspace/switch-to 0))
 "M-2"              (λ! (+workspace/switch-to 1))
 "M-3"              (λ! (+workspace/switch-to 2))
 "M-4"              (λ! (+workspace/switch-to 3))
 "M-5"              (λ! (+workspace/switch-to 4))
 "M-6"              (λ! (+workspace/switch-to 5))
 "M-7"              (λ! (+workspace/switch-to 6))
 "M-8"              (λ! (+workspace/switch-to 7))
 "M-9"              (λ! (+workspace/switch-to 8))
 "M-0"              #'+workspace/switch-to-last
 ;; Other sensible, textmate-esque global bindings
 "M-r"              #'+eval/buffer
 "M-R"              #'+eval/region-and-replace
 "M-b"              #'+eval/build
 :nvmei "M-a"       #'mark-whole-buffer
 "M-e"              #'counsel-dash-at-point
 "M-m"              #'evil-switch-to-windows-last-buffer
 :ne "M-g"          #'+amos/counsel-jumpdir-function
 "M-w"              #'delete-other-windows
 "C-x e"            #'pp-eval-last-sexp
 "C-l"              #'+amos:redisplay-and-recenter
 "C-s"              #'swiper
 "C-S-s"            #'counsel-projectile-rg
 :m "C-f"           #'evilem--motion-evil-find-char
 :m "C-b"           #'evilem--motion-evil-find-char-backward
 :m "C-y"           #'+amos/yank-buffer-filename-with-line-position
 :m "C-w"           #'kill-this-buffer
 :m "M-j"           #'+amos:multi-next-line
 :m "M-k"           #'+amos:multi-previous-line
 :i "M-i"           #'yas-insert-snippet
 :i "M-d"           #'kill-word
 :i "C-o"           #'kill-line
 :i "C-d"           #'delete-char
 :i "C-j"           #'company-dabbrev-code
 :i "M-r"           #'sp-slurp-hybrid-sexp
 :i "M-R"           #'sp-forward-barf-sexp
 :n "M-i"           #'yasdcv-translate-at-point
 :v "M-i"           #'+amos/evil-visual-insert-snippet
 :n "C-t" nil
 :n "C-j"           #'move-text-down
 :n "C-k"           #'move-text-up
 :n "p"             #'+amos@paste/evil-paste-after
 :n "P"             #'+amos@paste/evil-paste-before

 :nv "C-SPC"        #'+amos/other-window
 :m "("             #'+amos:previous-open-delim
 :m ")"             #'+amos:next-close-delim

 ;; Easier window navigation
 :env "M-h"          #'evil-window-left
 :env "M-j"          #'evil-window-down
 :env "M-k"          #'evil-window-up
 :env "M-l"          #'evil-window-right

 :n ",,"            #'projectile-find-other-file

 (:prefix "C-x"
   :nvime "u" #'link-hint-open-link
   "C-c"      #'+amos/tmux-detach
   "p"        #'doom/other-popup)

 (:prefix "C-c"
   "C-SPC" #'easy-hugo)


 ;; --- <leader> -------------------------------------
 (:leader
   :desc "Rg current directory" :nv "\\"  #'+amos/counsel-rg-cur-dir
   :desc "Rg current directory" :nv "|"  #'+amos/counsel-rg-cur-dir
   :desc "Ex command"  :nv ";"   #'evil-ex
   :desc "M-x"         :nv ":"   #'execute-extended-command
   :desc "Pop up scratch buffer"   :nv "x"  #'doom/open-scratch-buffer
   :desc "Org Capture"             :nv "X"  #'+org-capture/open

   ;; Most commonly used
   :desc "Find file in project"    :n "SPC" #'switch-to-buffer
   :desc "Switch workspace buffer" :n ","   #'switch-to-buffer
   :desc "Switch buffer"           :n "."   #'projectile-find-file
   :desc "Toggle last popup"       :n "~"   #'doom/popup-toggle
   :desc "Eval expression"         :n "`"   #'eval-expression
   :desc "Blink cursor line"       :n "DEL" #'+doom/blink-cursor
   :desc "Jump to bookmark"        :n "RET" #'bookmark-jump

   ;; C-u is used by evil
   :desc "Universal argument"    :n "u"  #'universal-argument
   :desc "Save current file"     :n "w"  #'save-buffer

   (:desc "previous..." :prefix "["
     :desc "Text size"           :nv "[" #'text-scale-decrease
     :desc "Buffer"              :nv "b" #'doom/previous-buffer
     :desc "Diff Hunk"           :nv "d" #'git-gutter:previous-hunk
     :desc "Todo"                :nv "t" #'hl-todo-previous
     :desc "Error"               :nv "e" #'previous-error
     :desc "Workspace"           :nv "w" #'+workspace/switch-left
     :desc "Smart jump"          :nv "h" #'smart-backward
     :desc "Spelling error"      :nv "s" #'evil-prev-flyspell-error
     :desc "Spelling correction" :n  "S" #'flyspell-correct-previous-word-generic)

   (:desc "next..." :prefix "]"
     :desc "Text size"           :nv "]" #'text-scale-increase
     :desc "Buffer"              :nv "b" #'doom/next-buffer
     :desc "Diff Hunk"           :nv "d" #'git-gutter:next-hunk
     :desc "Todo"                :nv "t" #'hl-todo-next
     :desc "Error"               :nv "e" #'next-error
     :desc "Workspace"           :nv "w" #'+workspace/switch-right
     :desc "Smart jump"          :nv "l" #'smart-forward
     :desc "Spelling error"      :nv "s" #'evil-next-flyspell-error
     :desc "Spelling correction" :n  "S" #'flyspell-correct-word-generic)

   (:desc "search" :prefix "/"
     :desc "Swiper"                :nv "/" #'swiper
     :desc "Imenu"                 :nv "i" #'imenu
     :desc "Imenu across buffers"  :nv "I" #'imenu-anywhere)

   (:desc "workspace" :prefix "TAB"
     :desc "Display tab bar"          :n "TAB" #'+workspace/display
     :desc "New workspace"            :n "n"   #'+workspace/new
     :desc "Load workspace from file" :n "l"   #'+workspace/load
     :desc "Load last session"        :n "L"   (λ! (+workspace/load-session))
     :desc "Save workspace to file"   :n "s"   #'+workspace/save
     :desc "Autosave current session" :n "S"   #'+workspace/save-session
     :desc "Switch workspace"         :n "."   #'+workspace/switch-to
     :desc "Kill all buffers"         :n "x"   #'doom/kill-all-buffers
     :desc "Delete session"           :n "X"   #'+workspace/kill-session
     :desc "Delete this workspace"    :n "d"   #'+workspace/delete
     :desc "Load session"             :n "L"   #'+workspace/load-session
     :desc "Next workspace"           :n "]"   #'+workspace/switch-right
     :desc "Previous workspace"       :n "["   #'+workspace/switch-left
     :desc "Switch to 1st workspace"  :n "1"   (λ! (+workspace/switch-to 0))
     :desc "Switch to 2nd workspace"  :n "2"   (λ! (+workspace/switch-to 1))
     :desc "Switch to 3rd workspace"  :n "3"   (λ! (+workspace/switch-to 2))
     :desc "Switch to 4th workspace"  :n "4"   (λ! (+workspace/switch-to 3))
     :desc "Switch to 5th workspace"  :n "5"   (λ! (+workspace/switch-to 4))
     :desc "Switch to 6th workspace"  :n "6"   (λ! (+workspace/switch-to 5))
     :desc "Switch to 7th workspace"  :n "7"   (λ! (+workspace/switch-to 6))
     :desc "Switch to 8th workspace"  :n "8"   (λ! (+workspace/switch-to 7))
     :desc "Switch to 9th workspace"  :n "9"   (λ! (+workspace/switch-to 8))
     :desc "Switch to last workspace" :n "0"   #'+workspace/switch-to-last)

   (:desc "buffer" :prefix "b"
     :desc "New empty buffer"        :n "n" #'evil-buffer-new
     :desc "Switch workspace buffer" :n "b" #'persp-switch-to-buffer
     :desc "Switch buffer"           :n "B" #'switch-to-buffer
     :desc "Kill buffer"             :n "k" #'doom/kill-this-buffer
     :desc "Kill other buffers"      :n "o" #'doom/kill-other-buffers
     :desc "Save buffer"             :n "s" #'+amos/switch-to-scratch-buffer
     :desc "Pop scratch buffer"      :n "x" #'doom/open-scratch-buffer
     :desc "Bury buffer"             :n "z" #'bury-buffer
     :desc "Next buffer"             :n "]" #'doom/next-buffer
     :desc "Previous buffer"         :n "[" #'doom/previous-buffer
     :desc "Sudo edit this file"     :n "S" #'doom/sudo-this-file)

   (:desc "code" :prefix "c"
     :desc "List errors"               :n  "x" #'flycheck-list-errors
     :desc "Evaluate buffer/region"    :n  "e" #'+eval/buffer
                                       :v  "e" #'+eval/region
     :desc "Evaluate & replace region" :nv "E" #'+eval:replace-region
     :desc "Build tasks"               :nv "b" #'+eval/build
     :desc "Jump to definition"        :n  "d" #'+jump/definition
     :desc "Jump to references"        :n  "D" #'+jump/references
     :desc "Open REPL"                 :n  "r" #'+eval/open-repl
                                       :v  "r" #'+eval:repl)

   (:desc "file" :prefix "f"
     :desc "File file"                 :n "f" #'find-file
     :desc "Sudo find file"            :n "F" #'doom/sudo-find-file
     :desc "Find file in project"      :n "/" #'projectile-find-file
     :desc "Find file from here"       :n "?" #'counsel-file-jump
     :desc "Find other file"           :n "a" #'projectile-find-other-file
     :desc "Open project editorconfig" :n "c" #'editorconfig-find-current-editorconfig
     :desc "Find file in dotfiles"     :n "d" #'+amos/find-in-dotfiles
     :desc "Delete current file"       :n "D" #'+evil:delete-this-file
     :desc "Find file in emacs.d"      :n "e" #'+amos/find-in-emacsd
     :desc "Browse emacs.d"            :n "E" #'+amos/browse-emacsd
     :desc "Recent files"              :n "r" #'recentf-open-files
     :desc "Recent project files"      :n "R" #'+amos/rename-current-buffer-file
     :desc "Yank filename"             :n "y" #'+amos/yank-buffer-filename
     :desc "Yank filename"             :n "Y" #'+amos/yank-buffer-filename-nondir)

   (:desc "git" :prefix "g"
     :desc "Git status"        :n  "s" #'magit-status
     :desc "Git blame"         :n  "b" #'magit-blame
     :desc "Git time machine"  :n  "t" #'git-timemachine-toggle
     :desc "Git revert hunk"   :n  "r" #'git-gutter:revert-hunk
     :desc "Git revert buffer" :n  "R" #'vc-revert
     :desc "List gists"        :n  "g" #'+gist:list
     :desc "Next hunk"         :nv "]" #'git-gutter:next-hunk
     :desc "Previous hunk"     :nv "[" #'git-gutter:previous-hunk)

   (:desc "help" :prefix "h"
     :n "h" help-map
     :desc "Apropos"               :n "a" #'apropos
     :desc "Reload theme"          :n "R" #'doom/reload-theme
     :desc "Find library"          :n "l" #'find-library
     :desc "Toggle Emacs log"      :n "m" #'doom/popup-toggle-messages
     :desc "Command log"           :n "L" #'global-command-log-mode
     :desc "Describe function"     :n "f" #'describe-function
     :desc "Describe key"          :n "k" #'describe-key
     :desc "Describe char"         :n "c" #'describe-char
     :desc "Describe mode"         :n "M" #'describe-mode
     :desc "Describe variable"     :n "v" #'describe-variable
     :desc "Describe face"         :n "F" #'describe-face
     :desc "Describe DOOM setting" :n "s" #'doom/describe-setting
     :desc "Describe DOOM module"  :n "d" #'doom/describe-module
     :desc "Find definition"       :n "." #'+jump/definition
     :desc "Find references"       :n "/" #'+jump/references
     :desc "Find documentation"    :n "h" #'+jump/documentation
     :desc "What face"             :n "'" #'doom/what-face
     :desc "What minor modes"      :n ";" #'doom/what-minor-mode
     :desc "Info"                  :n "i" #'info
     :desc "Toggle profiler"       :n "p" #'doom/toggle-profiler)

   (:desc "insert" :prefix "i"
     :desc "From kill-ring" :nv "y" #'counsel-yank-pop
     :desc "From snippet"   :nv "s" #'yas-insert-snippet)

   (:desc "notes" :prefix "n"
     :desc "Open todo file"        :n "t" #'+amos/open-todo-file
     :desc "Find file in notes"    :n "n" #'+amos/find-in-notes
     :desc "Browse notes"          :n "N" #'+amos/browse-notes
     :desc "Org capture"           :n "x" #'+org-capture/open
     :desc "Browse mode notes"     :n "m" #'+org/browse-notes-for-major-mode
     :desc "Browse project notes"  :n "p" #'+org/browse-notes-for-project)

   (:desc "open" :prefix "o"
     :desc "Default browser"     :n  "b" #'browse-url-of-file
     :desc "Dired"               :n  "d" #'+amos/dired-jump
     :desc "REPL"                :n  "r" #'+eval/open-repl
                                 :v  "r" #'+eval:repl
     :desc "Neotree"             :n  "n" #'+neotree/toggle
     :desc "Terminal"            :n  "t" #'+term/open-popup
     :desc "Terminal in project" :n  "T" #'+term/open-popup-in-project

     ;; applications
     :desc "APP: elfeed"  :n "E" #'=rss
     :desc "APP: email"   :n "m" #'=email
     :desc "APP: twitter" :n "T" #'=twitter
     :desc "APP: regex"   :n "X" #'=regex

     ;; macos
     (:when IS-MAC
       :desc "Reveal in Finder"          :n "o" #'+macos/reveal-in-finder
       :desc "Reveal project in Finder"  :n "O" #'+macos/reveal-project-in-finder
       :desc "Send to Transmit"          :n "u" #'+macos/send-to-transmit
       :desc "Send project to Transmit"  :n "U" #'+macos/send-project-to-transmit
       :desc "Send to Launchbar"         :n "l" #'+macos/send-to-launchbar
       :desc "Send project to Launchbar" :n "L" #'+macos/send-project-to-launchbar))

   :desc "Browse project"                :n "p" #'+amos/counsel-projectile-switch-project

   (:desc "project" :prefix "P"
     :desc "Browse project"          :n  "." (find-file-in! (doom-project-root))
     :desc "Find file in project"    :n  "/" #'projectile-find-file
     :desc "Run cmd in project root" :nv "!" #'projectile-run-shell-command-in-root
     :desc "Switch project"          :n  "p" #'projectile-switch-project
     :desc "Recent project files"    :n  "r" #'projectile-recentf
     :desc "List project tasks"      :n  "t" #'+ivy/tasks
     :desc "Pop scratch in project"  :n  "o" #'doom/open-project-scratch-buffer
     :desc "Invalidate cache"        :n  "x" #'projectile-invalidate-cache)

   (:desc "quit" :prefix "q"
     :desc "Quit"                    :n "q" #'+amos/prompt-kill-emacs
     :desc "Quit (forget session)"   :n "Q" #'+workspace/kill-session-and-quit)

   (:desc "remote" :prefix "r"
     :desc "Ivy resume"             :n "l" #'ivy-resume
     :desc "Upload local"           :n "u" #'+upload/local
     :desc "Upload local (force)"   :n "U" (λ! (+upload/local t))
     :desc "Download remote"        :n "d" #'+upload/remote-download
     :desc "Diff local & remote"    :n "D" #'+upload/diff
     :desc "Browse remote files"    :n "." #'+upload/browse
     :desc "Detect remote changes"  :n ">" #'+upload/check-remote)

   (:desc "snippets" :prefix "s"
     :desc "New snippet"           :n  "n" #'yas-new-snippet
     :desc "Insert snippet"        :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode" :n  "s" #'yas-visit-snippet-file
     :desc "Find snippet"          :n  "S" #'+amos/find-in-snippets)

   (:desc "toggle" :prefix "t"
     :desc "Flyspell"               :n "s" #'flyspell-mode
     :desc "Flycheck"               :n "f" #'flycheck-mode
     :desc "Rainbow"                :n "r" #'rainbow-mode
     :desc "Truncate lines"         :n "l" #'toggle-truncate-lines
     :desc "Fullscreen"             :n "w" #'whitespace-mode
     :desc "Fullscreen"             :n "f" #'doom/toggle-fullscreen
     :desc "Indent guides"          :n "i" #'highlight-indentation-mode
     :desc "Indent guides (column)" :n "I" #'highlight-indentation-current-column-mode
     :desc "Impatient mode"         :n "h" #'+impatient-mode/toggle
     :desc "Big mode"               :n "b" #'doom-big-font-mode
     :desc "Evil goggles"           :n "g" #'+evil-goggles/toggle))


 ;; --- Personal vim-esque bindings ------------------
 :n  "zx" #'doom/kill-this-buffer
 :n  "ZX" #'bury-buffer
 :n  "]b" #'doom/next-buffer
 :n  "[b" #'doom/previous-buffer
 :n  "]w" #'+workspace/switch-right
 :n  "[w" #'+workspace/switch-left
 :n  "gf" #'+amos:evil-find-file-at-point-with-line
 :m  "gt" #'+workspace/switch-right
 :m  "gT" #'+workspace/switch-left
 :m  "gd" #'+jump/definition
 :m  "gy" #'+amos/copy-and-comment-lines-inverse
 :m  "gY" #'+amos/copy-and-comment-lines
 :m  "gD" #'+jump/references
 :m  "gh" #'+jump/documentation
 :n  "go" #'+amos/evil-insert-line-below
 :n  "gO" #'+amos/evil-insert-line-above
 :n  "gp" #'+evil/reselect-paste
 :n  "gr" #'+jump/references
 :n  "gR" #'+eval/buffer
 :v  "gR" #'+eval:replace-region
 :v  "@"  #'+evil:macro-on-all-lines
 :n  "g@" #'+evil:macro-on-all-lines
 ;; repeat in visual mode (FIXME buggy)
 :v  "."  #'evil-repeat
 ;; don't leave visual mode after shifting
 :v  "<"  #'+evil/visual-dedent  ; vnoremap < <gv
 :v  ">"  #'+evil/visual-indent  ; vnoremap > >gv
 ;; paste from recent yank register (which isn't overwritten)
 :v  "C-p" "\"0p"

 (:after image-mode
   (:map image-mode-map
     :n "q" #'quit-window))

 (:map evil-window-map ; prefix "C-w"
   ;; Navigation
   "C-h"     #'evil-window-left
   "C-j"     #'evil-window-down
   "C-k"     #'evil-window-up
   "C-l"     #'evil-window-right
   "C-w"     #'ace-window
   ;; Swapping windows
   "H"       #'+evil/window-move-left
   "J"       #'+evil/window-move-down
   "K"       #'+evil/window-move-up
   "L"       #'+evil/window-move-right
   "C-S-w"   #'ace-swap-window
   ;; Window undo/redo
   "u"       #'winner-undo
   "C-u"     #'winner-undo
   "C-r"     #'winner-redo
   "o"       #'doom/window-enlargen
   ;; Delete window
   "c"       #'+workspace/close-window-or-workspace
   "C-C"     #'ace-delete-window)


 ;; --- Plugin bindings ------------------------------
 ;; auto-yasnippet
 :i  [C-tab] #'aya-expand
 :nv [C-tab] #'aya-create

 ;; company-mode (vim-like omnicompletion)
 :i "C-SPC"  #'+company/complete
 (:prefix "C-x"
   :i "C-l"   #'+company/whole-lines
   :i "C-k"   #'+company/dict-or-keywords
   :i "C-f"   #'company-files
   :i "C-]"   #'company-etags
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
     [escape]     (λ! (company-abort) (evil-normal-state 1)))
   ;; Automatically applies to `company-filter-map'
   (:map company-search-map
     "C-j"        #'company-search-repeat-forward
     "C-k"        #'company-search-repeat-backward
     "C-s"        (λ! (company-search-abort) (company-filter-candidates))
     [escape]     #'company-search-abort))

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
     "C-o"     nil
     "C-i"     nil
     "f"       #'counsel-find-file
     "S"       #'hydra-dired-quick-sort/body
     "j"       #'dired-next-line
     "k"       #'dired-previous-line
     "W"       (lambda () (interactive) (dired-copy-filename-as-kill 0))
     :nm "C-p" #'peep-dired
     :nm "Y"   #'+amos/dired-rsync
     :nm "S"   #'hydra-dired-quick-sort/body
     :n  "j"   #'+amos/evil-undefine
     :n  "k"   #'+amos/evil-undefine
     :n  "M-n" #'+amos/counsel-jumpfile-function
     :n  "M-o" #'+amos/prev-history
     :n  "M-i" #'+amos/next-history
     :n  "h"   #'+amos/up-directory
     :n  "l"   #'dired-open-file))

 (:after peep-dired
   (:map peep-dired-mode-map
     "j"           #'peep-dired-next-file
     "k"           #'peep-dired-prev-file
     "<SPC>"       #'peep-dired-scroll-page-down
     "C-<SPC>"     #'peep-dired-scroll-page-up
     "<backspace>" #'peep-dired-scroll-page-up
     "C-p"         #'peep-dired))

 ;; evil-nerd-commenter
 :n  "gc"  #'evilnc-comment-or-uncomment-lines

 ;; evil-exchange
 :n  "gx"  #'evil-exchange

 ;; evil-matchit
 :nv [tab] #'evil-jump-forward

 ;; evil-magit
 (:after evil-magit
   :map (magit-status-mode-map magit-revision-mode-map)
   "SPC" nil
   :n "C-j" nil
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

 ;; evil-multiedit
 :v  "R"     #'evil-multiedit-match-all
 :n  "M-d"   #'evil-multiedit-match-symbol-and-next
 :n  "M-D"   #'evil-multiedit-match-symbol-and-prev
 :v  "M-d"   #'evil-multiedit-match-and-next
 :v  "M-D"   #'evil-multiedit-match-and-prev
 :nv "C-M-d" #'evil-multiedit-restore
 (:after evil-multiedit
   (:map evil-multiedit-state-map
     "M-d" #'evil-multiedit-match-and-next
     "M-D" #'evil-multiedit-match-and-prev
     "RET" #'evil-multiedit-toggle-or-restrict-region)
   (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
     "C-n" #'evil-multiedit-next
     "C-p" #'evil-multiedit-prev))

 ;; evil-snipe
 (:after evil-snipe
   :map evil-snipe-local-mode-map
   :nm "S" nil
   :nm "s" nil
   ;; Binding to switch to evil-easymotion/avy after a snipe
   :map evil-snipe-parent-transient-map
   "C-f" (λ! (require 'evil-easymotion)
             (call-interactively
              (evilem-create #'evil-snipe-repeat
                             :bind ((evil-snipe-scope 'whole-visible-buffer)
                                    (evil-snipe-enable-highlight)
                                    (evil-snipe-enable-incremental-highlight))))))

 ;; evil-surround
 :v  "s"  #'evil-surround-region
 :v  "S"  #'evil-substitute
 :o  "s"  #'evil-surround-edit
 :o  "S"  #'evil-Surround-edit

 ;; expand-region
 :v  "v"  #'er/expand-region
 :v  "V"  #'er/contract-region

 ;; flycheck
 :nm  "C-." #'next-error
 :nm  "C-," #'previous-error
 (:after flycheck
   :map flycheck-error-list-mode-map
   :n "C-j" #'flycheck-error-list-next-error
   :n "C-k" #'flycheck-error-list-previous-error
   :n "j"   #'flycheck-error-list-next-error
   :n "k"   #'flycheck-error-list-previous-error
   :n "RET" #'flycheck-error-list-goto-error
   :n "C-l" #'flycheck-error-list-goto-error)

 ;; flyspell
 :m  "]S" #'flyspell-correct-word-generic
 :m  "[S" #'flyspell-correct-previous-word-generic

 ;; git-gutter
 :m  "]d" #'git-gutter:next-hunk
 :m  "[d" #'git-gutter:previous-hunk

 ;; git-timemachine
 (:after git-timemachine
   (:map git-timemachine-mode-map
     :nv "p" #'git-timemachine-show-previous-revision
     :nv "n" #'git-timemachine-show-next-revision
     :nv "g" #'git-timemachine-show-nth-revision
     :nv "q" #'git-timemachine-quit
     :nv "w" #'git-timemachine-kill-abbreviated-revision
     :nv "W" #'git-timemachine-kill-revision
     :nv "b" #'git-timemachine-blame))

 ;; gist
 (:after gist
   :map gist-list-menu-mode-map
   :n "RET" #'+gist/open-current
   :n "b"   #'gist-browse-current-url
   :n "c"   #'gist-add-buffer
   :n "d"   #'gist-kill-current
   :n "f"   #'gist-fork
   :n "q"   #'quit-window
   :n "r"   #'gist-list-reload
   :n "s"   #'gist-star
   :n "S"   #'gist-unstar
   :n "y"   #'gist-print-current-url)

 ;; helm
 (:after helm
   (:map helm-map
     "ESC"        nil
     "C-S-n"      #'helm-next-source
     "C-S-p"      #'helm-previous-source
     "C-u"        #'helm-delete-minibuffer-contents
     "C-w"        #'backward-kill-word
     "C-r"        #'evil-paste-from-register ; Evil registers in helm! Glorious!
     "M-b"        #'backward-word
     [left]       #'backward-char
     [right]      #'forward-char
     [escape]     #'helm-keyboard-quit
     [tab]        #'helm-execute-persistent-action)

   (:after helm-files
     (:map helm-generic-files-map
       :e "ESC"     #'helm-keyboard-quit)
     (:map helm-find-files-map
       "C-w" #'helm-find-files-up-one-level
       "TAB" #'helm-execute-persistent-action))

   (:after helm-ag
     (:map helm-ag-map
       "<backtab>"  #'helm-ag-edit)))

 ;; hl-todo
 :m  "]t" #'hl-todo-next
 :m  "[t" #'hl-todo-previous

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   [escape]        #'keyboard-escape-quit
   ;; "C-c C-o"       #'+amos/swiper-replace
   "TAB"           #'ivy-call-and-recenter
   "M-z"           #'undo
   "M-j"           #'ivy-immediate-done
   "C-r"           #'evil-paste-from-register
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

 ;; rotate-text
 :n  "!"  #'rotate-text

 ;; smart-forward
 :nv "K"  #'smart-up
 :m  "g]" #'smart-forward
 :m  "g[" #'smart-backward

 ;; undo-tree -- undo/redo for visual regions
 :v "u"   #'undo-tree-undo
 :v "C-r" #'undo-tree-redo

 ;; yasnippet
 (:after yasnippet
   (:map yas-keymap
     "C-e"           #'+snippets/goto-end-of-field
     "C-a"           #'+snippets/goto-start-of-field
     "<M-right>"     #'+snippets/goto-end-of-field
     "<M-left>"      #'+snippets/goto-start-of-field
     "<M-backspace>" #'+snippets/delete-to-start-of-field
     [escape]        #'evil-normal-state
     [backspace]     #'+snippets/delete-backward-char
     [delete]        #'+snippets/delete-forward-char-or-field)
   (:map yas-minor-mode-map
     :i "<tab>" yas-maybe-expand
     :v "<tab>" #'+snippets/expand-on-region))


 ;; --- Major mode bindings --------------------------
 (:after markdown-mode
   (:map markdown-mode-map
     ;; fix conflicts with private bindings
     "<backspace>" nil
     "<M-left>"    nil
     "<M-right>"   nil))


 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "j" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down


 ;; --- Built-in plugins -----------------------------
 (:after comint
   ;; TAB auto-completion in term buffers
   :map comint-mode-map [tab] #'company-complete)

 (:after debug
   ;; For elisp debugging
   :map debugger-mode-map
   :n "q"   (lambda () (interactive) (top-level) (doom/kill-this-buffer))
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
   :n "b" 'edebug-set-breakpoint
   :n "u" 'edebug-unset-breakpoint
   :n "B" 'edebug-next-breakpoint
   :n "x" 'edebug-set-conditional-breakpoint
   :n "X" 'edebug-set-global-break-condition

   ;; evaluation
   :n "r" 'edebug-previous-result
   :n "e" 'edebug-eval-expression
   :n "\C-x\C-e" 'edebug-eval-last-sexp
   :n "E" 'edebug-visit-eval-list

   ;; views
   :n "w" 'edebug-where
   :n "v" 'edebug-view-outside ;; maybe obsolete??
   :n "p" 'edebug-bounce-point
   :n "P" 'edebug-view-outside ;; same as v
   :n "W" 'edebug-toggle-save-windows

   ;; misc
   :n "?" 'edebug-help
   :n "d" 'edebug-backtrace
   :n "-" 'negative-argument

   ;; statistics
   :n "=" 'edebug-temp-display-freq-count)

 (:map help-mode-map
   :n "[["  #'help-go-back
   :n "]]"  #'help-go-forward
   :n "o"   #'ace-link-help
   :n "q"   #'quit-window
   :n "Q"   #'+ivy-quit-and-resume)

 (:after vc-annotate
   :map vc-annotate-mode-map
   :n "q"   #'kill-this-buffer
   :n "d"   #'vc-annotate-show-diff-revision-at-line
   :n "D"   #'vc-annotate-show-changeset-diff-revision-at-line
   :n "SPC" #'vc-annotate-show-log-revision-at-line
   :n "]]"  #'vc-annotate-next-revision
   :n "[["  #'vc-annotate-prev-revision
   :n "TAB" #'vc-annotate-toggle-annotation-visibility
   :n "RET" #'vc-annotate-find-revision-at-line))


;; --- Custom key functionality ---------------------
(defmacro do-repeat! (command next-func prev-func)
  "Repeat motions with ;/,"
  (let ((fn-sym (intern (format "+evil*repeat-%s" command))))
    `(progn
       (defun ,fn-sym (&rest _)
         (define-key evil-motion-state-map (kbd ";") ',next-func)
         (define-key evil-motion-state-map (kbd ",") ',prev-func))
       (advice-add #',command :before #',fn-sym))))

;; n/N
(do-repeat! evil-ex-search-next evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-previous evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-forward evil-ex-search-next evil-ex-search-previous)
(do-repeat! evil-ex-search-backward evil-ex-search-next evil-ex-search-previous)

;; f/F/t/T/s/S
(after! evil-snipe
  ;; (setq evil-snipe-repeat-keys nil
  ;;       evil-snipe-override-evil-repeat-keys nil) ; causes problems with remapped ;

  (do-repeat! evil-snipe-f evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-F evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-t evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-T evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-s evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-S evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-x evil-snipe-repeat evil-snipe-repeat-reverse)
  (do-repeat! evil-snipe-X evil-snipe-repeat evil-snipe-repeat-reverse))

;; */#
(after! evil-visualstar
  (do-repeat! evil-visualstar/begin-search-forward
    evil-ex-search-next evil-ex-search-previous)
  (do-repeat! evil-visualstar/begin-search-backward
    evil-ex-search-previous evil-ex-search-next))

(after! profiler
  (map!
   (:map profiler-report-mode-map
     :nm "RET" #'profiler-report-expand-entry)))

;; evil-easymotion
(after! evil-easymotion
  (let ((prefix "C-t"))
    ;; NOTE `evilem-default-keybinds' unsets all other keys on the prefix (in
    ;; motion state)
    (evilem-default-keybindings prefix)
    (evilem-define (kbd (concat prefix " n")) #'evil-ex-search-next)
    (evilem-define (kbd (concat prefix " N")) #'evil-ex-search-previous)
    (evilem-define (kbd (concat prefix " f")) #'evil-snipe-repeat
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))
    (evilem-define (kbd (concat prefix " F")) #'evil-snipe-repeat-reverse
                   :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))


;;
;; Keybinding fixes
;;

;; This section is dedicated to "fixing" certain keys so that they behave
;; properly, more like vim, or how I like it.

(map! (:map input-decode-map
        [S-iso-lefttab] [backtab]
        (:unless window-system "TAB" [tab])) ; Fix TAB in terminal

      (:after cus-edit
        (:map custom-mode-map
          :n "q"    #'Custom-buffer-done))

      (:after view
        (:map view-mode-map
          :n "q"    #'View-quit))

      (:map key-translation-map
        "M-`" (kbd "C-S-s")
        "C-@" (kbd "C-SPC")
        "C-^" (kbd "C-,")
        "C-_" (kbd "C-."))

      ;; I want C-a and C-e to be a little smarter. C-a will jump to
      ;; indentation. Pressing it again will send you to the true bol. Same goes
      ;; for C-e, except it will ignore comments and trailing whitespace before
      ;; jumping to eol.
      :i "C-a" #'doom/backward-to-bol-or-indent
      :i "C-e" #'doom/forward-to-last-non-comment-or-eol
      :i "C-u" #'doom/backward-kill-to-bol-and-indent

      ;; textmate-esque newline insertion
      :i [M-return]     #'evil-open-below
      :i [S-M-return]   #'evil-open-above
      ;; textmate-esque deletion
      [M-backspace]     #'+amos:evil-delete-backward-symbol
      :i [backspace]    #'delete-backward-char
      :i [M-backspace]  #'+amos:evil-delete-backward-symbol
      ;; Emacsien motions for insert mode
      :i "M-b" #'backward-word
      :i "M-f" #'forward-word

      ;; Highjacks space/backspace to:
      ;;   a) balance spaces inside brackets/parentheses ( | ) -> (|)
      ;;   b) delete space-indented blocks intelligently
      ;;   c) do none of this when inside a string
      :i "SPC"                          #'doom/inflate-space-maybe
      :i [remap delete-backward-char]   #'doom/deflate-space-maybe
      :i [remap newline]                #'doom/newline-and-indent

      ;; Restore common editing keys (and ESC) in minibuffer
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
