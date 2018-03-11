;;; init.el -*- lexical-binding: t; -*-
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/.emacs.d
;;
;;   =================     ===============     ===============   ========  ========
;;   \\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
;;   ||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
;;   || . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
;;   ||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
;;   || . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
;;   ||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
;;   || . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
;;   ||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
;;   ||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
;;   ||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
;;   ||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
;;   ||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
;;   ||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
;;   ||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
;;   ||.=='    _-'                                                     `' |  /==.||
;;   =='    _-'                                                            \/   `==
;;   \   _-'                                                                `-_   /
;;    `''                                                                      ``'
;;
;; These demons are not part of GNU Emacs.
;;
;;; License: MIT

(require 'core (concat user-emacs-directory "core/core"))

(doom! :feature
       eval              ; run code, run (also, repls)
       evil              ; come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       jump              ; helping you get around
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       syntax-checker    ; tasing you for every semicolon you forget
       version-control   ; remember, remember that commit in November
       ;; workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life
      ;helm              ; the *other* search engine for love and life
      ;ido               ; the other *other* search engine...

       :ui
       doom              ; what makes DOOM look the way it does
       ;; doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
      ;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
      ;evil-goggles      ; display visual hints when editing in evil
       unicode           ; extended unicode support for various languages
      ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
      ;vi-tilde-fringe   ; fringe tildes to mark beyond EOB

       :tools
       electric-indent   ; smarter, keyword-based electric-indent
       make              ; run make tasks from Emacs
       rotate-text       ; cycle region at point between text candidates
       tmux              ; an API for interacting with tmux

       :lang
       assembly          ; assembly for fun or debugging
      ;cc                ; C/C++/Obj-C madness
       crystal           ; ruby at the speed of c
      ;clojure
      ;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
      ;elixir            ; erlang done right
       elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       go                ; the hipster dialect
      ;(haskell +intero) ; a language that's lazier than I am
       hy                ; readability of scheme w/ speed of python
      ;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       julia             ; a better, faster MATLAB
       latex             ; writing papers in Emacs has never been so fun
       ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
      ;ocaml             ; an objective camel
       perl              ; write code no one else can comprehend
       php               ; make php less awful to work with
       plantuml          ; diagrams for confusing people more
       purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
      ;swift             ; who asked for emoji variables?
       typescript        ; javascript, but better
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded last.
       :app
      ;email             ; emacs as an email client
      ;irc               ; how neckbeards socialize
      rss               ; emacs as an RSS reader
      ;twitter           ; twitter client https://twitter.com/vnought
      ;write             ; emacs as a word processor (latex + org + markdown)

       ;; Private modules named after your username are loaded automatically.
       ;; Leaving this here is harmless though.
       :private
       amos
       cc
       org
       org-babel
       org-capture
       org-export
       email)
(put 'narrow-to-region 'disabled nil)
