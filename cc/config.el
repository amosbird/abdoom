;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(defvar +cc-include-paths (list "include/")
  "A list of paths, relative to a project root, to search for headers in C/C++.
Paths can be absolute.

The purpose of this variable is to ensure syntax checkers and code-completion
knows where to look for headers.")

(defvar +cc-compiler-options
  `((c-mode . nil)
    (c++-mode
     . ,(list "-std=c++17" ; use C++11 by default
              (when IS-MAC
                ;; NOTE beware: you'll get abi-inconsistencies when passing
                ;; std-objects to libraries linked with libstdc++ (e.g. if you
                ;; use boost which wasn't compiled with libc++)
                (list "-stdlib=libc++"))))
    (objc-mode . nil))
  "A list of default compiler options for the C family. These are ignored if a
compilation database is present in the project.")

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm" . objc-mode) ("\\.h\\'" . c++-mode)
  :preface
  (defun +cc-c++-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (or (file-exists-p (expand-file-name
                             (concat (file-name-sans-extension buffer-file-name)
                                     ".cpp")))
             (when-let (file (car-safe (projectile-get-other-files
                                        buffer-file-name
                                        (projectile-current-project-files))))
               (equal (file-name-extension file) "cpp")))))

  (defun +cc-objc-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))

  ;; (push (cons #'+cc-c++-header-file-p  'c++-mode)  magic-mode-alist)
  ;; (push (cons #'+cc-objc-header-file-p 'objc-mode) magic-mode-alist)

  :init
  (setq-default c-basic-offset tab-width)

  :config
  (set! :electric '(c-mode c++-mode objc-mode java-mode)
    :chars '(?\n ?\}))
  ;; (set! :company-backend
  ;;       '(c-mode c++-mode objc-mode)
  ;;       '(company-irony-c-headers company-irony))

  ;;; Style/formatting
  ;; C/C++ style settings
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)
  (c-set-offset 'substatement-open '0) ; don't indent brackets
  (c-set-offset 'inline-open       '0)
  (c-set-offset 'block-open        '+)
  (c-set-offset 'brace-list-open   '+)
  (c-set-offset 'case-label        '+)
  (c-set-offset 'access-label      '-)
  (c-set-offset 'arglist-intro     '+)
  (c-set-offset 'arglist-close     '0)
  ;; Indent privacy keywords at same level as class properties
  ;; (c-set-offset 'inclass #'+cc-c-lineup-inclass)

  ;;; Better fontification (also see `modern-cpp-font-lock')
  (add-hook 'c-mode-common-hook #'rainbow-delimiters-mode)
  (add-hook! (c-mode c++-mode) #'highlight-numbers-mode)
  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)

  ;; Improve indentation of inline lambdas in C++11
  (advice-add #'c-lineup-arglist :around #'+cc*align-lambda-arglist)

  ;;; Keybindings
  ;; Completely disable electric keys because it interferes with smartparens and
  ;; custom bindings. We'll do this ourselves.
  (setq c-tab-always-indent t
        c-electric-flag nil)
  (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")"))
    (define-key c-mode-base-map key nil))
  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (map! :map c++-mode-map
        "<" nil
        "C-c i" #'+amos/add-include
        :i ">" #'+cc/autoclose->-maybe)

  ;; ...and leave it to smartparens
  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "<" ">" :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p))
    (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
    ;; Doxygen blocks
    (sp-local-pair "/**" "*/" :post-handlers '(("||\n[i]" "RET") ("||\n[i]" "SPC")))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))


(def-package! modern-cpp-font-lock
  :commands modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

(def-package! cmake-mode
  :mode
  (("/CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

(def-package! rtags
  :after cc-mode
  :config
  (require 'rtags)
  (require 'counsel-dash)
  (require 'ivy-rtags)
  (require 'flycheck-rtags)

  (defconst +amos-rdm-buffer-name "*rdm*" "The rdm buffer name.")

  (defun +amos--system-process-running-p (name)
    "If a process called NAME is running on the system."
    (let* ((all-args (mapcar (lambda (x) (cdr (assq 'args (process-attributes x)))) (list-system-processes)))
           (match-args (+amos--filter (lambda (x) (+amos--string-match (concat "\\b" name "\\b") x)) all-args)))
      (not (null match-args))))

  (defun +amos--string-match (regexp name)
    "Wrap 'string-match' of REGEXP and NAME to make sure we don't pass it a nil string."
    (when name
      (string-match regexp name)))

  (defun +amos--filter (pred seq)
    "Apply PRED to filter SEQ."
    (delq nil (mapcar (lambda (x) (and (funcall pred x) x)) seq)))

  (defun +amos--process-running-p (name)
    "If a process called NAME is running or not."
    (or (get-process name) (+amos--system-process-running-p name)))

  (defun +amos--message (str &rest vars)
    "Output a message with STR and formatted by VARS."
    (message (apply #'format (concat "+amos [%s]: " str) (cons (current-time-string) vars))))

  (defun +amos-maybe-start-rdm ()
    "Start the rdm (rtags) server."
    (unless (+amos--process-running-p "rdm")
      (let ((buf (get-buffer-create +amos-rdm-buffer-name)))
        (+amos--message "Starting rdm server")
        (with-current-buffer buf
          (let ((rdm-process (start-process "rdm" (current-buffer)
                                            "rdm")))
            (set-process-query-on-exit-flag rdm-process nil))))))

  (add-hook! (c-mode c++-mode) #'+amos-maybe-start-rdm)

  (set!
    :jump 'c-mode
    :definition #'rtags-find-symbol-at-point
    :references #'rtags-find-references-at-point
    :documentation #'counsel-dash-at-point)
  (set!
    :jump 'c++-mode
    :definition #'rtags-find-symbol-at-point
    :references #'rtags-find-references-at-point
    :documentation #'counsel-dash-at-point)

  (add-hook! 'rtags-jump-hook #'evil-set-jump)
  (add-hook! 'rtags-after-find-file-hook #'recenter)
  (setq rtags-autostart-diagnostics t
        rtags-use-bookmarks nil
        rtags-display-result-backend 'ivy)
  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil)
    (flycheck-mode +1))
  (add-hook! (c-mode c++-mode) #'my-flycheck-rtags-setup)
  )

(def-package! disaster :commands disaster)

(def-package! cuda-mode :mode "\\.cuh?$")

(def-package! opencl-mode :mode "\\.cl$")

(def-package! demangle-mode
  :commands demangle-mode
  :init (add-hook 'llvm-mode-hook #'demangle-mode))

(def-package! glsl-mode
  :mode "\\.glsl$"
  :mode "\\.vert$"
  :mode "\\.frag$"
  :mode "\\.geom$")


(when (featurep! :completion company)
  (def-package! company-glsl
    :after glsl-mode
    :config
    (if (executable-find "glslangValidator")
        (warn "glsl-mode: couldn't find glslangValidator, disabling company-glsl")
      (set! :company-backend 'glsl-mode '(company-glsl)))))

(def-package! clang-format
  :commands clang-format-buffer clang-format)

(defun +amos/add-include (header)
  "Add an #include line for `header' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (let ((incl (format "#include <%s>" header)))
    (save-excursion
      (if (search-backward incl nil t)
          (message "You already have %s." incl)
        (when (search-backward "#include" nil 'stop-at-top)
          (forward-line)
          (beginning-of-line))
        (insert incl)
        (newline)))))
