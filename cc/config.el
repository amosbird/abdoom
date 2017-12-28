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
    :chars '(?{ ?\n ?}))
  ;; (set! :company-backend
  ;;       '(c-mode c++-mode objc-mode)
  ;;       '(company-irony-c-headers company-irony))


  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (map! (:map c++-mode-map
         "<" nil
         :i ">"        #'+cc/autoclose->-maybe
         "C-c C-r"     #'+amos/rc-index-current-file
         "C-c l"       #'+amos/ivy-add-library-link
         "C-c i"       #'+amos/ivy-add-include))

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
  ;; (setq c-tab-always-indent t
  ;;       c-electric-flag nil)
  ;; (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")"))
  ;;   (define-key c-mode-base-map key nil))

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

  (defun +amos/rc-index-current-file ()
    (interactive)
    (let ((path (file-name-directory buffer-file-name)))
      (shell-command (format "rc --project-root=%s -c clang++ -std=c++17 -x c++ %s" path buffer-file-name))))

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
  (add-hook! (c-mode c++-mode) #'my-flycheck-rtags-setup))

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

(defvar +amos/default-include-headers
  '("algorithm" "any" "array" "atomic" "bitset" "cassert" "ccomplex" "cctype" "cerrno" "cfenv" "cfloat" "chrono" "cinttypes" "ciso646" "climits" "clocale" "cmath" "codecvt" "complex" "complex.h" "condition_variable" "csetjmp" "csignal" "cstdalign" "cstdarg" "cstdbool" "cstddef" "cstdint" "cstdio" "cstdlib" "cstring" "ctgmath" "ctime" "cuchar" "cwchar" "cwctype" "cxxabi.h" "deque" "exception" "fenv.h" "forward_list" "fstream" "functional" "future" "initializer_list" "iomanip" "ios" "iosfwd" "iostream" "istream" "iterator" "limits" "list" "locale" "map" "math.h" "memory" "mutex" "new" "numeric" "optional" "ostream" "queue" "random" "ratio" "regex" "scoped_allocator" "set" "shared_mutex" "sstream" "stack" "stdexcept" "stdlib.h" "streambuf" "string" "string_view" "system_error" "tgmath.h" "thread" "tuple" "type_traits" "typeindex" "typeinfo" "unordered_map" "unordered_set" "utility" "valarray" "variant" "vector" "auto_ptr.h" "backward_warning.h" "binders.h" "hash_fun.h" "hash_map" "hash_set" "hashtable.h" "strstream" "adxintrin.h" "altivec.h" "ammintrin.h" "arm_acle.h" "arm_neon.h" "armintr.h" "avx2intrin.h" "avx512bwintrin.h" "avx512cdintrin.h" "avx512dqintrin.h" "avx512erintrin.h" "avx512fintrin.h" "avx512ifmaintrin.h" "avx512ifmavlintrin.h" "avx512pfintrin.h" "avx512vbmiintrin.h" "avx512vbmivlintrin.h" "avx512vlbwintrin.h" "avx512vlcdintrin.h" "avx512vldqintrin.h" "avx512vlintrin.h" "avx512vpopcntdqintrin.h" "avxintrin.h" "bmi2intrin.h" "bmiintrin.h" "clflushoptintrin.h" "clzerointrin.h" "cpuid.h" "cuda_wrappers" "emmintrin.h" "f16cintrin.h" "float.h" "fma4intrin.h" "fmaintrin.h" "fxsrintrin.h" "htmintrin.h" "htmxlintrin.h" "ia32intrin.h" "immintrin.h" "intrin.h" "inttypes.h" "iso646.h" "limits.h" "lwpintrin.h" "lzcntintrin.h" "mm3dnow.h" "mm_malloc.h" "mmintrin.h" "module.modulemap" "msa.h" "mwaitxintrin.h" "nmmintrin.h" "opencl-c.h" "pkuintrin.h" "pmmintrin.h" "popcntintrin.h" "prfchwintrin.h" "rdseedintrin.h" "rtmintrin.h" "s390intrin.h" "sanitizer" "shaintrin.h" "smmintrin.h" "stdalign.h" "stdarg.h" "stdatomic.h" "stdbool.h" "stddef.h" "stdint.h" "stdnoreturn.h" "tbmintrin.h" "tgmath.h" "tmmintrin.h" "unwind.h" "vadefs.h" "varargs.h" "vecintrin.h" "wmmintrin.h" "x86intrin.h" "xmmintrin.h" "xopintrin.h" "xray" "xsavecintrin.h" "xsaveintrin.h" "xsaveoptintrin.h" "xsavesintrin.h" "xtestintrin.h" "unistd.h" "libaio.h"))

(defun +amos/add-include (header)
  "Add an #include line for `header' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (let ((incl (format "#include <%s>" header)))
    (save-excursion
      (if (search-backward incl nil t)
          nil
        (when (search-backward "#include" nil 'stop-at-top)
          (forward-line)
          (beginning-of-line))
        (insert incl)
        (newline)))))

(defun +amos/ivy-add-include ()
  (interactive)
  (ivy-read "Include: " (append +amos/default-include-headers
                                (split-string
                                 (shell-command-to-string "cd /usr/local/include ; find . -type f | sed 's=^./=='")))
            :require-match t
            :action #'+amos/add-include))

(defun +amos/add-library-link (library)
  "Add an -llibrary line for `library' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (let ((lib (format "%s \\" library)))
    (save-excursion
      (if (search-forward lib nil t)
          nil
        (when (re-search-forward "^-l.*\\\\$" nil 'stop-at-the-end 1)
          (forward-line)
          (beginning-of-line))
        (insert lib)
        (newline)))))

(defun +amos/ivy-add-library-link ()
  (interactive)
  (ivy-read "Library: " (split-string (shell-command-to-string "ldconfig -p | awk ' $1 ~ /^lib.*so$/ { print gensub(/^lib(.*).so$/, \"-l\\\\1\", 1, $1)}'"))
            :require-match t
            :action #'+amos/add-library-link))
