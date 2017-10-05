;;; private/amos/init.el -*- lexical-binding: t; -*-

;; I've swapped these keys on my keyboard
(setq user-mail-address "amosbird@gmail.com"
      user-full-name    "Amos Bird")

;; An extra measure to prevent the flash of unstyled mode-line while Emacs is
;; booting up (when Doom is byte-compiled).
(setq-default mode-line-format nil)
(setq +org-dir (expand-file-name "~/org/"))
(advice-add #'nlinum-mode :override #'ignore)


(def-package-hook! centered-window-mode
  :pre-init
  (defun amos-special-window-p (window)
    (let* ((buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (equal buffname "*doom*")
          (equal (with-current-buffer buffer major-mode) 'pdf-view-mode)
          (and  (not (window-full-width-p window))
                (not (window-at-side-p window 'left))))))
  (push #'amos-special-window-p cwm-ignore-window-predicates)
  t)

;; host-specific settings
(pcase (system-name)
  ("t450s"
   ;; smaller screen, smaller fonts
   (set! :font "Ubuntu Mono" :size 14)
   (set! :variable-font "Fira Sans" :size 14)
   (set! :unicode-font "DejaVu Sans Mono" :size 14)
   (setq +doom-modeline-height 25))
  (_
   ;; smaller screen, smaller fonts
   (set! :font "Fira Mono" :size 10)
   (set! :variable-font "Fira Sans" :size 10)
   (set! :unicode-font "DejaVu Sans Mono" :size 10)
   (setq +doom-modeline-height 25))
  )
