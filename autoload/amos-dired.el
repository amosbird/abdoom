;;; private/amos/autoload/amos-dired.el -*- lexical-binding: t; -*-

(require 'ring)
(require 'cl-seq)
(require 'files)
(require 'tramp)
(require 'dired)
(require 'cl-macs)

(defun +dired|sort-directories-first ()
  "List directories first in dired buffers."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook #'+dired|sort-directories-first)

;; Automatically create missing directories when creating new files
(defun +dired|create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(push #'+dired|create-non-existent-directory find-file-not-found-functions)

(defvar +amos-dired-history-ring (make-ring 200))
(defvar +amos-dired-history-index 0)

(defvar +amos-eval-history-ring (make-ring 200))
(defvar +amos-eval-history-index 0)

(defun +amos--ring-elements (ring)
  "Return deduplicated elements of `ring'"
  (delq nil
        (cl-remove-duplicates
         (ring-elements ring)
         :test (lambda (x y) (or (null y) (equal x y))))))

(defun +amos--ring-index-elements (ring)
  "Return elements of `ring', along with its index in a (cons)."
  (let (listing)
    (dotimes (idx (ring-length ring) listing)
      (setq listing (append (cons idx (ring-ref ring idx)) listing)))))

(defun +amos--update-history (name ring index)
  "Update history ring and current index"
  (when (or (ring-empty-p ring)
            (file-directory-p name)
            (not (eq name (ring-ref ring 0))))
    (progn
      (ring-insert ring (directory-file-name name))
      (setq index 0))))

(defun +amos--dired-jump-history (jump)
  "Move through dired history ring by increment `jump'"
  (let* ((ring +amos-dired-history-ring)
         (curr-index +amos-dired-history-index)
         (goto-idx (min
                    (max 0 (+ curr-index jump))
                    (- (ring-length ring) 1)))
         (jump-history (ring-ref ring goto-idx)))
    (message "+amos-history : %i/%i" (+ 1 goto-idx) (ring-length +amos-dired-history-ring))
    (when (and (not (= goto-idx curr-index)) jump-history)
      (setq +amos-dired-history-index goto-idx)
      (+amos/find-file jump-history t))))

(defun +amos--get-all-jump-dirs ()
  (split-string (shell-command-to-string "jump top") "\n" t))

;;;###autoload
(defun +amos/next-history ()
  "Move forward in history"
  (interactive)
  (+amos--dired-jump-history -1))

;;;###autoload
(defun +amos/prev-history ()
  "Move backward in history"
  (interactive)
  (+amos--dired-jump-history 1))

;;;###autoload
(defun +amos/up-directory (&optional other-window)
  (interactive)
  (dired-up-directory other-window)
  (+amos-store-jump-history)
  (+amos--update-history default-directory +amos-dired-history-ring +amos-dired-history-index))

;;;###autoload
(defun +amos/find-file (&optional entry ignore-history)
  (interactive)
  (let ((orig (current-buffer))
        (find-name (or entry
                       (dired-get-filename nil t))))
    (when find-name
      (if (file-exists-p find-name)
          (progn
            ;; select origination file
            (find-file find-name)
            (when (and (file-directory-p find-name)
                       (not (eq (current-buffer) orig)))
              (unless ignore-history
                (+amos--update-history find-name +amos-dired-history-ring +amos-dired-history-index))))
        (message (shell-command-to-string "jump clean"))
        (error "File doesn't exist anymore!")))))

(defun dired-mouse-find-alternate-file (event)
  "In dired, visit the file or directory you click on instead of the dired buffer."
  (interactive "e")
  (let (file)
    (save-excursion
      (with-current-buffer (window-buffer (posn-window (event-end event)))
        (save-excursion
          (goto-char (posn-point (event-end event)))
          (setq file (dired-get-filename nil t)))))
    (select-window (posn-window (event-end event)))
    (find-alternate-file (file-name-sans-versions file t))))
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-alternate-file)

;;;###autoload
(defun +amos/counsel-jumpfile-function ()
  (interactive)
  (ivy-read "Jump file: " (directory-files-recursively default-directory ".*")
            :require-match t
            :action #'+amos/find-file
            :caller #'+amos/counsel-jumpfile-function))

;;;###autoload
(defun +amos/counsel-jumpdir-function ()
  (interactive)
  (ivy-read "Jump directory: " (+amos--get-all-jump-dirs)
            :require-match t
            :action #'+amos/find-file
            :caller #'+amos/counsel-jumpdir-function))

;;;###autoload
(defun +amos-store-jump-history ()
  (shell-command! "jump chdir || true"))

;;;###autoload
(defun +amos/dired-jump ()
  (interactive)
  (+amos-store-jump-history)
  (require 'dired)
  (dired-jump)
  (recenter))

;;;###autoload
(defun +amos/show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list
    (completing-read "Select from history: "
                     (+amos--ring-elements +amos-dired-history-ring))))
  (when history (+amos/find-file history)))

;;;###autoload
(defun +amos/dired-rsync (dest)
  (interactive
   (list
    (expand-file-name
     (read-file-name
      "Rsync to:"
      (dired-dwim-target-directory)))))
  ;; store all selected files into "files" list
  (let ((files (dired-get-marked-files
                nil current-prefix-arg))
        ;; the rsync command
        (tmtxt/rsync-command
         "rsync -arvz --progress "))
    ;; add all selected file names as arguments
    ;; to the rsync command
    (dolist (file files)
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument file)
                    " ")))
    ;; append the destination
    (setq tmtxt/rsync-command
          (concat tmtxt/rsync-command
                  (shell-quote-argument dest)))
    ;; run the async shell command
    (async-shell-command tmtxt/rsync-command "*rsync*")
    ;; finally, switch to that window
    (other-window 1)))

(defun dired-toggle-sudo-internal (path &optional sudo-user)
  "Convert PATH to its sudoed version. root is used by default
unless SUDO-USER is provided."
  (let* (;; Handle the case of local files. `tramp-dissect-file-name' does
         ;; not raise an error anymore.
         (path (if (tramp-tramp-file-p path) path (concat "/:" path)))
         (file-vec (or (ignore-errors (tramp-dissect-file-name
                                       path))
                       (tramp-dissect-file-name
                        (concat "/:" path) 1)))
         (method  (tramp-file-name-method file-vec))
         (user (tramp-file-name-user file-vec))
         (host  (tramp-file-name-host file-vec))
         (localname (expand-file-name
                     (tramp-file-name-localname file-vec))))
    (when (string= system-name host)
      (setq host nil))
    (cond
     ;; remote directory -> sudo
     ((and host (string= method "scp"))
      (setq method "sudo" user sudo-user))
     ;; remote directory -> normal
     ((and host (string= method "sudo"))
      (setq method "scp" user nil))
     ;; Local directory -> normal
     ((and (not host) (string= method "scp"))
      (setq method "sudo"))
     ;; Local directory -> sudo
     ((and (not host) (string= method "sudo"))
      (setq method nil user sudo-user))
     ;; Local directory -> normal
     (t
      (setq method "sudo" user sudo-user)))
    (replace-regexp-in-string
     "^/:/" "/"
     (tramp-make-tramp-file-name method user host localname))))

(defun dired-toggle-sudo-find (fname)
  "Create a new buffer for file name FNAME."
  (let ((save-point (point)))
    (find-alternate-file fname)
    (goto-char save-point)))

;;;###autoload
(defun dired-toggle-sudo (&optional sudo-user)
  "Reopen current file or dired buffer with sudo.

If SUDO-USER is nil assume root.

If called with `universal-argument' (C-u), ask for username.
"
  (interactive "P")
  (let* ((fname (or buffer-file-name
                    dired-directory))
         (sudo-user (if current-prefix-arg
                        (read-string "Username: ")
                      sudo-user))
         (orig (current-buffer))
         (file-now (if (eq major-mode 'dired-mode)
                       (dired-get-filename t))))
    (when fname
      (setq fname (dired-toggle-sudo-internal fname sudo-user))
      (if (not (eq major-mode 'dired-mode))
          (dired-toggle-sudo-find fname)
        (kill-buffer orig)
        (dired fname)
        (when file-now
          (dired-goto-file (expand-file-name file-now fname)))))))

(defvar peep-dired-peeped-buffers ()
  "List with buffers of peeped files")

(defcustom peep-dired-cleanup-on-disable t
  "Cleanup opened buffers when disabling the minor mode"
  :group 'peep-dired
  :type 'boolean)

(defcustom peep-dired-ignored-extensions
  '("mkv" "iso" "mp4")
  "Extensions to not try to open"
  :group 'peep-dired
  :type 'list)

(defcustom peep-dired-max-size (* 10 1024 1024)
  "Do to not try to open file exteeds this size"
  :group 'peep-dired
  :type 'integer)

(defun peep-dired-scroll-page-down ()
  (interactive)
  (scroll-other-window 30))

(defun peep-dired-scroll-page-up ()
  (interactive)
  (scroll-other-window -30))

(defun peep-dired-display-file-other-window ()
  ;; (remove-hook 'post-command-hook #'peep-dired-display-file-other-window)
  (if (eq (buffer-local-value 'major-mode (window-buffer peep-dired-window)) 'dired-mode)
      (when (eq major-mode 'dired-mode)
        (let ((entry-name (dired-file-name-at-point)))
          (when entry-name
            (unless (or (member (file-name-extension entry-name)
                                peep-dired-ignored-extensions)
                        (> (nth 7 (file-attributes entry-name))
                           peep-dired-max-size))
              (remove-hook 'find-file-hook #'recentf-track-opened-file)
              (let ((buffer (find-file-noselect entry-name)))
                (add-to-list 'peep-dired-peeped-buffers buffer)
                (display-buffer buffer
                                '((display-buffer-use-some-window display-buffer-pop-up-window)
                                  (inhibit-switch-frame . t)
                                  (inhibit-same-window . t))))
              (add-hook 'find-file-hook #'recentf-track-opened-file)))))
    (peep-dired-disable)))

(defun peep-dired-cleanup ()
  (mapc 'kill-buffer-if-not-modified peep-dired-peeped-buffers)
  (setq peep-dired-peeped-buffers ()))

(defvar peep-dired-window nil)

(defun peep-dired-disable ()
  (remove-hook 'post-command-hook #'peep-dired-display-file-other-window)
  (setq peep-dired-window nil)
  (delete-other-windows)
  ;; (jump-to-register :peep_dired_before)
  (when peep-dired-cleanup-on-disable
    (mapc (lambda (b) (unless (eq b (current-buffer)) (kill-buffer-if-not-modified b))) peep-dired-peeped-buffers))
  (setq peep-dired-peeped-buffers ()))

(defun peep-dired-enable ()
  (unless (string= major-mode "dired-mode")
    (error "Run it from dired buffer"))
  ;; (window-configuration-to-register :peep_dired_before)
  (setq peep-dired-window (selected-window))
  ;; (peep-dired-display-file-other-window)
  (add-hook 'post-command-hook #'peep-dired-display-file-other-window 'append))

;;;###autoload
(defun peep-dired-toggle ()
  (interactive)
  (if peep-dired-window (peep-dired-disable)
    (peep-dired-enable)))

(after! dired-x
  (setq dired-omit-files
        (concat dired-omit-files "\\|\\.directory$"))
  (add-hook! 'dired-mode-hook
    (let ((inhibit-message t))
      (toggle-truncate-lines +1)
      (dired-omit-mode)
      (+amos-store-jump-history))))

(define-advice dired-revert (:after (&rest _) +amos*dired-revert)
  "Call `recenter' after `dired-revert'."
  (condition-case nil
      (recenter)
    (error nil)))
