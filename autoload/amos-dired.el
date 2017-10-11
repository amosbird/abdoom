;;; private/amos/autoload/amos-dired.el -*- lexical-binding: t; -*-

(require 'dired)
(require 'ring)
(require 'cl-seq)

(defvar +amos-history-ring (make-ring 200))
(defvar +amos-history-index 0)

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

(defun +amos--jump-history (jump)
  "Move through history ring by increment `jump'"
  (let* ((ring +amos-history-ring)
         (curr-index +amos-history-index)
         (goto-idx (min
                    (max 0 (+ curr-index jump))
                    (- (ring-length ring) 1)))
         (jump-history (ring-ref ring goto-idx)))
    (message "+amos-history : %i/%i" (+ 1 goto-idx) (ring-length +amos-history-ring))
    (when (and (not (= goto-idx curr-index)) jump-history)
      (setq +amos-history-index goto-idx)
      (+amos/find-file jump-history t))))

(defun +amos--update-history (name)
  "Update history ring and current index"
  (when (or (ring-empty-p +amos-history-ring)
            (not (eq name (ring-ref +amos-history-ring 0))))
    (progn
      (ring-insert +amos-history-ring (directory-file-name name))
      (setq +amos-history-index 0))))

(defun +amos--get-all-current-files ()
  (split-string (shell-command-to-string "ls -a") "\n" t))

(defun +amos--get-all-jump-dirs ()
  (split-string (shell-command-to-string "jump top") "\n" t))

;;;###autoload
(defun +amos/next-history ()
  "Move forward in history"
  (interactive)
  (+amos--jump-history -1))

;;;###autoload
(defun +amos/prev-history ()
  "Move backward in history"
  (interactive)
  (+amos--jump-history 1))

(defun +amos/kill-all-other-dired-buffers (&optional current-buf)
  "kill all dired-buffers and diredp-w32-drivers-mode(w32 use this mode )
  except current-buf ,if current-buf is nil then kill all"
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (not (eq current-buf buf))
                 (or  (eq 'dired-mode  major-mode)
                      (eq 'diredp-w32-drives-mode major-mode)))
        (kill-buffer buf)))))

(defadvice dired (before dired-single-buffer activate)
  "Replace current buffer if file is a directory."
  (+amos/kill-all-other-dired-buffers))

;;;###autoload
(defun +amos/up-directory (&optional other-window)
  (interactive)
  (dired-up-directory other-window)
  (+amos--update-history default-directory)
  (+amos/kill-all-other-dired-buffers (current-buffer)))

;;;###autoload
(defun +amos/find-file (&optional entry ignore-history)
  (interactive)
  (let ((orig (current-buffer))
        (find-name (or entry
                       (dired-get-filename nil t))))
    (when find-name
      (if (file-exists-p find-name)
          (progn
            (unless ignore-history
              (+amos--update-history find-name))
            ;; select origination file
            (find-file find-name)
            (when (and (file-directory-p find-name)
                       (not (eq (current-buffer) orig)))
              (+amos/kill-all-other-dired-buffers (current-buffer))))
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
(defun +amos/dired-jump ()
  (interactive)
  (require 'dired)
  (dired-jump))

;;;###autoload
(defun +amos/show-history (history)
  "Show history prompt for recent directories"
  (interactive
   (list
    (completing-read "Select from history: "
                     (+amos--ring-elements +amos-history-ring))))
  (when history (+amos/find-file history)))
