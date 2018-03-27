;;; private/amos/autoload/amos.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +amos/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +amos/yank-buffer-filename-nondir ()
  "Copy the current buffer's filename to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (file-name-nondirectory filename)))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +amos/yank-buffer-filename-with-line-position ()
  "Copy the current buffer's filename with line number to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (concat "b " filename ":" (number-to-string (line-number-at-pos)))))
    (error "Couldn't find filename in current buffer")))

;;;###autoload
(defun +amos/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

;;;###autoload
(defun +amos/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

;;;###autoload
(defun +amos/copy-without-useless-indent-and-newline ()
  (interactive)
  (let ((inhibit-message t))
    (when (evil-visual-state-p)
      (call-interactively #'narrow-reindent-to-region)
      ;; (call-interactively #'narrow-to-region)
      (set-mark (point-min))
      (goto-char (point-max))
      (backward-char)
      (end-of-line)
      (call-interactively #'copy-region-as-kill)
      (narrow-reindent-widen)
      ;; (widen)
      (recenter))))

(defun +amos/copy-without-useless-indent-and-newline2 ()
  (interactive)
  (let ((inhibit-message t))
    (when (evil-visual-state-p)
      (call-interactively #'narrow-reindent-to-region)
      (goto-char (point-max))
      (backward-char)
      (end-of-line)
      (let ((text (filter-buffer-substring (point-min) (point))))
        (evil-set-register ?y text))
      (call-interactively #'narrow-reindent-widen)
      (recenter))))

;;;###autoload
(defun +amos/evil-visual-insert-snippet ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (+amos/copy-without-useless-indent-and-newline2)
    (setq yas--condition-cache-timestamp (current-time))
    (let* ((yas-wrap-around-region 121)
           (templates (yas--all-templates (yas--get-snippet-tables)))
           (yas--current-template (and templates
                                       (or (and (cl-rest templates) ;; more than one template for same key
                                                (yas--prompt-for-template templates))
                                           (car templates))))
           (_ (evil-substitute start end 'line ?_))
           (where (cons (point) (point))))
      (if yas--current-template
          (progn
            (yas-expand-snippet (yas--template-content yas--current-template)
                                (car where)
                                (cdr where)
                                (yas--template-expand-env yas--current-template)))
        (yas--message 1 "No snippets can be inserted here!")))))

;;;###autoload
(defun shell-command! (command)
  (let ((inhibit-message t))
    (shell-command command)))

;;;###autoload
(defun +amos/tmux-detach ()
  "Detach if inside tmux."
  (interactive)
  (shell-command! "tmux detach-client"))

;;;###autoload
(defun +amos/tmux-switch-window (&optional next)
  "Switch window if inside tmux."
  (interactive)
  ;; (push (selected-frame) last-frame)
  (if next
      (shell-command! (format "tmux next-window; tmux send f12"))
    (shell-command! (format "tmux previous-window; tmux send f12"))))

;;;###autoload
(defun +amos/tmux-select-window (num)
  "Select window if inside tmux."
  (interactive)
  (shell-command! (format "tmux select-window -t %d; tmux send f12" num)))

;;;###autoload
(defun +amos/tmux-new-window (&optional func)
  "New window if inside tmux."
  (interactive)
  (if (functionp func)
      (shell-command! (format "tmux new-window emacsclient -t -eval '(%s)'" (symbol-name func)))
    (shell-command! "tmux new-window emacsclient -t"))
  (+amos/update-tmux-modeline))

;;;###autoload
(defun +amos/find-file-other-frame (filename &optional wildcards)
  "Open file if inside tmux."
  (interactive
   (find-file-read-args "Find file in other frame: "
                        (confirm-nonexistent-file-or-buffer)))
  (shell-command! (format "tmux new-window emacsclient -t -eval '(find-file \"%s\" \"%s\")'" filename wildcards)))

;;;###autoload
(defun +amos/switch-to-buffer-other-frame (buffer-or-name &optional norecord)
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other frame: ")))
  (shell-command! (format "tmux new-window emacsclient -t -eval '(switch-to-buffer \"%s\" %s)'" buffer-or-name norecord)))

;;;###autoload
(defun +amos/tmux-fork-window ()
  "Detach if inside tmux."
  (interactive)
  (+amos-store-jump-history)
  (shell-command! (format "tmux switch-client -t amos; tmux run -t amos \"tmux new-window -c %s\"" default-directory)))

;;;###autoload
(defun +amos/tmux-kill-window ()
  "Kill tmux window if inside tmux."
  (interactive)
  (shell-command! "tmux send -t $(tmux display -pt'{last}' '#{pane_id}') f12; tmux kill-window"))

;;;###autoload
(defun +amos/tmux-source ()
  "Source tmux config if inside tmux."
  (interactive)
  (shell-command! "tmux source-file ~/.tmux/.tmux.conf.emacs"))

(evil-define-operator +amos/evil-commentary-yank-line (beg end type)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R>")
  (let* ((beg (save-excursion (beginning-of-line) (point)))
         (end (save-excursion (end-of-line) (point)))
         (column (evil-column))
         (line (buffer-substring-no-properties beg end)))
    (evil-commentary-line beg end)
    (end-of-line)
    (open-line 1)
    (forward-line 1)
    (insert line)
    (move-to-column column)))

;;;###autoload
(defun +amos/org-babel-edit (arg)
  "Edit the source block at point in a popup.

If ARG is non-nil (universal argument), use the current window."
  (interactive "P")
  (if arg
      (call-interactively #'org-edit-special)
    (with-popup-rules! (("^\\*Org Src" :regexp t :size 0.5 :select t :align 'right :noesc t))
      (call-interactively #'org-edit-special))))

;;;###autoload
(defun +amos/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

;;;###autoload
(defun +snippets/goto-start-of-field ()
  "Go to the beginning of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-beginning-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/goto-end-of-field ()
  "Go to the end of the current field."
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
        (move-end-of-line 1)
      (goto-char position))))

;;;###autoload
(defun +snippets/delete-backward-char (&optional field)
  "Prevents Yas from interfering with backspace deletion."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((eq (point) (marker-position (yas--field-start field))) nil)
          (t (delete-char -1)))))

;;;###autoload
(defun +snippets/delete-forward-char-or-field (&optional field)
  "Delete forward, or skip the current field if it's empty. This is to prevent
buggy behavior when <delete> is pressed in an empty field."
  (interactive)
  (let ((field (or field (and yas--active-field-overlay
                              (overlay-buffer yas--active-field-overlay)
                              (overlay-get yas--active-field-overlay 'yas--field)))))
    (cond ((and field
                (not (yas--field-modified-p field))
                (eq (point) (marker-position (yas--field-start field))))
           (yas--skip-and-clear field)
           (yas-next-field 1))
          ((eq (point) (marker-position (yas--field-end field))) nil)
          (t (delete-char 1)))))

;;;###autoload
(defun +snippets/delete-to-start-of-field (&optional field)
  "Delete to start-of-field."
  (interactive)
  (let* ((field (or field (and yas--active-field-overlay
                               (overlay-buffer yas--active-field-overlay)
                               (overlay-get yas--active-field-overlay 'yas--field))))
         (sof (marker-position (yas--field-start field))))
    (when (and field (> (point) sof))
      (delete-region sof (point)))))
