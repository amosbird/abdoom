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
(defun +amos/evil-visual-insert-snippet ()
  (interactive)
  (when (evil-visual-state-p)
    (call-interactively #'narrow-to-region)
    (execute-kbd-macro "gv")
    (setq evil-this-register ?y)
    (execute-kbd-macro "y")
    (call-interactively #'widen)
    (execute-kbd-macro "gv")
    (setq evil-this-register ?n)
    (call-interactively #'evil-substitute)
    (yas-insert-snippet)))

;;;###autoload
(defun +amos/tmux-detach ()
  "Detach if inside tmux."
  (interactive)
  (+tmux/run "detach-client"))

;;;###autoload
(defun +amos/copy-and-comment-lines-inverse (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line t))
    (evilnc-copy-and-comment-lines arg)))

;;;###autoload
(defun +amos/copy-and-comment-lines (&optional arg)
  (interactive "p")
  (let ((evilnc-invert-comment-line-by-line nil))
    (evilnc-copy-and-comment-lines arg)))

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
