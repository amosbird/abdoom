;;; private/amos/autoload/amos.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +amos/install-snippets ()
  "Install my snippets from https://github.com/amosbird/emacs-snippets into
private/amos/snippets."
  (interactive)
  (doom-fetch :github "amosbird/emacs-snippets"
              (expand-file-name "snippets" (doom-module-path :private 'amos))))

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

(defmacro +amos-def-finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "+amos/find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir)
             projectile-require-project-root
             projectile-cached-buffer-file-name
             projectile-cached-project-root)
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "+amos/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload '+amos/find-in-templates "private/amos/autoload/amos" nil t)
;;;###autoload (autoload '+amos/browse-templates "private/amos/autoload/amos" nil t)
(+amos-def-finder! templates +file-templates-dir)

;;;###autoload (autoload '+amos/find-in-snippets "private/amos/autoload/amos" nil t)
;;;###autoload (autoload '+amos/browse-snippets "private/amos/autoload/amos" nil t)
(+amos-def-finder! snippets +amos-snippets-dir)

;;;###autoload (autoload '+amos/find-in-dotfiles "private/amos/autoload/amos" nil t)
;;;###autoload (autoload '+amos/browse-dotfiles "private/amos/autoload/amos" nil t)
(+amos-def-finder! dotfiles (expand-file-name ".dotfiles" "~"))

;;;###autoload (autoload '+amos/find-in-emacsd "private/amos/autoload/amos" nil t)
;;;###autoload (autoload '+amos/browse-emacsd "private/amos/autoload/amos" nil t)
(+amos-def-finder! emacsd doom-emacs-dir)

;;;###autoload (autoload '+amos/find-in-notes "private/amos/autoload/amos" nil t)
;;;###autoload (autoload '+amos/browse-notes "private/amos/autoload/amos" nil t)
(+amos-def-finder! notes +org-dir)

;;;###autoload
(defun +amos/open-todo-file ()
  (interactive)
  (find-file "~/org/todo.org"))

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
