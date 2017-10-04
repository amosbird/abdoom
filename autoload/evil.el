;;; private/amos/autoload/evil.el -*- lexical-binding: t; -*-

;;;###autoload (autoload '+amos:multi-next-line "private/amos/autoload/evil" nil t)
(evil-define-motion +amos:multi-next-line (count)
  "Move down 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (* 6 (or count 1)))))

;;;###autoload (autoload '+amos:multi-previous-line "private/amos/autoload/evil" nil t)
(evil-define-motion +amos:multi-previous-line (count)
  "Move up 6 lines."
  :type line
  (let ((line-move-visual (or visual-line-mode (derived-mode-p 'text-mode))))
    (evil-line-move (- (* 6 (or count 1))))))

;;;###autoload (autoload '+amos:cd "private/amos/autoload/evil" nil t)
(evil-define-command +amos:cd ()
  "Change `default-directory' with `cd'."
  (interactive "<f>")
  (cd input))

;;;###autoload (autoload '+amos:kill-all-buffers "private/amos/autoload/evil" nil t)
(evil-define-command +amos:kill-all-buffers (&optional bang)
  "Kill all buffers. If BANG, kill current session too."
  (interactive "<!>")
  (if bang
      (+workspace/kill-session)
    (doom/kill-all-buffers)))

;;;###autoload (autoload '+amos:kill-matching-buffers "private/amos/autoload/evil" nil t)
(evil-define-command +amos:kill-matching-buffers (&optional bang pattern)
  "Kill all buffers matching PATTERN regexp. If BANG, only match project
buffers."
  (interactive "<a>")
  (doom/kill-matching-buffers pattern bang))
