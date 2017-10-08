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

;;;###autoload (autoload '+amos:redisplay-and-recenter "private/amos/autoload/evil" nil t)
(evil-define-command +amos:redisplay-and-recenter ()
  :repeat nil
  (interactive)
  (redraw-display)
  (recenter))

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

;;;###autoload (autoload '+amos:evil-find-file-at-point-with-line "private/amos/autoload/evil" nil t)
(evil-define-command +amos:evil-find-file-at-point-with-line ()
  "Opens the file at point and goes to line-number."
  (interactive)
  (let ((fname (with-no-warnings (ffap-file-at-point))))
    (if fname
        (let ((line
               (save-excursion
                 (goto-char (cadr ffap-string-at-point-region))
                 (and (re-search-backward ":\\([0-9]+\\)\\=" (line-beginning-position) t)
                      (string-to-number (match-string 1))))))
          (with-no-warnings (ffap))
          (when line
            (goto-char (point-min))
            (forward-line (1- line))))
      (user-error "File does not exist."))))

;;;###autoload (autoload '+amos:previous-open-delim "private/amos/autoload/evil" nil t)
(evil-define-motion +amos:previous-open-delim (count)
  "Go to [count] previous closest unmatched '([{'."
  :type exclusive
  (let* ((paren (save-excursion (if (eq 0 (evil-up-paren ?( ?) (- (or count 1)))) (point) nil)))
         (bracket (save-excursion (if (eq 0 (evil-up-paren ?[ ?] (- (or count 1)))) (point) nil)))
         (brace (save-excursion (if (eq 0 (evil-up-paren ?{ ?} (- (or count 1)))) (point) nil)))
         (delim (condition-case nil (-max (--filter it (list paren bracket brace))) (error nil))))
    (if delim
        (progn
          (goto-char delim)
          (set-match-data (list (point) (1+ (point))))
          0)
      -1)))

;;;###autoload (autoload '+amos:next-close-delim "private/amos/autoload/evil" nil t)
(evil-define-motion +amos:next-close-delim (count)
  "Go to [count] next closest unmatched ')]}'."
  :type exclusive
  (forward-char)
  (let* ((paren (save-excursion (if (eq 0 (evil-up-paren ?( ?) (or count 1))) (point) nil)))
         (bracket (save-excursion (if (eq 0 (evil-up-paren ?[ ?] (or count 1))) (point) nil)))
         (brace (save-excursion (if (eq 0 (evil-up-paren ?{ ?} (or count 1))) (point) nil)))
         (delim (condition-case nil (-min (--filter it (list paren bracket brace))) (error nil))))
    (if delim
        (progn
          (goto-char delim)
          (set-match-data (list (1- (point)) (point)))
          0)
      -1))
  (backward-char))
