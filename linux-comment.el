(require 'cl)
(defun get-prefix (string)
  "replace with spaces all printable characers but the last one
   in the passed in string, then convert series of spaces into
   tabs as appropriate and return the generated string"

  (with-temp-buffer
    (insert string)
    (goto-char (point-min))
    (while (re-search-forward "[^[:blank:]]" nil t)
       (replace-match " "))
    (goto-char (point-max))
    (delete-char -1)
    (insert "* ")
    (tabify (point-min) (point-max))
    (buffer-string)))

(defun single-line-comment(open-point close-point)
  (eq (line-number-at-pos open-point) (line-number-at-pos close-point)))

(defun* c-comment-handler()
  "reformat a comment block."
  (interactive)
  (let ((current-position (point))
	(openc "/\\*")
	(closec "\\*/")
	open-point
	close-point
	next-open-point
	line-start)
    ; find where the comment starts and ends
    (save-excursion (setq open-point (re-search-backward openc 0 t)))
    (save-excursion (setq close-point (re-search-forward closec (buffer-size) t)))
    (save-excursion (setq next-open-point (re-search-forward openc (buffer-size) t)))

    ; verify that the comment indeed exists and the point is within the
    ; comment block.
    (if (or (eq nil open-point)
	    (eq nil close-point)
	    (< next-open-point close-point))
	(progn (message "not in a comment block")
	       (return-from c-comment-handler nil)))
    (delete-trailing-whitespace open-point close-point)
    (c-fill-paragraph)
    (save-excursion (setq open-point (re-search-backward openc 0 t)))
    (save-excursion (setq close-point
			  (re-search-forward closec (buffer-size) t)))
    (if (single-line-comment open-point close-point)
	(return-from c-comment-handler nil))
    (save-excursion
      ; let's handle the start of the comment.
      (re-search-backward openc 0 t)
      (forward-char 2)
      (setq new-text (buffer-substring (line-beginning-position) (point)))
      (setq new-text (replace-regexp-in-string "/\\*" " *" new-text))
      (if (not (eolp))
	  (insert (concat "\n" new-text)))

      ; Now let's handle the end of the comment.
      (re-search-forward closec (buffer-size) t)
      (if (re-search-backward "^[[:blank:]]*\\*/" (line-beginning-position) t)
	  (replace-match (concat new-text "/"))
	(forward-char -3)
	(delete-char 3)
	(insert (concat "\n" new-text "/\n")))

      ; and now let's handle the rest of the comment block
      (goto-char open-point)
      (forward-line 2)
      (while (< (point) close-point)
	(if (not (re-search-forward "^[[:blank:]]*\\*" (line-end-position) t))
	    (if (re-search-forward "^[[:blank:]]*$" (line-end-position) t)
		(replace-match new-text)
	      (re-search-forward "^[[:blank:]]*" (line-end-position) t)
	      (replace-match (concat new-text " "))))
	(forward-line 1)))))
