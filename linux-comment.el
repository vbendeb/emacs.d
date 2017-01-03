(require 'cl)

(defun single-line-comment(open-point close-point)
  (eq (line-number-at-pos open-point) (line-number-at-pos close-point)))

(defun* check-and-handle-cpp-comment()
  (if (not (re-search-backward "//" (line-beginning-position) t))
      (return-from check-and-handle-cpp-comment nil))
  (c-fill-paragraph)
  (let ((current-line (line-number-at-pos))
	first-line
	last-line
	new-text)
    (setq first-line current-line
	  last-line current-line)
    (save-excursion
      (catch 'lowest
	(while (> first-line 1)
	  (progn
	    (forward-line -1)
	    (if (re-search-forward "^[[:blank:]]*//" (line-end-position) t)
		(setq first-line (- first-line 1))
	      (throw 'lowest t))))))
    (save-excursion
      (catch 'highest
	(while (not (eobp))
	  (progn
	    (forward-line 1)
	    (if (re-search-forward "^[[:blank:]]*//" (line-end-position) t)
		(setq last-line (+ last-line 1))
	      (throw 'highest t))))))
    (if (not (eq first-line last-line))
	(progn
	  (goto-line current-line)
	  (re-search-forward "^[[:blank:]]*//" (line-end-position) t)
	  (setq new-text (buffer-substring (line-beginning-position) (point)))
	  (goto-line first-line)
	  (insert (concat new-text "\n"))
	  (goto-line (+ last-line 1))
	  (goto-char (line-end-position))
	  (insert (concat "\n" new-text))))))

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
	    (and next-open-point
		 (< next-open-point close-point)))
        ; we are not in a regular comment, maybe this is a c++ comment?
	(progn
	  (save-excursion (check-and-handle-cpp-comment))
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
	(insert (concat "\n" new-text "/")))

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
