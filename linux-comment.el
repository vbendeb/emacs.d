(require 'cl)
(defun get-prefix (string)
  "replace with spaces all printable characers but the last one
in the passed in string, then convert series of spaces into tabs
as appropriate and return the generated string"

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

(defun* fix-linux-comment()
  "reformat a comment block as created by emacs c-fill-paragraph
into a linux kernel style compliant block."
  (interactive)
  (let ((current-position (point))
	(openc "^.*/\\*")
	(closec "\\*/")
	open-point
	close-point
	eol-point
	x-point
	prefix)
    (save-excursion)

    ; find where the comment starts and ends
    (setq open-point (re-search-backward openc 0 t)
	  close-point (re-search-forward closec (buffer-size) t))

    ; verify that the comment indeed exists and the point is within the
    ; comment block.
    (if (or (eq nil open-point)
	    (eq nil close-point)
	    (< close-point current-position))
	(progn (message "not in a comment block %d %d %d"
			open-point
			current-position
			close-point)
	       (return-from fix-linux-comment nil)))

    ; Inset a newline after the opening pattern if it is not on a separate
    ; line.
    (goto-char open-point)
    (setq eol-point (progn (end-of-line) (point)))
    (goto-char open-point)
    (setq x-point (re-search-forward openc close-point))
    (if (not (equal x-point eol-point))
	(progn (newline)
	       (previous-line)
	       (end-of-line)))

    ; generate a prefix pattern from the opening line of the comment
    (setq prefix (get-prefix (buffer-substring open-point (point))))

    (setq open-point (progn (next-line 2) (point)))

    ; get to the end of comment
    (re-search-forward closec (buffer-size))

    ; replace the space before closing sequence with a newline
    (forward-char -3)(delete-char 1)(newline)

    ; insert prefix before the closing sequence (removing the last two
    ; charactes of the prefix) - which completes the last line of the
    ; multiline C comment
    (insert prefix)
    (delete-char -2)

    ; now replace all lines' prefixes with the comment prefix.
    (while (> (point) open-point)
      (beginning-of-line)
      (setq eol-point (point))
      (previous-line)
      (re-search-forward "^[[:blank:]]*" eol-point)
      (replace-match prefix))))

(defun* linux-comment()
  (interactive)
  (c-fill-paragraph)
  (linux-fill-paragraph))