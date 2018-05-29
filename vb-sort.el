(defun at-space()
  (interactive)
  (let ((cap (char-after)))
    (or (eq cap 32) (eq cap 9))))

(defun at-end ()
  (interactive)
  (eq (point) (point-max)))

(defun next-word ()
  (interactive)
  (if (not (at-end))
      (forward-char)))

(defun end-rec ()
  (interactive)
  (while (and (not (at-end)) (not (at-space)))
    (forward-char))
  (while (and (not (at-end)) (at-space))
    (forward-char))
  (if (not (at-end))
      (backward-char)))

(defun sort-range ()
  (interactive)
  (let (line-breaks-in-region)
    (if (not (use-region-p))
	(message "No highligted region found")
      (if (eq (count-lines (region-beginning) (region-end)) 1)
	  (progn
	    (save-excursion
	      (save-restriction
		(narrow-to-region (region-beginning) (region-end))
		(goto-char (point-min))
		(sort-subr nil 'next-word 'end-rec))))
	(sort-lines nil (region-beginning) (region-end))))))


