(defvar marked-point-1 nil )
(defvar marked-point-2 nil )
(defvar marked-point-3 nil )
(defvar marked-point-4 nil )
(defvar marked-point-5 nil )
(defvar marked-point-6 nil )
(defvar marked-point-7 nil )
(defvar marked-point-8 nil )
(defvar marked-point-9 nil )

(defun normalize-cmd-key (key-code)
  (setq rv (- key-code m0-key-code)))

(defun set-mark-point ()
"Set a mark point to remember position in a buffer
 (valid point numbers are between 1 and 9).
 To return to the set point use <\M-j><number>"
    (interactive)
     (let (point-name (cmd-key (normalize-cmd-key last-command-char)))

        (if (or (< cmd-key 1) (> cmd-key 9))
            (message "the key is %d how did you get here?" cmd-key)
            (progn
	      (setq point-name (concat "marked-point-" (number-to-string cmd-key)))
	      (set (intern-soft point-name) (list (current-buffer) (point)))
              (message "mark point %d set" cmd-key)))))

(defun goto-mark-point (mark-point)
    "Return to a previously set mark point (see set-mark-point)"
    (interactive "nMark point: ")
     (let (point-list (point-name (concat "marked-point-"
					  (number-to-string mark-point))))
       (if (or(> mark-point 9) (< mark-point 1))
	   (message "%d is a bad mark point number (should be 1 to 9)" mark-point)
	 (progn
	   (setq point-list (symbol-value (intern-soft point-name)))
	   (if (equal point-list nil)
	       (message "mark point %d not set" mark-point)
	     (progn
		  (switch-to-buffer (car point-list))
		  (goto-char (cadr point-list))))))))
