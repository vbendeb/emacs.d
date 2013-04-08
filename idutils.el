(provide 'idutils)

(defvar vb-gid-stack nil)
(defvar gid-buffer-name "*gid-buffer*")
(defvar saved-window-config  nil)
(defvar saved-local-map nil)
(defvar saved-location nil)
(defvar vb-root-dir nil)

(setq debug-on-error t)

(defun jump-to-position ())

(defun vb-previous-tag ()
  (interactive)
  (if (> (length vb-gid-stack) 0)
      (let (list-node)
	(setq list-node (car (reverse vb-gid-stack)))
	(setq vb-gid-stack (reverse (cdr (reverse vb-gid-stack))))
	(if (bufferp (car list-node))
	  (progn
	      (switch-to-buffer (car list-node))
	      (goto-char (cadr list-node)))
	  (message "buffer must have been deleted?")))
    (message "at the beginning of the list (%d)" (length vb-gid-stack))))

(defun vb-sync-up ()
   (interactive)
  (let ((list-index 0)
	  (list-length (length vb-gid-stack))
          list-el)
    (message "\nlist dump (%d/%d):" list-index list-length)
    (while (< list-index list-length)
      (setq list-el (nth list-index vb-gid-stack))
      (message "%s:%d" (car list-el) (cadr list-el))
      (setq list-index (1+ list-index)))))

(defun vb-zap-tags ()
   (interactive)
   (setq vb-gid-stack nil)
   (message "tag list zapped!"))

(defun vb-gid ()
  "Run gid with the word at cursor. Use \C-p, \C-n, <cr> to browse through
   the result set"
   (interactive)
   (vb-gid-body 'gid))

(defun vb-gidf ()
  "Run gidf with the word at cursor. Use \C-p, \C-n, <cr> to browse through
   the result set"
   (interactive)
   (vb-gid-body 'gidf))

(defun get-match-n (vb-string vb-matches n-el)
  "get the substring out of the passed in string. The substring is described by
   the pair of numbers in the matches list. The number of the pair (starting
   with 0) is passed in in n-el"
   (let ((index (* 2 n-el)))
     (if (> index (length vb-matches))
       ""; empty string returned if the index exceeds the list size
       (substring vb-string (nth index vb-matches) (nth (1+ index) vb-matches)))))

(defun vb-gid-body (vb-cmd-name)
  (let ((word-to-search (word-around-point))
	   gid-buffer
           gid-buffer-line
	   vb-match-list
           new-file
	   (src-file-name (buffer-file-name))
	   found-line-count)
    (if (null word-to-search)
      (message "nothing to look up!") ; no tag to search

      (message "word to search is %s" word-to-search)
                       ; save location in the current buffer
      (setq saved-location (list (current-buffer) (point)))

	               ; find information about the tag
      (set-buffer (get-buffer-create gid-buffer-name))
      (erase-buffer)
      (setq gid-buffer (current-buffer))
      (call-process "traverse_up" nil
		    gid-buffer nil
		    (format "%s %s %s %s" default-directory src-file-name
			    vb-cmd-name word-to-search))
      (beginning-of-buffer)
      (setq vb-root-dir (buffer-substring (point) (point-at-eol)))
      (if (not (= (length vb-root-dir) 0))
	  (progn
	    (message "root-dir is %s" vb-root-dir)
	    (delete-region (point) (+ (line-end-position) 1))))
      (end-of-buffer)
      (setq found-line-count (count-lines (point) 1))
      (if (= 0 found-line-count)
	(message "no tags found for %s" word-to-search)
	(if (= 1 found-line-count)
	  (progn
	    (backward-char)
	    (handle-single-line (buffer-substring (point) 1)))
	  (handle-multi-line))))))

(defun handle-single-line (gid-buffer-line)
  (let (new-file)
    (if (equal nil
	       (string-match "\\(^[^:].*\\):\\([0-9]+\\):\\(.*\\)"
			     gid-buffer-line))
	(message "what's wrong?! no match found in %s" gid-buffer-line)
      (setq vb-match-list (match-data))
      (setq vb-gid-stack (reverse (cons saved-location (reverse vb-gid-stack))))
      (message "root dir is %s" vb-root-dir)
      (setq new-file (format "%s/%s" vb-root-dir
			     (get-match-n gid-buffer-line vb-match-list 1)))
      (message "new file is %s" new-file)
      (find-file new-file)
      (goto-line
       (string-to-number (get-match-n gid-buffer-line vb-match-list 2)))
      (recenter))))

(defun handle-multi-line ()
      ; save window config
    (setq saved-window-config (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer gid-buffer-name)
    (goto-line 1)
    (setq saved-local-map (current-local-map))
    (local-set-key  [?\C-m] 'vb-pick-string)
    (local-set-key  [?\C-g] 'vb-pick-string))

(defun vb-pick-string ()
  (interactive)
  (let (current-line start)
    (beginning-of-line)
    (setq start (point))
    (next-line 1)
    (backward-char)
    (setq current-line (buffer-substring (point) start))
    (set-window-configuration saved-window-config)
    (use-local-map saved-local-map)
     (if (= 13 last-command-char)
        (handle-single-line current-line))))

(defun word-around-point ()
  "Return the word around the point as a string."
  (save-excursion
    (if (not (eobp))
	(forward-char 1))
    (forward-word -1)
    (forward-word 1)
    (forward-sexp -1)
    (buffer-substring (point) (progn
				(forward-sexp 1)
				(point)))))
