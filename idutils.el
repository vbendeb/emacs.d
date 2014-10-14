(provide 'idutils)

(defvar vb-gid-stack nil)
(defvar gid-buffer-name "*gid-buffer*")
(defvar saved-window-config  nil)
(defvar saved-local-map nil)
(defvar saved-location nil)
(defvar vb-root-dir nil)
(defvar vb-root-map (list))

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

(defun append-if-new (old-list new-elt)
  (let (found
	elt)
    (setq found nil)
    (dolist (elt old-list)
      (if (equal elt new-elt)
	  (setq found 't)))
    (if (eq found nil)
	(append (list new-elt) old-list)
      old-list)))

(defun get-root-dirs ()
  (let (bufer-name
	file-name
	obj-name
	(dir-list (list)))
    (dolist (buffer-name (buffer-list))
      (setq file-name (buffer-file-name buffer-name))
      (if file-name
	  (while (not (equal file-name "/"))
	    (setq file-name (file-name-directory
			     (directory-file-name file-name)))
	    (if (file-accessible-directory-p
		 (concat file-name ".git"))
		(progn
		  (setq dir-list (append-if-new dir-list file-name))
		  (setq file-name "/"))
	      (if (file-readable-p
		   (concat file-name "ID"))
		  (progn
		    (setq dir-list (append-if-new dir-list file-name))
		    (setq file-name "/")))))))
    (message "dirs are %s" dir-list)
    dir-list))

(defun vb-gid-body (vb-cmd-name)
  (let ((gid-buf-cursor 0)
	(gid-buf-line-count 0)
	(root-dir-map (list))
	(word-to-search (word-around-point))
	added-chars
	gid-buffer
	point-before
	root-dir-name)
    (if (null word-to-search)
	(message "nothing to look up!") ; no tag to search
	; save location in the current buffer
      (setq vb-root-map (list))
      (setq saved-location (list (current-buffer) (point)))
	; find information about the tag
      (set-buffer (get-buffer-create gid-buffer-name))
      (erase-buffer)
      ; for some not completely understood reason using (delete-char -1) on a
      ; buffer which has a single '\n' character in it causes a failure. Just
      ; insert a space into the buffer here and drop it in the end.
      (insert " ")
      (setq gid-buffer (current-buffer))
      (dolist (root-dir-name (get-root-dirs))
	(setq point-before (point))
	(call-process "traverse_up" nil
		      gid-buffer nil
		      (format "%s %s %s"
			      root-dir-name
			      vb-cmd-name
			      word-to-search))
	(setq added-chars (- (point) point-before))
	(if (<= added-chars 1)
	    (delete-char (- added-chars)) ; get rid of the empty string
	  (setq point-before (point))
	  (setq gid-buf-line-count (count-lines 1 (point)))
	  (setq root-dir-map (append
			      root-dir-map
			      (list (list
				     gid-buf-cursor
				     gid-buf-line-count
				     root-dir-name))))
	  (setq gid-buf-cursor gid-buf-line-count)))

      ; remove the space added in the beginning
      (goto-char (point-min))
      (delete-char 1)
      (goto-char (point-max))

      (if (= 0 gid-buf-line-count)
	  (message "no tags found for %s" word-to-search)
        (if (= 1 gid-buf-line-count)
	    (progn
	      (setq vb-root-dir (car (last (car root-dir-map))))
	      (backward-char)
	      (handle-single-line (buffer-substring (point) 1)))
	  (setq vb-root-map root-dir-map)
          (handle-multi-line))))))

(defun handle-single-line (gid-buffer-line)
  (let (new-file)
    (if (equal nil
               (string-match "\\(^[^:].*\\):\\([0-9]+\\):\\(.*\\)"
                             gid-buffer-line))
        (message "what's wrong?! no match found in %s" gid-buffer-line)
      (setq vb-match-list (match-data))
      (setq vb-gid-stack (reverse (cons saved-location (reverse vb-gid-stack))))
      (setq new-file (format "%s/%s" vb-root-dir
                             (get-match-n gid-buffer-line vb-match-list 1)))
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
  (let (current-line
	start
	this-line-num)
    (beginning-of-line)
    (setq start (point))
    (next-line 1)
    (backward-char)
    (setq current-line (buffer-substring (point) start))
    (set-window-configuration saved-window-config)
    (use-local-map saved-local-map)
    (if (= 13 last-command-event)
	(progn ; need to find the root directory of this line
	  (setq this-line-num (- (count-lines 1 (point)) 1))
	  (dolist (vb-root vb-root-map)
	    (let ((first-line (car vb-root))
		  (last-line (car (cdr vb-root))))
	      (if (and (>= this-line-num first-line)
		       (<= this-line-num last-line))
		  (setq vb-root-dir (car (last vb-root))))))
	  (handle-single-line current-line)))))

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
