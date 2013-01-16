
(defvar opening-bracket nil )
(defvar closing-bracket nil )
(defvar on-the-spot     nil )
(defvar check-number    0 )
(defvar move-right 1)

(defun find-matching-bracket ()
  (interactive)
  (let (( cur-point (point))) ; cur-point is our current position in the file
    ; try finding a bracket going forward in the file
    ( setq
      opening-bracket nil
      on-the-spot     t
      check-number    0
      move-right      1
    )
    ( while ( and
	       (not (eobp))
	       (not opening-bracket)
             )
       (evaluate-bracket)
    )
    ( if (null opening-bracket)
      ; no brackets going forward, let's see if there are ones going back
         ( progn
            ( goto-char cur-point )
            ( setq move-right 0 )
	    ( while ( and
	       (not ( bobp ) )
	       (not opening-bracket)
              )
	      (evaluate-bracket)
	    )
         )
      )
     ( if (null opening-bracket)
      ( progn
        ( goto-char cur-point )
        ( message "no brackets in this file" )
      )
      ( if ( and
             on-the-spot
	     (/= (goto-matching-bracket) 0 )
           )
        ( progn
          ( goto-char cur-point )
          ( message "bracket mismatch!" )
         )
      )
    )
  )
)

(defun char-at-point ()
  ( if (eobp)
    (backward-char))
  ( buffer-substring (point) (1+ (point)))
)

(defun evaluate-bracket ()
   ( let ((this-char (char-at-point)))
     ( setq check-number (1+ check-number))
     ( cond
       ( (equal this-char "(" ) ( feed-vars "(" ")" 1 ) )
       ( (equal this-char "[" ) ( feed-vars "[" "]" 1 ) )
       ( (equal this-char "{" ) ( feed-vars "{" "}" 1 ) )
       ( (equal this-char "<" ) ( feed-vars "<" ">" 1 ) )
       ( (equal this-char ")" ) ( feed-vars ")" "(" 0 ) )
       ( (equal this-char "]" ) ( feed-vars "]" "[" 0 ) )
       ( (equal this-char "}" ) ( feed-vars "}" "{" 0 ) )
       ( (equal this-char ">" ) ( feed-vars ">" "<" 0 ) )
       ( t
          ( if (= check-number 1 )
	      (setq on-the-spot nil)
	  )
          ( move-point )
       )
     ) 
   ) 
)

(defun feed-vars (v1 v2 v3 )
  ( setq opening-bracket v1 )
  ( setq closing-bracket v2 )
  ( setq move-right v3 )
)

(defun move-point ()
  ( if (/= move-right 0 )
     ( if ( eobp )
        0
        ( progn
          (forward-char 1)
          1
        )
     )
     ( if ( bobp )
        0
        ( progn
          (backward-char 1 )
          1
        )
     )
   )
)

(defun goto-matching-bracket ()
  ( let ( this-char ( bracket-counter 1 ) )
    ( while ( and (/= bracket-counter 0 ) ( move-point ) )
      ( setq this-char (char-at-point) )
      ( cond
       ( (equal this-char opening-bracket) (setq bracket-counter (1+ bracket-counter )) )
       ( (equal this-char closing-bracket) (setq bracket-counter (1- bracket-counter )) )
      )
    )
    bracket-counter
  )
)

(defun finish-here ( par1 )
  ( message par1 )
  ( while t )
)
