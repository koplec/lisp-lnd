;; fig13.3 Generating a rhyming dictionary
(defparameter dict (make-array 25000 :fill-pointer 0))

(defun read-words (from)
  (setf (fill-pointer dict) 0)
  (with-open-file (in from :direction :input)
    (do ((w (read-line in nil :eof)
	    (read-line in nil :eof)))
	((eql w :eof))
      (vector-push w dict))))

(defun xform (fn seq)
  (map-into seq fn seq))

(defun write-words (to)
  (with-open-file (out to :direction :output
			  :if-exists :supersede) ;;supersede:取って代わる、置き換わる
    (map nil #'(lambda (x)
		 (fresh-line out)
		 (princ x out))
	 (xform #'nreverse
		(sort (xform #'nreverse dict)
		      #'string<)))))


  
