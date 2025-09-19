;; iteration without loop
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setf wins obj
		    max score))))
	(values wins max))))


;; fig 14.2 iteration with loop
(defun most2 (fn lst)
  (if (null lst)
      (values nil nil)
      (loop with wins = (car lst)
	    with max = (funcall fn wins)
	    for obj in (cdr lst)
	    for score = (funcall fn obj)
	    when (> score max)
	      do (setf wins obj
		       max score)
	    finally (return (values wins max)))))
      
