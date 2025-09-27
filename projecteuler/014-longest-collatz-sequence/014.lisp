(defun collatz-num (n)
  (if (evenp n) (/ n 2)
      (+ (* 3 n) 1)))

(defun collatz-length (n)
  (labels ((rec (num len)
	     (if (= num 1)
		 len
		 (rec (collatz-num num) (1+ len)))))
    (rec n 1)))


(defparameter *collatz-table* (make-hash-table))
(defun memorized-collatz-length (n)
  (if (= n 1)
      1
      (if (gethash n *collatz-table*)
	  (gethash n *collatz-table*)
	  (progn
	    (let* ((next-collatz-num (collatz-num n))
		   (next-collatz-len (memorized-collatz-length next-collatz-num))
		   (collatz-len (1+ (memorized-collatz-length next-collatz-num))))
	      (setf (gethash n *collatz-table*) collatz-len)
	      collatz-len)))))
	      
	      
(defun solve-014 (n)
  (let ((max-len -1)
	(max-num -1))
    (do ((i 1 (1+ i)))
	((>= i n) (values max-len max-num))
      (let ((len (memorized-collatz-length i)))
	(if (> len max-len)
	    (progn
	      (setf max-len len)
	      (setf max-num i)))))))

;;; (solve-014 1000000) -> 525 837799
