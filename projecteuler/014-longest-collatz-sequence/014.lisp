(defpackage :pe014
  (:use :cl :kplb)
  (:export :collatz-length
	   :memorized-collatz-length
	   ))

(in-package :pe014)

(defun collatz-num (n)
  (if (evenp n) (/ n 2)
      (+ (* 3 n) 1)))

(defun collatz-length (n)
  (labels ((rec (num len)
	     (if (= num 1)
		 len
		 (rec (collatz-num num) (1+ len)))))
    (rec n 1)))

;;; (collatz-length 2) 2
;;; (collatz-length 13) 10

(defparameter *collatz-table* (make-hash-table :test #'eql))
(setf (gethash 1 *collatz-table*) 1)
(defun memorized-collatz-length (n)
  ;; multiple-value-bindで返り値の2番めで判定する
  (multiple-value-bind (v ok?) (gethash n *collatz-table*)
    (if ok? v
	(let* ((next-collatz-num (collatz-num n))
	       (next-collatz-len (memorized-collatz-length next-collatz-num))
	       (collatz-len (1+ next-collatz-len)))
	  (setf (gethash n *collatz-table*) collatz-len)
	  collatz-len))))
	      
	      
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
;;; (solve-014 2) -> 1 1
;;; (solve-014 10) -> 20 9
