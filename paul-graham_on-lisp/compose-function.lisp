;; 5.4

;; fig5.3
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity ;;fns nilのとき
      ))
	


;; fig5.4
(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	  (if else ;;elseがあるときだけ実行される
	      (funcall else x)))))


;;;; 関数集合の合併のオペレータ
(defun fint (fn &rest fns)
  "関数集合の積、 intersection"
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x) 
		 (funcall chain x))))))
;;展開すると (and (funcall fn x) (funcall fn1 x) (funcall fn2 x) ...) 

(defun fun (fn &rest fns)
  "関数集合の和、union"
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	#'(lambda (x)
	    (or (funcall fn x) (funcall chain x))))))
;;展開すると (or (funcall fn x) (funcall fn1 x) (funcall fn2 x) ...) 
		
		
