;; fig10.2

(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop)) ;;2回stopを評価しないように？
	 ((> ,var ,gstop))
       ,@body)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))


(defmacro random-choice (&rest exprs)
  `(case (random ,(length exprs)) ;;lengthより小さい0以上の整数
     ,@(let ((key -1))
	 (mapcar #'(lambda (expr)
		     `(,(incf key) ,expr)) ;;これでどんどんkeyをincrementするのは思いつかない。
		 exprs))))

(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
	  syms)
     ,@body))

;;with-gensymsの例
(defmacro square (expr)
  (with-gensyms (g)
    `(let ((,g ,expr))
       (* ,g ,g))))


(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
