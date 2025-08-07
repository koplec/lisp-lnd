;; 6.6 Example: Function Builders

;; 欲を言えば、fns同士のinputとoutputの型の整合性をチェックする機構があるといいなと思った
;; compose実行したときにfnの型を調べる感じ
(defun compose (&rest fns) 
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))
		    
		   

(defun disjoin (fn &rest fns)
  "引数に関数群を適用したときに生成される出力を集合論的にunion
on-lispにもfunとして定義があった"
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
	#'(lambda (&rest args)
	    (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  "引数に関数群を適用したときに生成される出力を集合論的にintersection
on-lispにもfinとして定義があった"
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
	#'(lambda (&rest args)
	    (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))


(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))

(defun always (x)
  #'(lambda (&rest args) x))

