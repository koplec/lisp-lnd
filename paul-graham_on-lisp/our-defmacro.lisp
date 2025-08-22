;; fig. 7.6

(defmacro our-expander (name)
  `(get ,name 'expander))
;;getってなんだろう？？？

(defmacro our-defmacro (name params &body body)
  (let ((g (gensym)))
    `(progn
       (setf (our-expander ',name) ;; 'とquoteをいれて評価させない、ただのquoteつきの名前にする
	     #'(lambda (,g)
		 (block ,name
		   (destructuring-bind ,params (cdr ,g)
		     ,@body))))
       ',name)))

(defun our-macroexpand-1 (expr)
  (if (and (consp expr) (our-expander (car expr)))
      (funcall (our-expander (car expr)) expr)
      expr))
	     

