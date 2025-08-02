(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)));; memoizeした関数の値を保管しておく
    #'(lambda (&rest args)
	(multiple-value-bind (val found) (gethash args cache) ;;hashのなかを確認する
	  ;; このbodyの役割知りたい
	  (if found 
	      val
	      ;;値がなかったらその時計算する
	      (setf (gethash args cache) (apply fn args)))))))
		    
	  
			  
