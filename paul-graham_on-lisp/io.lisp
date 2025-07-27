;; 4.6 入出力
;; 図4.7

(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			(apply #'read-line args)
			")"))))


(defun prompt (&rest args)
  (apply #'format *query-io* args) ;;query-io: common lispでのユーザとの対話的なやり取りのストリーム
  (read *query-io*) ;;readでユーザ入力をLispオブジェクトとして読み取るので、:qといれたらシンボルとして評価される
  )

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop. ~%")
  (loop
	(let ((in (apply #'prompt args)))
	  (if (funcall quit in)
	      (return)
	      (format *query-io* "~A~%" (funcall fn in))))))
		     
