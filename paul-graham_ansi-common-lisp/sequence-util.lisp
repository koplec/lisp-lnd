(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (do ((forward 0 (+ forward 1))
	      (back (- len 1) (- back 1)))
	     ;;終了条件と返り値
	     ((or (> forward back)
		  (not (eql (elt s forward)
			    (elt s back)))) ;;途中の終了条件 中央を超えているか、 超えないときはforwardとbackでの要素が等しくないとき
	      (> forward back));;ループ終了時に返す 超えていればtrue超えていなければfalse
	   ))))


