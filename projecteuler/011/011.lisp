(defun split-by-space-to-integers (line)
  "空白で分割して整数のリストを返す"
  (let ((stream (make-string-input-stream line)))
    (loop for token = (read ;;空白文字ごとに読む
		       stream
		       nil ;; EOFになったら第3引数を返す
		       nil ;; 第3引数
		       )
	  while token
	  collect (parse-integer (princ-to-string token)))))

(defun list->2d-array (lst)
  (let ((rows (length lst))
	(cols (length (car lst))))
    (make-array (list rows cols)
		:initial-contents lst)))

(defun read-grid->list  (filepath)
  "filepathで与えられたファイルから数値を読み取って20x20の2次元行列を作る"
  (with-open-file (in filepath
		      :direction :input)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (split-by-space-to-integers line))))
    
(defun read-grid->2d-array (filepath)
  (list->2d-array
   (read-grid->list filepath)))

(defun search-max-value (ary)
  (let ((max -1)
	(right-score -1)
	(down-score -1)
	(diag-score-1 -1)
	(diag-score-2 -1)
	(row (array-dimension ary 0))
	(col (array-dimension ary 1)))
    (format t "row:~A col:~A~%" row col)
    (dotimes (i row)
      (dotimes (j col)
	(progn
	  (format t "i:~A j:~A-> ~A ~%" i j (aref ary i j))
	  (format t "max:~A~%" max)
	  (when (< (+ i 3) row)
	    (setf down-score (* (aref ary i j) (aref ary (1+ i) j) (aref ary (+ i 2) j) (aref ary (+ i 3) j)))
	    (format t "down-score:~A~%" down-score)
	    (if (> down-score max) (setf max down-score)))
	  
	  (when (< (+ j 3) col)
	    (setf right-score (* (aref ary i j) (aref ary i (1+ j)) (aref ary i (+ j 2)) (aref ary i (+ j 3))))
	    (format t "right-score:~A~%" right-score)
	    (if (> right-score max) (setf max right-score)))

	  (when (and (< (+ i 3) row) (< (+ j 3) col))
	    (setf diag-score-1 (* (aref ary i j) (aref ary (1+ i) (1+ j)) (aref ary (+ i 2) (+ j 2)) (aref ary (+ i 3) (+ j 3))))
	    (format t "diag-score-1:~A~%" diag-score-1)
	    (if (> diag-score-1 max) (setf max diag-score-1))
	    )
	  
	  (when (and (< (+ i 3) row) (>= (- j 3) 0))
	    (setf diag-score-2 (* (aref ary i j) (aref ary (+ i 1) (- j 1))
				  (aref ary (+ i 2) (- j 2))
				  (aref ary (+ i 3) (- j 3))))
	    (format t "diag-score-2:~A~%" diag-score-2)
	    (if (> diag-score-2 max) (setf max diag-score-2))
	    )
	  )))
    max))
     
;; ans -> 70600674
