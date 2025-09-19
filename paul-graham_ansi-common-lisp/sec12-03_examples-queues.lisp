;; fig 12.7 Implementing queues

(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      ;;carに(obj)をいれて、さらにcdr->(obj)する
      ;;carは同じ構造を共有している
      (setf (cdr q)
	    (setf (car q) (list obj)))
      ;;cdrのcdrにあたらしいobjをセットする、すると、末尾にobjが追加されrう
      ;;cdr qにその末尾をセットする
      ;;
      (setf (cdr (cdr q)) (list obj)
	    (cdr q) (cdr (cdr q)))))

(defun dequeue (q)
  (pop (car q)))
