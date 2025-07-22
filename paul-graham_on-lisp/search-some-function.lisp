;; On Lisp Paul Graham
;; 4.4 検索

;; 図4.4より
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
	(if val
	    (values (car lst) val)
	    (find2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))


(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test))) ;;yはxよりも前にある
    (and rest (member x rest :test test)))) ;;beforeと違って、xが存在することを確認したい

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test)) :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do
     ((src lst (cdr src)));;初期値とloopの設定

     ;;終了処理
     ((or (null src) (funcall fn (car src)))
      (values (nreverse acc) src))
      ;;終了しないときの処理
      (push (car src) acc)
      )
    )
  )
