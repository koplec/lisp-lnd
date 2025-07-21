;; Paul Graham ANSI Common Lisp
;; 3.15 Example: Shortest Path

;; network-example
;; (setf my-min '((a b c) (b c) (c d)))
;; minだと既に定義している名前と被る

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end paths net)
  "Breadth-first search"
  (if (null paths)
      nil
      (let ((path (car paths))) ;;現状のpathsの先頭をpathとして取得
	(let ((node (car path)))
	  (if (eql node end) ;;とうとう、endに到達した。
	      (reverse path) ;;答えを渡して終わり
	      (bfs end
		   (append (cdr paths)
			   (new-paths path node net))
		   net))))))
	    
	    

(defun new-paths (path node net)
  "nodeの隣接点を探して、pathの手前に接続して、新しいpathの一覧を作成する"
  (mapcar #'(lambda (n)
	      (cons n path))    
	  (cdr (assoc node net));;nodeの隣接node cdrすることで先頭のnodeを取り除いている
	  ))


(defun test-new-paths ()
  (let ((test-net '((a b))))
    ;;aの隣接ノードbから(a)にbを追加した(b a)ができる
    (assert (equal (new-paths '(a) 'a test-net)
		   '((b a))))
    ;;bには隣接ノードがないので結果はnil
    (assert (equal (new-paths '(b a) 'b test-net)
		   nil))
    ;;存在しないノードに対しても結果はnil
    (assert (equal (new-paths '(b a) 'x test-net)
		   nil))
    )


  (let ((test-net-2 '((a b) (b c))))
    ;;aの隣接ノードbから(a)にbを追加した(b a)ができる
    (assert (equal (new-paths '(a) 'a test-net-2) '((b a))))
    ;;bの隣接ノードcから(b a)にcを追加した(c b a)ができる
    (assert (equal (new-paths '(b a) 'b test-net-2) '((c b a))))
    ;;隣接ノードがないときは何もできない
    (assert (equal
	     (new-paths '(b a) 'x test-net-2)
	     nil))
    )


  
  (format t "*** test-new-paths passed ***")
  t)

(test-new-paths)
