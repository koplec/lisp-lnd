;; Paul Graham ANSI Common Lisp
;; 3.15 Example: Shortest Path

(defvar *test-counter* 0)
(defvar *test-failures* 0)

(defmacro test-case (label condition)
  `(progn
     (incf *test-counter*)
     (unless ,condition
       (incf *test-failures*)
       (format t "~&[FAILED] ~A~%" ,label))
     (when ,condition
       (format t "~%[ OK ] ~A~%" ,label))))


;; network-example
;; (setf my-min '((a b c) (b c) (c d)))
;; minだと既に定義している名前と被る;; Paul Graham ANSI Common Lisp
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
    (test-case "aの隣接ノードbから(a)にbを追加した(b a)ができる"
	       (equal (new-paths '(a) 'a test-net)
		      '((b a))))
    (test-case "bには隣接ノードがないので結果はnil"
	       (equal (new-paths '(b a) 'b test-net)
		   nil))
    (test-case "存在しないノードに対しても結果はnil"
	       (equal (new-paths '(b a) 'x test-net)
		   nil)))


  (let ((test-net-2 '((a b) (b c))))
    (test-case "aの隣接ノードbから(a)にbを追加した(b a)ができる"
	       (equal (new-paths '(a) 'a test-net-2) '((b a))))
    (test-case "bの隣接ノードcから(b a)にcを追加した(c b a)ができる"
	       (equal (new-paths '(b a) 'b test-net-2) '((c b a))))
    (test-case "隣接ノードがないときは何もできない"
	       (equal
		(new-paths '(b a) 'x test-net-2)
		nil))
    (test-case "すでに(b a)があるときに、aの隣接ノードも追加できそう。。。"
	       (equal (new-paths '(b a) 'a test-net-2)
		      '((b b a))))
    )
  
  (format t "*** test-new-paths end ***")
  t)


(defun test-bfs ()
  (let ((test-net '((a b))))
    (test-case "a->a"
	       (equal (bfs 'a '((a)) test-net)
		      '(a)))
    (test-case "a->b"
	       (equal (bfs 'b '((a)) test-net)
		      '(a b)))
    )

  (let ((test-net '((a b) (b c))))
    (test-case "a->a"
	       (equal (bfs 'a '((a)) test-net)
		      '(a)))
    (test-case "a->b"
	       (equal (bfs 'b '((a)) test-net)
		      '(a b)))
    (test-case "a->b->c"
	       (equal (bfs 'c '((a)) test-net)
		      '(a b c)))

    )

  (let ((test-net '((a b) (b c d) (d e))))
    (test-case "a->b->c"
	       (equal (bfs 'c '((a)) test-net)
		      '(a b c)))
    (test-case "a->b->d"
	       (equal (bfs 'd '((a)) test-net)
		      '(a b d)))
    (test-case "a->b->d->e"
	       (equal (bfs 'e '((a)) test-net)
		      '(a b d e)))

    (test-case "b->d->e"
	       (equal (bfs 'e '((b)) test-net)
		      '(b d e)))
   
    )


  (format t "*** test-bfs end ***")
  t)

(defun test-shortest-path ()
  (let ((net '((a b c) (b c) (c d))))
    (test-case "a->c->d"
	       (equal '(a c d)
		      (shortest-path 'a 'd net)))
    (test-case "a->c"
	       (equal '(a c)
		      (shortest-path 'a 'c net)))
     )

  (format t "*** test-shortest-path end ***")
  t)

(test-new-paths)
(test-bfs)
(test-shortest-path)
