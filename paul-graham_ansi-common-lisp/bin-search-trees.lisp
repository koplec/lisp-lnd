;; 4.7 Example: Binary Search Trees

(defstruct (node (:print-function
		  (lambda (n s d)
		    (format s "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
	(if (eql obj elt) ;;eqlもどこかから与えるようにしたいかも、例えばbst-eqlみたいなものを作りたいかも
	    bst
	    (if (funcall < obj elt) ;; (<じゃないことに注意
		(make-node
		 :elt elt
		 :l (bst-insert obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elt elt
		 :l (node-l bst)
		 :r (bst-insert obj (node-r bst) <)))))))

(defun bst-find (obj bst <) ;; ここに<があるのは、仕方がないと思うかどうか nodeの中に埋め込みたい気持ちもある
  (if (null bst)
      nil
      (let ((elt (node-elt bst))) ;; ここから下の構造は、うえのbst-insertと似ているよね
	(if (eql obj elt)
	    bst
	    (if (funcall < obj elt)
		(bst-find obj (node-l bst) < )
		(bst-find obj (node-r bst) <))))))

(defun bst-min (bst)
  (and bst
       (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
       (or (bst-max (node-r bst)) bst)))
		
	
(defun bst-remove (obj bst <)
  (if (null bst)
      nil
      (let ((elt (node-elt bst)))
	(if (eql obj elt)
	    (percolate bst)
	    (if (funcall < obj elt)
		(make-node
		 :elt elt
		 :l (bst-remove obj (node-l bst) <)
		 :r (node-r bst))
		(make-node
		 :elt elt
		 :l (node-l bst)
		 :r (bst-remove obj (node-r bst) <)))))))

(defun percolate (bst)
  "bstの再配備 頭は要素としてなくなる"
  (cond ((null (node-l bst))
	 (if (null (node-r bst))
	     nil
	     (rperc bst)))
	((null (node-r bst)) (lperc bst))
	(t (if (zerop (random 2))
	       (lperc bst)
	       (rperc bst)))))

(defun rperc (bst)
  "bstの右側の要素を頭に持ってきて、percolate続ける"
  (make-node :elt (node-elt (node-r bst))
	     :l (node-l bst)
	     :r (percolate (node-r bst))))

(defun lperc (bst)
  "bstの左側の要素を頭に持ってきて、percolate続ける"
  (make-node :elt (node-elt (node-l bst))
	     :l (percolate (node-l bst))
	     :r (node-r bst)))
	     

(defun bst-traverse (fn bst)
  (when bst
    (bst-traverse fn (node-l bst))
    (funcall fn (node-elt bst))
    (bst-traverse fn (node-r bst))))
