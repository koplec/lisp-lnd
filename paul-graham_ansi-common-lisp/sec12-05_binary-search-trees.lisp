;; fig12.8 Binary search trees Destructive insertion

(defstruct (node (:print-function
		  (lambda (n s _)
		    (format s "#<~A>" (node-elt n)))))
  elt
  (l nil)
  (r nil))

(defun bst-insert! (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (progn
	(bsti obj bst <)
	bst)))

(defun bsti (obj bst <)
  (let ((elt (node-elt bst)))
    (if (eql obj elt)
	bst
	(if (funcall < obj elt)
	    (let ((l (node-l bst)))
	      (if l
		  (bsti obj l <)
		  (setf (node-l bst)
			(make-node :elt obj))))
	    (let ((r (node-r bst)))
	      (if r
		  (bsti obj r <)
		  (setf (node-r bst)
			(make-node :elt obj))))))))

;; fig 12.9 Binary search trees: Destructive deletion
(defun bst-delete (obj bst <)
  (if bst
      (bstd obj bst nil nil <))
  bst)

(defun bstd (obj bst prev dir <)
  (let ((elt (node-elt bst)))
    (if (eql elt obj)
	(let ((rest (percolate! bst)))
	  (case dir
	    (:l (setf (node-l prev) rest))
	    (:r (setf (node-r prev) rest))))
	(if (funcall < obj elt)
	    (if (node-l bst)
		(bstd obj (node-l bst) bst :l <))
	    (if (node-r bst)
		(bstd obj (node-r bst) bst :r <))))))

(defun percolate! (bst)
  ""
  (cond ((null (node-l bst))
	 (if (null (node-r bst))
	     nil
	     (rperc! bst)))
	((null (node-r bst))
	 (lperc! bst))
	;;lもrもnullでないとき
	(t (if (zerop (random 2))
	       (lperc! bst)
	       (rperc! bst)))))
	


(defun lperc! (bst)
  "bstの左側の要素を頭に持ってきて、左側をpercolate!する"
  (setf (node-elt bst)
	(node-elt (node-l bst)))
  (percolate! (node-l bst)))

(defun rperc! (bst)
  (setf (node-elt bst)
	(node-elt (node-r bst)))
  (percolate! (node-r bst)))
