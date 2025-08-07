;; 5.6 部分ツリーへの再帰

;; fig5.8 tree traverse
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		     base)
		 (funcall rec
			  (self (car tree))
			  ;;
			  ;; このifでcdr treeで対応しているところ単に(self (cdr tree))にしたい気もするけど、nilのときの対応方法でrecの記述がだるくなる？
			  ;;
			  (if (cdr tree) 
			      (self (cdr tree)))))))
    #'self))

;; fig 5.9
;; our-copy-tree
(funcall
 (ttrav #'cons)
 '((a (b c) d e) f)) ;;->((A (B C) D E) F)

;;count-leaves
(funcall
 (ttrav #'(lambda (l r) ;;l, rにはすでに計算した結果が入っていると考えるとわかりやすい
	    (+ l (or r 1)))
	1)
 '((a b (c d) (e) f))) ;;->10


;; flatten
(funcall
 (ttrav #'nconc #'mklist)
 '((a b (c d) (e) f))) ;;->(A B C D E F)


;; fig5.10
(defun trec (rec &optional (base #'identity))
  (labels
      ((self (tree)
	 (if (atom tree)
	     (if (functionp base)
		 (funcall base tree)
		 base)
	     (funcall rec
		      tree
		      #'(lambda ()
			  (self (car tree)))
		      #'(lambda ()
			  (if (cdr tree)
			      (self (cdr tree))))))))
    #'self))

;;flattenn
(funcall
 (trec #'(lambda (obj left right)
	   (nconc (funcall left) (funcall right)))
       #'mklist)
 '((a) b (c d) (e) f))

;;rfind-if
(funcall
 (trec #'(lambda (obj left right)
	   (or (funcall left) (funcall right)))
       #'(lambda (tree)
	   (and (oddp tree) tree)))
 '(2 (3 4) 5)) ;;->3
