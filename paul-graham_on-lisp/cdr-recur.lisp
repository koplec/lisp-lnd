;; 5.5 cdr部での再帰

(defun our-length (lst)
  (if (null lst)
      0
      (1+
       (our-length (cdr lst)))))

(defun our-every (fn lst)
  (if (null lst)
      t
      (and (funcall fn (car lst))
	   (our-every fn (cdr lst)))))

(defun lrec (
	     rec ;;recursiveに適用する関数 引数に(car lst)と 残りの処理を行う(self (cdr lst))が入る
	     &optional base
	       )
  (labels ((self (lst) ;;再帰本体 listを処理する
	     (if (null lst)
		 (if (functionp base)
		     (funcall base)
		     base)
		 (funcall rec (car lst)
			  #'(lambda ()
			      ;;このthunkが(cdr lst)部の計算を遅延させている
			      (self (cdr lst))) ;;recの再帰呼出し 内部でselfを高階関数として呼び出す lambdaだから処理されないので、結果が入らなのが、our-everyやour-lengthと形が違っている気がする。 これは処理を遅延している 必要なときまで呼ばないというのは自分自身の哲学にしてみたいなと思った
			  ;(self (cdr lst)) ;;こっちじゃだめかはよくわからないけど、recはcdrを処理したもの（処理が終わったもの）とcarを処理するイメージを持つ必要がある
			  ))))
    #'self))

;;our-length
(funcall
 (lrec
  #'(lambda (x thunk-rest) (1+ (funcall thunk-rest))
  0
  )
 '(1 2 3))

;;our-every-evenp
(funcall
 (lrec
  #'(lambda (x fn) (and (evenp x) (funcall fn)))
  t
  )
 '(2 4 6))

;; copy-list
(funcall
 (lrec
  #'(lambda (x fn) (cons x (funcall fn)))
  nil)
 '(a b c))

;; remove-duplicates
(funcall
 (lrec
  #'(lambda (x fn) (adjoin x (funcall fn))) ;;adjoin 重複する要素は追加しない
  nil)
 '(a b b c))

(adjoin 'a '(a b c)) ;; -> (A B C)

;;find-if for example evenp
(funcall
 (lrec
  #'(lambda (x fn) (if (evenp x) x
		       (funcall fn)))
  nil)
 '(1 2 3 4))

;; some, for example oddp
(funcall
 (lrec
  #'(lambda (x fn) (or (oddp x)
		       (funcall fn)))
  nil)
 '(1 2 3 4))

