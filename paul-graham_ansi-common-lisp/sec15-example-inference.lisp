;;fig 15.1 Matching function
(defun match (x y &optional binds)
  ;; (match '(child ?y ?x) '(child ?x ?y) '((?y . a) (?x . b))) -> NIL
  ;;;; 与えた束縛が違う値なのにmatchさせるx, yが等しいとなっているのでNILを返す
  ;; (match '?v '?w  '((?v . a) (?w . b))) -> NIL
  ;;;; 与えた束縛で?v ?wの値が等しくないのに等しいようにしようとしているのでNIL
  ;;;; (binding '?v binds) -> 'a
  (cond
    ((eql x y) (values binds t))
    ((assoc x binds) ;;束縛の中でxに対応した値を見つけたら
     ;; bindsは、association list, a-list ((a . b) (c . d) (e . f))のような
     ;;xと等しいkeyを探してそのconsを返す
     (match
	 (binding x binds) y;;xをbindsの値と入れ替えたうえで、yとの対応を見る
       binds))
    ((assoc y binds)
     (match x (binding y binds) binds))
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    (t
     (when (and (consp x) (consp y))
       (multiple-value-bind (b2 yes);;b2:matchした結果を一旦収納して新しいbindsになる
	   (match (car x) (car y) binds)
	 (and yes (match (cdr x) (cdr y) b2)))))))

(defun var? (x)
  ;; ?aのように先頭が?で始まるsymbolをvarとする
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  "xをbinds内で評価する"
  ;; (binding '?x '((?x . 1))) -> 1
  ;; (binding '?x '((?x . ?y) (?y . 2))) -> 2
  ;; (binding '?x '((?x . ?y ) (?y . 3) (3 . 4))) -> 4 ;;あんまり考えたくない
  ;; bindsはずっと保持されるので、循環参照は危ない
  ;; (binding '?x '((?x . ?y) (?y . ?x))) -> loop 止まらない。。。
  (let ((b (assoc x binds)))
    (if b
	(or (binding (cdr b) binds) ;;ここ難しいね
	    (cdr b)))))


    
;; fig15.2 Defining rules.
(defvar *rules* (make-hash-table))
;; key:symbol, value:list

(defmacro <- (con &optional ant)
  ;; conの先頭がruleの名称で、cdrがruleの内容
  ;; 例えば、(<- (parent donald nancy))としたら、
  ;; hash-tableのkeyとして、parentがきて、そこに
  ;; (donald nancy)が値としてpushされる
  ;; keyのことを、predicateとよんでいる
  `(length (push (cons (cdr ',con) ',ant)
		 (gethash (car ',con) *rules*))))

;; fig15.3 Inferences
(defun prove (expr &optional binds)
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t (prove-simple (car expr) (cdr expr) binds))))

(defun prove-simple (pred args binds)
  (mapcan #'(lambda (r)
	      (multiple-value-bind (b2 yes) (match args (car r) binds)
		(when yes
		  (if (cdr r)
		      (prove (cdr r) b2)
		      (list b2)))))
	  (mapcar #'change-vars
		  (gethash pred *rules*))))

(defun change-vars (r)
  ;;変数をかぶらないものに変換する
  (sublis
   (mapcar #'(lambda (v) (cons v (gensym "?"))) (vars-in r)) ;;'((?a . #001) (?b . #002)..)のような変数を一時変数に変換するためにリスト
	  r))

(defun vars-in (expr)
  ;;exprの中のvarを抽出してlistでまとめる
  (if (atom expr)
      (if (var? expr) (list expr))
      (union (vars-in (car expr))
	     (vars-in (cdr expr)))))
	      
