;; fig7.8

(defmacro our-do (bindforms ;; ((var1 init1 step1) (var2 init2 step2)...)
		  (test &rest result) ;;反復終了条件と結果値
		  &body body) ;;各反復で実行
  (let ((label (gensym)))
    `(prog ;;progの変数束縛を使う 内部的にlet
	 ,(make-initforms bindforms) ;; 変数束縛 ,があるので、マクロ展開時に評価されて埋め込まれるイメージ 展開時は関数は実行されるということ 実行時ではすでにmake-initformsが評価されたあと
	,label ;;,があるので(gensym)がletの時点で評価された結果が埋め込まれる
	(if ,test
	    (return (progn ,@result))) ;; 終了条件が満たされたとき
	,@body ;; @bodyがそのまま挿し込まれる
	(psetq ,@(make-stepforms bindforms))
	(go ,label))))


;; 初期値の設定 (変数名 初期値)のlistを生成
(defun make-initforms (bindforms)
  (mapcar #'(lambda (b)
	      (if (consp b)
		  (list (car b) (cadr b))
		  (list b nil)))
	  bindforms))

;; 値の更新 (変数名 変数の更新方法)のlistを生成
(defun make-stepforms (bindforms)
  (mapcar #'(lambda (b)
	      (if (and (consp b) (third b))
		  (list (car b) (third b))
		  nil))
	  bindforms))
