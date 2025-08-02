;; Paul Graham On Lisp
;; 4.8 シンボルと文字列
;; 図4.8 シンボルと文字列に作用する関数

;; そもそもシンボルって何？
;; 名前のついたatom


(defun mkstr (&rest args)
  "引数を人間向けプリントで連結した文字列を作成する"
  (with-output-to-string (s) ;; s: この場限りで作られる文字列ストリーム 処理が終わったら返される
    (dolist (a args)
      (princ a s))))
;; ctrl-x ctrl-eでslimeで評価される


(defun symb (&rest args)
  "引数を連結した名前のシンボルを*package*にinternして返す" ;;
  (values ;;わざわざ多値で返すのどういうこと？
   (intern (apply #'mkstr args)) 	;intern シンボル生成関数 現在の*package*で既存シンボルを探してなければ作る そもそもinternは、収容するという意味
   ))

(defun reread (&rest args) ;;よめるコード片を与える
   "文字列とデータを往復"
  ;; 
  (values
   (read-from-string (apply #'mkstr args)) ;;read-from-stringは、信頼できるコードだけを読ませるべき
   ))


(defun explode (sym)
  "シンボル名を文字ごとに分解し、各文字を intern したシンボルのリストにする。"
  (map
   'list ;; 結果シーケンスタイプの指定 listで返すってこと
   #'(lambda (c)
       (intern
	(make-string 1 :initial-element c)))
   (symbol-name sym) ;; シンボルを文字列に直している
   ))
