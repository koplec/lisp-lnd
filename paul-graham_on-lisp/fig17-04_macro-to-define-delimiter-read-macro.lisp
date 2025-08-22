;; fig 17.4 デリミタリードマクロを定義するためのマクロ
(defmacro defdelim (left right parms &body body)
  `(ddfn ,left ,right #'(lambda ,parms ,@body))) ;; ,parmsは()で囲わなくているかたちを想定

(let ((rpar (get-macro-character #\)) )) ;;closureで右カッコを準備
  (defun ddfn (left right fn)
    (set-macro-character right rpar)
    (set-dispatch-macro-character #\# left ;;ここのleftが2番めの文字であることに注意
				  #'(lambda (stream ch1 ch2)
				      (apply fn
					     (read-delimited-list right stream t))))))

;; 再掲
(defun mapa-b (fn a b &optional (step 1))
  "引数aからはじまって、bに至る直前までfnを適用して、その結果の一覧をListで返す"
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))


;; example
(defdelim #\[ #\] (x y)
  (list 'quote (mapa-b #'identity (ceiling x) (ceiling y))))
				  

;; fig17.03 関数合成のためのリードマクロ
;; 今日 (2025-08-21)のじてんではfnを定義していないので、好ご期待！
