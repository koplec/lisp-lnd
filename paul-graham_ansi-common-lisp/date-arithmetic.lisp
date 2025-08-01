;; Paul Graham ANSI Common LISP
;; 5.7 Example: Date Arithmetic

(defconstant month
  #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
	   (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
  (+ (svref month (- m 1)) ;; 前月までの累積日数
     (if (and (> m 2) (leap? y)) 1 0))) ;;  うるう年だったら２月以降の累積日数が1増える

(defun year-num (y)
  "2000年以後の経過日数を計算"
  (let ((d 0))
    (if (>= y yzero)
	;; 2000年以後のときの処理
	(dotimes (i (- y yzero) d) 	; i=y-yzeroからスタート 最終的な返り値はdにする
	  (incf d (year-days (+ yzero i))))
	;; 2000年より前のときの処理
	(dotimes (i (- yzero y) (- d))
	  (incf d (year-days (+ y i)))))))

(defun year-days (y)
  (if (leap? y) 366 365))

;; fig5.2

(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))

(defun num-year (n)
  "数値が何年に当たるのかと年に換算できない日付の2つを返す"
  (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
	    (d (- (year-days y)) (- d (year-days y))))
	   ((<= d n);; "dがn以下になったらとは？nは負の数なので、累積してdが大きな負の値になったとき
	    (values y (- n d)))) ;;小さな負のnから、大きな負のdを引くので、正の値になる
      (do* ((y yzero (+ y 1)) ;; yを決める 1年ごと増えていく
	    (prev 0 d);;初期値0で、一つ前のdの値を見る
	    (d (year-days y) (+ d (year-days y)))) ;;初期値は、与えられたyの日数で、毎年の日数が追加していく
	   ((> d n) (values y (- n prev))))));;dはどんどん増えていって、あるときnを超えたら、その前の値を引いたのが残りの日数になる

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
	    ((> n 59) (nmon (- n 1)))
	    (t (nmon n)))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<))) ;;vectorであるmonthが小さい順に並んでいるからそこの何番目か
    (values m (+ 1 (- n (svref month (- m 1))))))) ;;monthとその前の月の日数を引く

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))
    
