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
