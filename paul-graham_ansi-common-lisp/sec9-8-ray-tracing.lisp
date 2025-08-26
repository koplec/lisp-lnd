;; fig9.2 Math utilities

(defun sq(x)
  (* x x))

(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))

;; :conc-name は自動生成されるアクセサ関数名の接頭辞を決めるオプション。
;;   例: (defstruct (point (:conc-name p-)) x y z)
;;       → アクセサは p-x, p-y, p-z となる。
;;   デフォルト (:conc-name point-) だと point-x などになる。
;;   (:conc-name nil) とすると x, y, z という汎用的な名前になるが、
;;   衝突しやすいので注意。
(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

;; 1次、2次方程式の解
(defun minroot (a b c)
   "1次、2次方程式の解 最も小さいものを返す"
  (if (zerop a)
      (/ (- c) b)
      (let ((disc (- (sq b) (* 4 a c))));;判別式
	(unless (minusp disc) ;;複素数まで範囲広げないのね、こういうとき明示的にerrorが書きたいような気がするけど、レイ・トレーシングならどうでもいいのかも
	  (let ((discrt (sqrt disc)))
	    ;;一つの解だけ求める
	    (min (/ (+ (- b) discrt) (* 2 a))
		 (/ (- (- b) discrt) (* 2 a))))))))
      
;; multiple-value-call の説明
;; -----------------------------------------------
;; Common Lisp の関数は複数値を返せる。
;; (unit-vector ...) は3つの値 (x y z) を返す。
;; multiple-value-call は、それら複数値を
;; まとめて別の関数に渡す特別フォーム。
;;
;; 例: (multiple-value-call #'mag (unit-vector 23 12 47))
;;     → unit-vector の3値を mag の3引数に渡す
;;     → 結果は 1.0 （正規化ベクトルの長さ）
;;
;; 他の例: (multiple-value-call #'list (values 1 2) (values 3) (values 4 5))
;;     → (list 1 2 3 4 5)
;;(multiple-value-call #'mag (unit-vector 23 12 47)) -> 1.0

;; fig9.3 ray-tracing
(defstruct surface
  "the objects in the simulated world"
  color ;;ranging from 0(black) to 1(white)
  )

(defparameter *world* nil)
(defconstant eye
  ;eye exists above the plane and is positiond on the z-axis, 200units from the origin
  (make-point :x 0 :y 0 :z 200))

(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    ;; PGMファイルというのは、P2というタグがあるらしい
    ;; その後の数字でbreadth(100)とwidth(100)を指定している
    (format p "P2 ~A ~A 255" (* res 100) (* res 100)) ;;pって何？
    (let ((inc (/ res)))
      ;;100x100のimageに乗せること前提
      (do ((y -50 (+ y inc)))
	  ((< (- 50 y) inc)) ;; 50-y > incのあいだ続ける
	(do ((x -50 (+ x inc)))
	    ((< (- 50 x) inc))
	  (print (color-at x y) p))))))

(defun color-at (x y)
  "eyeからスクリーン上のxyを見たときの色"
  (multiple-value-bind (xr yr zr)
      (unit-vector (- x (x eye))
		   (- y (y eye))
		   (- 0 (z eye))) ;;目からスクリーンx,yへの単位ベクトル
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  ;;目から(xr yr zr)で表される単位ベクトル方向に向けたときのレイ・トレーシング計算
  (multiple-value-bind (surface int) (first-hit pt xr yr zr);;
		       (if surface
			   (* (lambert surface int xr yr zr) (surface-color surface))
			   0 ;;background color black (if surface is not hit)
			   )))

(defun first-hit (pt xr yr zr)
  "ptからxr yr zrで現れる単位ベクトル方向に向けたときに最初に当たるオブジェクトを求める"
  (let (surface hit dist)
    (dolist (s *world*)
      (let ((h (intersect s pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface s
		    hit h
		    dist d))))))
    (values surface hit)))

(defun lambert (surface int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal surface int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

    
	   
;; fig9.5 Spheres
(defstruct (sphere (:include surface)) ;;:includeとするとすでに定義されている構造体を含められるので、ここではcolorを含める
  radius center)

(defun defsphere (x y z r c)
  (let ((s (make-sphere
	    :radius r
	    :center (make-point :x x :y y :z z)
	    :color c)))
    (push s *world*)
    s))


;; ※typecase
;; 引数の型でちぇっく
;; sがsphere型なら関数オブジェクト #'sphere-intersectを返す
;; objectのtypeによりintersectを適用できるようにするためにoverload的なことをしている
(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
	   s pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
	;;surface上のベクトルを求める
	(make-point :x (+ (x pt) (* n xr))
		    :y (+ (y pt) (* n yr))
		    :z (+ (z pt) (* n zr))))))


(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
	   s pt))

(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))
			    

;; fig9.6 Using the ray-tracer
(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -1500 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
      ((> x 2))
    (do ((z 2 (1+ z)))
	((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))
