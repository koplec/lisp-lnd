;; 4.5 mapping

;; 図4.6 map系関数

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  "引数aからはじまって、bに至る直前までfnを適用して、その結果の一覧をListで返す"
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))



(defun our-mapcan (fn &rest lsts)
  (apply #'nconc (apply #'mapcar fn lsts)))

(defun mappend (fn &rest lsts)
  "非破壊的なmapcan"
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  "複数のlist（listのlistではない）に対してmapcarを行いたいとき"
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result))) ;;結果はflattenする
    (nreverse result)))

(defun rmapcar (fn &rest args)
  "recursive mapcar mapcarが単層リストに対して行う操作をツリーに対して行う"
  (if (some #'atom args) ;;ここsomeだと、引数がatomであるものとないものがあるけど大丈夫？ すべてatomかlistかしかこの関数は動作を想定していない？
      (apply fn args)
      (apply #'mapcar
	     #'(lambda (&rest args)
		 (apply #'rmapcar fn args))
	     args)))
