;; fig 13.4 Harbor

(defparameter *harbor* nil)

(defstruct ship
  name flag tons)

;(defun enter (n f d)
;  (push (make-ship :name n :flag f :tons d) *harbor*))

;(defun find-ship (name)
;  (find name *harbor* :key #'ship-name :test #'equal))

;(defun leave (name)
;  (setf *harbor*
;	(delete (find-ship name) *harbor*))

;; fig 13.5 Harbor ver2
(defparameter pool (make-array 1000 :fill-pointer t))
;;fill-pointer tで最後をさすようになる。最初間違えて1としていたので、なぜか長さが1のままだった

;;runtimeのときにmake-shipで1000個のshipをつくる
(dotimes (i 1000)
  (setf (aref pool i) (make-ship)))

;;poolから取り出したshipにアクセスするためのhash-table
(defparameter harbor (make-hash-table :size 1100
				      :test #'eq))



(defun enter (n f d)
  (let ((s (if (plusp (length pool))
	       ;;poolから必要なものを取り出す
	       ;;poolからなくなる
	       (vector-pop pool)
	       (make-ship))))
    (setf (ship-name s) n
	  (ship-flag s) f
	  (ship-tons s) d
	  (gethash n harbor) s)))

(defun find-ship (n)
  (gethash n harbor))

(defun leave (n)
  (let ((s (gethash n harbor)))
    (remhash n harbor)
    (vector-push s pool)))
