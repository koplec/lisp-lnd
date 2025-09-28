(defpackage :pe014.test
  (:use :cl :kplb.test)
  (:import-from :pe014
   :collatz-length
   :memorized-collatz-length))


(in-package :pe014.test)

;; (pe014.test::collatz-length-test)で実行
;; ※kplb.tet必要

(deftest collatz-length-test
  (is (+ 1 2) 3)
  (is (collatz-length 2) 2)
  )

(deftest memorized-collatz-length-test
  (is (memorized-collatz-length 2) 2)
  (is (memorized-collatz-length 13) (collatz-length 13))
  )


