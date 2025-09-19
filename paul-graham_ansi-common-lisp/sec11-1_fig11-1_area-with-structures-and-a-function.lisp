;; fig 11.1 areas with structures and a function

(defstruct rectangle
  height width)

(defstruct circle radius)

(defun area (x)
  (cond ((rectangle-p x)
	 (* (rectangle-width x) (rectangle-height x)))
	((circle-p x)
	 (* pi (expt (circle-radius x) 2)))))
