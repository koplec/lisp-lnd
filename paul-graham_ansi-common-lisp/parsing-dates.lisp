(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c) (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str test p2)
		    nil)))
	nil)))

(defun constituent (c)
  (and (graphic-char-p c)		;graphic characters: all the characters we can see plus tehe space character
       (not (char= c #\  )))) ;;space 

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
	       (parse-month (second toks))
	       (parse-integer (third toks)))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun"
    "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
		     :test #'string-equal))) ;;string-equalはnot case-sensitive
    (if p
	(+ p 1)
	nil)))
