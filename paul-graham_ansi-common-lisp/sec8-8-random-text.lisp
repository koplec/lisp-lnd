;; fig 8.2 Reading sample text
(defparameter *words* (make-hash-table :size 10000))
;; keyは単語で、valueはkeyに対応した単語の後に出現する単語のalist
;; こんなもの((|sin|  . 1) (|wide| . 2) (|sights| . 1))

(defconstant maxword 100)

(defun read-text (pathname)
  (with-open-file (s pathname :direction :input)
    (let ((buffer (make-string maxword)) ;;容量は100で、英単語としては問題ないでしょうと
	  (pos 0))
      (do ((c (read-char s nil :eof)
	      (read-char s nil :eof)))
	  ((eql c :eof))
	(if (or (alpha-char-p c) (char= c #\'))
	    (progn ;;単語形成のための文字収集
	      (setf (aref buffer pos) c)
	      (incf pos))
	    (progn ;; 
	      (unless (zerop pos)
		(see (intern (string-downcase
			      (subseq buffer 0 pos))));;単語を小文字にしてsymbol（例えば|lowerword|みたいな）にする
		(setf pos 0))
	      (let ((p (punc c)))
		(if p (see p)))))))))

(defun punc (c)
  "句読点をsymbolにする"
  (case c
    (#\. '|.|)
    (#\, '|,|)
    (#\; '|;|)
    (#\! '|!|)
    (#\? '|?|)))

(let ((prev '|.|)) ;;一つ前の単語を覚えておく
  (defun see (symb)
    "prev -> (next . count)のnextをsymbで更新する感じ"
    (let ((pair (assoc symb (gethash prev *words*))));;一つ前の単語から紐づくalistを調べてそこにsymbに対応したデータがあるか確認
      (if (null pair)
	  (push (cons symb 1) (gethash prev *words*))
	  (incf (cdr pair)))) ;;あったら、数値部分をincrement
    (setf prev symb)))

(defun generate-text (n &optional (prev '|.|))
  (if (zerop n)
      (terpri) ;;改行出力 TERminate PRInt line
      (let ((next (random-next prev)))
	(format t "~A " next)
	(generate-text (1- n) next))))

(defun random-next (prev)
  (let* ((choices (gethash prev *words*));;alistを撮ってきて
	 (i (random (reduce #'+ choices
			    :key #'cdr))));;数字部分を全部足してそれを最大値にして乱数を選ぶ
    (dolist (pair choices) ;;choicesをloop
      (if (minusp (decf i (cdr pair))) ;;上の数値から現在選んでいる単語の出現回数を引いてマイナスかどうかチェック ざつにどれかは選ばれる感じになっていそう
	  (return (car pair))))))

