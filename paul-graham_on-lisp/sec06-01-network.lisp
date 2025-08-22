;; fig6.2
(defstruct node contents yes no)
(defvar *nodes* (make-hash-table))

(defun defnode (name contents &optional yes no)
  (setf (gethash name *nodes*)
	(make-node :contents contents
		   :yes yes
		   :no no)))


;; fig 6.3 network example
(defnode 'people
  "Is the person a man?"
  'male
  'female)

(defnode 'male
  "Is he living?"
  'liveman
  'deadman)

(defnode 'deadman
  "Was he American?"
  'us 'them)

(defnode 'us
  "Is he on a coin?"
  'coin 'cidence)

(defnode 'coin
  "Is the coin a penny?"
  'penny 'coins)

(defnode 'penny
  'lincoln)

;; fig6.4
(defun run-node (name)
  (let ((n (gethash name *nodes*)))
    (cond ((node-yes n) ;;node-yesがあるときの処理
	   (format t "~A~%>> " (node-contents n))
	   (case (read)
	     (yes (run-node (node-yes n)))
	     (t (run-node (node-no n)))))
	  ;; node-yesがないときにはcontentsを表示
	  (t (node-contents n)))))
	   
	     
