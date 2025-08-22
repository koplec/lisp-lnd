;; fig 7.2 Operations on ring buffers

;; ring bufferの実装
;; ring bufferに書き込むwriterと読み込むreaderの視点が大切
;; writerは、startとendを管理してどこから書き始めて、どこまで書き終わったかを管理
;; readerは、usedとnewを管理して、どこまで読んだか、どこまで読み勧めてよいかを管理
(defstruct buf
  vec ;;
  ;; start <= used <= new <= end
  (start -1) ;;point to first value, popしたらincrement 
  ;; used, endは現在マッチしている文字列のstart endのようなもの
  ;; used-newはwindowのようなもの
  (used -1) ;; すでにbufferから読んだ位置
  (new -1)  ;; ここまで読んで良いと定義した位置
  (end -1) ;;point to end value, 新しい値をinsertしたらincrement
  )

(defun bref (buf n)
  "buffer内のインデックスnの値を参照する accessor"
  (svref (buf-vec buf)
	 (mod n (length (buf-vec buf))) ;;インデックスはlengthの長さで折り返す
	 ))

(defun (setf bref) (val buf n)
  ;; common lispの(setf <name>)定番の定義
  ;; setfの右辺がval
  ;; 左辺の引数 (bref buf n)の中身が(buf n)に渡る？
  ;; 例えば、(setf (bref buf n) val)のように使って、valが(bref buf n)で参照できるようになるという意味
  "brefの場所に値を入れる"
  (setf
   (svref (buf-vec buf)
	       (mod n (length (buf-vec buf))))
	val))

(defun new-buf (len)
  (make-buf :vec (make-array len)))

(defun buf-insert (x b)
  "bufringの後ろに値を詰める writerの視点でendがのびていくだけ"
  (setf (bref b
	      (incf (buf-end b)) ;;incrementされる副作用, incf自体はそのincrement舌値を返す
	      )
	x))

(defun buf-pop (b)
  "bufの先頭を取り出して、startをincrement 先頭を取り出すので、データのスタート地点はインクリメントされ、さらにreaderの視点でどこまで読んだか先に進む"
  (prog1
      (bref b
	    (incf (buf-start b)))
    ;; popしたときにusedとnewの値がresetされる。
    ;; じゃあここでbuf-reset呼ぶとかあっても良いかなと思った
    (setf (buf-used b) (buf-start b)
	  ;;popした後のstart位置をusedに保管、読み出した次の値を保管
	  (buf-new b) (buf-end b)
	  ;;popしたときend位置は変わらなさそうだけど、読み終わりの場所を決めておく
	  )))

(defun buf-next (b)
  "usedの次の値を見に行く このときusedの値もincrementする buf-popまたはbuf-resetしたら使えるようになる"
  (when (< (buf-used b) (buf-new b))
    (bref b (incf (buf-used b)))))

(defun buf-reset (b)
  "buf-popの最後と同じ作用だけど、わざわざ別関数にしているのはなぜ？"
  (setf (buf-used b) (buf-start b)
	(buf-new b) (buf-end b)))

(defun buf-clear (b)
  (setf (buf-start b) -1
	(buf-used b) -1
	(buf-new b) -1
	(buf-end b) -1))

(defun buf-flush (b str)
  "usedからendまでstreamにbufferの値を流す"
  (do ((i (1+ (buf-used b)) (1+ i)))
      ((> i (buf-end b)))
    (princ (bref b i) str)))
       
	  
    
;;補助関数を追加
(defun buf-empty-p (b)
  (= (buf-end b) (buf-start b)))

(defun buf-count (b)
  "論理的な要素数を数える"
  (- (buf-end b) (buf-start b)))

(defun buf-peek-next (b)
  "現在の次の値を読むだけ"
  (let ((i (1+ (buf-used b))))
    (when (<= i (buf-new b)) ;;used, endは論理的なインデックスだから変に超えたりしないはず
      (bref b i))))

(defun buf-peek-front (b)
  "先頭要素をpopせずに読む"
  (let ((i (1+ (buf-start b))))
    (when (<= i (buf-end b))
      (bref b i))))

;; fig 7.2 String substition
(defun file-subst (old new file1 file2)
  "oldで与えられたstringをnewで書き換える"
  (with-open-file (in file1 :direction :input)
    (with-open-file (out file2 :direction :output
			       :if-exists :supersede) ;;もし存在していたら上書き保存
      (stream-subst old new in out))))

(defun stream-subst (old new in out)
  (let* ((pos 0) ;; oldのどこまで一致しているか
	 (len (length old)) ;; 
	 (buf (new-buf len)) ;; 一時保留の文字置き場
	 (from-buf nil)) ;; 今回のループで読んだcがbuffer由来かどうか doで初期化したほうが良くない？
    (do ((c (read-char in nil :eof) ;;cの初期値, inから値を取る
	    (or (setf from-buf (buf-next buf))
		;; 先に保留分を消化 bufferの次の値を取得する (buf-next buf)がnilのとき
		;; buf-nextが取得されるのは、定義よりbuf-used < buf-newのとき
		;; 
		(read-char in nil :eof)
		;;cの次のiteration 保留分がないとき先に進む
	    )
	    ))
	((eql c :eof)) ;;eolで終了
      (cond ((char= c (char old pos))
	     ;; cとoldの文字が等しい matchしたときのことを考える
	     (incf pos)
	     (cond
	       ;; 3 complete-match
	       ((= pos len) 
		(princ new out) ;; 置換文字列を確定出力
		(setf pos 0) ;;出力済みなのでクリア
		(buf-clear buf)) ;;終わったのでclearしておく、保留は不要 -> ここでfrom-bufがnilになるので、read-char inが走る
	       ;; 2 match begin
	       ;; from-bufがnilのときここで溜め込む
	       ;; すなわち、matchはじめてpopするまではずっと
	       ((not from-buf) ;; inから入力されているものは、溜め込む
		;; たぶんまだbuf-popしていなくて文字を溜め込む段階 -> from-bufがnil
		(buf-insert c buf))))
	    
	    ;; 以下は、matchしなかったとき
	    ((zerop pos) ;; 1 1文字もまだmatchしていない
	     (princ c out) ;;読み込んだcをそのまま出力
	     (when from-buf ;; matchしなかったので、from-bufがあるときはなくさないといけない
	       (buf-pop buf) ;;ここbuf-resetだけにしたいかも
	       (buf-reset buf)))
	    (t ;; 4 今まで1文字以上matchしていた、今後もマッチする可能性があるのに、置換されないことにならないの？
	     (unless from-buf ;;from-bufがnilのときの処理
	       (buf-insert c buf)) ;;今回のcは保留に積むだけで、この周回では出さない
	     (princ (buf-pop buf) out) ;;古い1文字だけ確定出力
	     (buf-reset buf)
	     (setf pos 0))))
    (buf-flush buf out))) ;; EOFでループを抜けても保留に残っている確定出力して良い文字列を出力する
	     
	      
