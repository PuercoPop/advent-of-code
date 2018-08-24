(defpackage "DAY/7"
  (:use "CL"
        "ALEXANDRIA"))
(in-package "DAY/7")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/day7-input.txt")

(defun split-ip (ip)
  (let ((rest ip)
        (supernet nil)
        (hypernet nil))
    (tagbody
     start
       (let ((opening-bracket (position #\[ rest :test 'char-equal)))
         (if opening-bracket
             (progn (push (subseq rest 0 opening-bracket)
                          supernet)
                    (setf rest (subseq rest (1+ opening-bracket))))
             (progn
               (push rest supernet)
               (go end))))
     hypernet
       (let ((closing-bracket (position #\] rest :test 'char-equal)))
         (if closing-bracket
             (progn (push (subseq rest 0 closing-bracket)
                          hypernet)
                    (setf rest (subseq rest (1+ closing-bracket))))
             (error "Impossible state"))
         (go start))
     end)
    (values supernet hypernet)))

(defun abbap (sequence)
  "An ABBA is any four-character sequence which consists of a pair of two
  different characters followed by the reverse of that pair."
  (loop :for index :from 0 :upto (- (length sequence) 4)
        :when (and (char-equal (aref sequence index)
                               (aref sequence (+ 3 index)))
                   (char-equal (aref sequence (1+ index))
                               (aref sequence (+ 2 index)))
                   (not (char-equal (aref sequence index)
                                    (aref sequence (1+ index)))))
          :do (return t)))

(defun tlsp (ip)
  (multiple-value-bind (supernet hypernet) (split-ip ip)
    (and (some 'abbap supernet) (notany 'abbap hypernet))))

(defun solve-1 (input)
  (with-open-file (in input)
    (loop :for line := (read-line in nil 'eof)
          :until (eq 'eof line)
          :when (tlsp line)
            :sum 1)))

(solve-1 +input+) ; => 118 (7 bits, #x76, #o166, #b1110110)

(abbap "ioxxoj")
(abbap "aaaa")
(assert (tlsp "abba[mnop]qrst"))
(assert (not (tlsp "abcd[bddb]xyyx")))
(assert (not (tlsp "aaaa[qwer]tyui")))
(assert (tlsp "ioxxoj[asdfgh]zxcvbn"))

(defun abap (string)
  (let (result)
    (loop :for index :from 0
          :until (eql (+ 2 index) (length string))
          :when (and (char-equal (aref string index)
                                 (aref string (+ 2 index)))
                     (not (char-equal (aref string index)
                                      (aref string (+ 1 index)))))
            :do (push (subseq string index (+ 3 index))
                      result))
    result))

(defun babp (hypernet aba)
  (let ((bab-code (format nil "~A~A~A" (aref aba 1) (aref aba 0) (aref aba 1))))
    (search bab-code hypernet)))

(defun corresponding-bab-code (hypernet-sequences aba-code)
  (some (lambda (y)
          (babp y aba-code))
        hypernet-sequences))

(defun sslp (ip)
  (multiple-value-bind (supernets hypernets) (split-ip ip)
    (some (lambda (x)
            (when-let (aba-codes (abap x))
              (some (lambda (aba-code)
                      (corresponding-bab-code hypernets aba-code))
                    aba-codes)))
          supernets)))

(defun solve-2 (input)
  (with-open-file (in input)
    (loop :for line := (read-line in nil 'eof)
          :until (eq 'eof line)
          :when (sslp line)
            :sum 1)))
(solve-2 +input+) ; => 260 (9 bits, #x104)

(assert (sslp "aba[bab]xyz"))
(assert (not (sslp "xyx[xyx]xyx")))
(assert (sslp "aaa[kek]eke"))
(assert (sslp "zazbz[bzb]cdb"))
