(defpackage "DAY/8"
  (:use "CL"
        "ALEXANDRIA"))
(in-package "DAY/8")

(defparameter +input+ #P"/home/puercopop/quicklisp/local-projects/playground/advent-of-code/2016/8.input")

(defparameter +full-row+ #b11111111111111111111111111111111111111111111111111)

(defparameter +grid+ (list 0 0 0 0 0 0))

(defmacro define-grid (x y)
  (with-gensyms (carry temp)
    `(progn
       (setf +full-row+ (ldb (byte ,x 0) most-positive-fixnum)
             +grid+ (make-list ,y :initial-element 0))
       (defun <<-1 (int)
         (let ((,carry (ldb (byte 1 (1- ,x)) int)))
           (ldb (byte ,x 0)
                (logior (ash int 1)
                        ,carry))))
       (defun >>-1 (int)
         (let ((,carry (ldb (byte 1 0) int)))
           (ldb (byte ,x 0)
                (logior (ash int -1)
                        (if (zerop ,carry)
                            0
                            (- +full-row+ (ash +full-row+ -1)))))))

       (defun select-column (x &key row)
         (ldb (byte 1 (- ,(1- x) x)) (elt +grid+ (mod row ,y))))

       (defun (setf select-column) (new x &key row)
         (setf (elt +grid+ (mod row ,y))
               (dpb new (byte 1 (- ,(1- x) x)) (elt +grid+ (mod row ,y)))))

       (defun select-row (y)
         (elt +grid+ (mod y ,y)))

       (defun (setf select-row) (new y)
         (setf (elt +grid+ (mod y ,y))
               new))

       (defun ^-1 (x)
         (let ((,temp (select-column x :row ,(1- y))))
           (dotimes (i ,y)
             (psetf (select-column x :row (- ,(1- y) (1+ i)))
                    ,temp
                    ,temp
                    (select-column x :row (- ,(1- y) (1+ i)))))))

       (defun v-1 (x)
         (let ((,temp (select-column x :row 0)))
           (dotimes (i ,y)
             (psetf (select-column x :row (1+ i))
                    ,temp
                    ,temp
                    (select-column x :row (1+ i)))))))))

(defun >> (int n)
  (loop :with next := int
        :repeat n
        :do (setf next (>>-1 next))
        :finally (return next)))

(defun << (int n)
  (loop :with next := int
        :repeat n
        :do (setf next (<<-1 next))
        :finally (return next)))

(defun ^ (x n)
  (loop :repeat n
        :do (^-1 x)))

(defun v (x n)
  (loop :repeat n
        :do (v-1 x)))

(defun rotate-row (row-number n)
  (setf (select-row row-number)
        (if (plusp n)
            (>> (select-row row-number) n)
            (<< (select-row row-number) (abs n)))))

(defun rotate-column (column-number n)
  (if (plusp n)
      (v column-number n)
      (^ column-number (abs n))))

(defun rect (x y)
  (dotimes (i y)
    (setf (select-row i) (logior (select-row i) (>> (1- (expt 2 x)) x)))))

(defun count-row (int)
  (loop :with row-length := (round (log +full-row+ 2))
        :for index :from 0 :upto (1- row-length)
        :when (logbitp index int)
          :sum 1))

(defun count-pixels (grid)
  (loop :for row :in grid
        :sum (count-row row)))

(defun print-row (int)
  (let ((row-length (round (log +full-row+ 2))))
    (dotimes (i row-length)
      (let ((char (if (logbitp (- row-length (1+ i)) int)
                      #\#
                      #\.)))
        (format t "~A" char)))
    (format t "~%")))

(defun print-grid (grid)
  (loop :for row :in grid
        :do (print-row row)))

(define-grid 7 3)

(rect 3 2)
(rotate-column 1 1)
(rotate-row 0 4)
(rotate-column 1 1)
;; Luego contar bits
(count-pixels +grid+)


(defun parse-rect-parameters (line)
  (let ((first-space (position #\Space line))
        (x (position #\x line)))
    (mapcar 'parse-integer (list (subseq line first-space x)
                                 (subseq line (1+ x))))))

(parse-rect-parameters "rect 19x1")

(defun parse-rotate-parameters (line)
  (let ((= (position #\= line))
        (b (position #\b line))
        (last-space (position #\Space line :from-end t)))
    (mapcar 'parse-integer (list (subseq line (1+ =) (1- b))
                                 (subseq line last-space)))))

(parse-rotate-parameters "rotate column x=20 by 3")
(defun do-command (line)
  (cond ((search "rect" line) (apply 'rect (parse-rect-parameters line)))
        ((search "rotate column" line) (apply 'rotate-column (parse-rotate-parameters line)))
        ((search "rotate row" line) (apply 'rotate-row (parse-rotate-parameters line)))
        (t (error "Command not found."))))
(parse-command "rect 19x1")
(parse-command "rotate column x=20 by 3")
(parse-command "rotate row y=4 by 15")

(define-grid 50 6)
(with-input-from-file (in +input+)
  (loop :for line := (read-line in nil 'eof)
        :until (eq line 'eof)
        :do (do-command line)))
(count-pixels +grid+)
(print-grid +grid+)
