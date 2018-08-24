(defpackage "DAY/25"
  (:use "CL"))
(in-package "DAY/25")

;; row 2981, column 3075.

(defparameter *starting-code* 20151125)

(defun next-number (n)
  (rem (* n 252533) 33554393))

(defun next-position (row column)
  (values (1- row) (1+ column)))

(defun code-at (target-row target-column)
  (let ((max-row 1)
        (row 1)
        (column 1)
        (code *starting-code*))
    (tagbody
     step
       (multiple-value-setq (row column) (next-position row column))
       (when (zerop row)
         (setf column 1
               max-row (1+ max-row)
               row max-row))
       (setf code (next-number code))
     
     test
       (if (and (eql row target-row)
                (eql column target-column))
           (return-from code-at code)
           (go step)))))

(code-at 6 6)
(code-at 2981 3075)

(next-number (next-number 20151125))
(next-number (next-number 33511524))
