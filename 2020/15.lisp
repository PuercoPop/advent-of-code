(defpackage #:day/15
  (:use #:cl))
(in-package #:day/15)

(defparameter +input+ '(0 13 1 16 6 17))

(defun solve (nth &rest initial-numbers)
  (let ((memory (make-hash-table)))
    (loop :for num :in initial-numbers
          :for ix :from 1
          :do (setf (gethash num memory) ix))
    (loop :with prev-number := (car (last initial-numbers))
          :for ix :from (length initial-numbers) :below nth ;; 2020
          :for prev-prev-position := (gethash prev-number memory 0)
          :do (setf (gethash prev-number memory) ix)
              (if (zerop prev-prev-position)
                  (setf prev-number 0)
                  (setf prev-number (- ix prev-prev-position)))
          :finally (return prev-number))))

;; 0 3 6 0 3 3 1 0 4 0
(solve 2020 0 3 6) ; => 436 (9 bits, #x1B4)
(solve 2020 1 3 2) ; => 1 (1 bit, #x1, #o1, #b1)
(solve 2020 2 1 3) ; => 10 (4 bits, #xA, #o12, #b1010)
(solve 2020 1 2 3) ; => 27 (5 bits, #x1B, #o33, #b11011)
(solve 2020 2 3 1) ; => 78 (7 bits, #x4E, #o116, #b1001110)
(solve 2020 3 2 1) ; => 438 (9 bits, #x1B6)
(solve 2020 3 1 2) ; => 1836 (11 bits, #x72C)
(apply #'solve 2020 +input+) ; => 234 (8 bits, #xEA, #o352, #b11101010)

(apply #'solve 30000000 +input+) ; => 8984 (14 bits, #x2318)
