(defpackage "AOC/3"
  (:use "CL"))
(in-package "AOC/3")

#|

  04  03  02 
  05  00  01
  06  07  08

        37 36 35 34 33 32 31

        38 17 16 15 14 13 30
                     
        39 18  5  4  3 12 29

        40 19  6  1  2 11 28

        41 20  7  8  9 10 27
 
        42 21 22 23 24 25 26

        43 44 45 46 47 48 49


Cada 'anillo' tiene cada vez más números.

1er: 2 - 9  (8)
2do: 10- 25 (16)
3er: 26-49  (24)
|#


(defparameter +input+ 325489)

(defun find-ring (num)
  (loop :for n :from 1
        :for lower-bound := 2 :then (1+ upper-bound)
        :for upper-bound := 9 :then (+ upper-bound (+ 8 ring-length))
        :for ring-length := (* 8 n)
        :when (<= lower-bound num upper-bound)
          :do (return (values n lower-bound upper-bound ring-length))))

(defparameter +sector+ '(:right-down :right-up :up-right :up-left :left-up :left-down :down-left :down-right ))

(multiple-value-bind (ring-number lower-bound upper-bound ring-length) (find-ring +input+)
  (loop
    :for n :from 1 :below 8
    :when (< (+ lower-bound (* n ring-number)) +input+ (+ lower-bound (* (1+ n) ring-number)))
      :return (values (elt +sector+ n) n)))

;; Next question is distance to one of the 'extremes of the cross'

(- 13 3) ; => 10 (4 bits, #xA, #o12, #b1010)
(- 3 1) ; => 2 (2 bits, #x2, #o2, #b10)
(- 9 2)
(- 49 26) ; => 23 (5 bits, #x17, #o27, #b10111)
