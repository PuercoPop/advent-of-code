(defpackage "DAY/15"
  (:use "CL"))
(in-package "DAY/15")

(loop :for i :from 0
      :when (every 'zerop (list (mod (+ i 15 1) 17)
                                (mod (+ i 2  1 1) 3)
                                (mod (+ i 4  1 1 1) 19)
                                (mod (+ i 2  1 1 1 1) 13)
                                (mod (+ i 2  1 1 1 1 1) 7)
                                (mod (+ i 0  1 1 1 1 1 1) 5)))
        :do (return i))

(loop :for i :from 0
      :when (every 'zerop (list (mod (+ i 15 1) 17)
                                (mod (+ i 2  1 1) 3)
                                (mod (+ i 4  1 1 1) 19)
                                (mod (+ i 2  1 1 1 1) 13)
                                (mod (+ i 2  1 1 1 1 1) 7)
                                (mod (+ i 0  1 1 1 1 1 1) 5)
                                (mod (+ i 0  1 1 1 1 1 1 1) 11)))
        :do (return i))

;; Test Case

(loop :for i :from 0 :upto 10
      :collect (list i
                     (mod (+ i 4 1) 5)
                     (mod (+ i 1 1 1) 2)))
