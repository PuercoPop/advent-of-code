(defpackage "AOC/15"
  (:use "CL"))
(in-package "AOC/15")

(defparameter +a+ 618)
(defparameter +b+ 814)

(defun next (prev factor)
  (rem (* prev factor) 2147483647))

(defun ex (limit)
  (loop
    :with initial-a := 65
    :with initial-b := 8921
    :for times :from 0 :upto limit
    :for gen-a := (next initial-a 16807) :then (next gen-a 16807)
    :for gen-b := (next initial-b 48271) :then (next gen-b 48271)
    :when (= (ldb (byte 16 0) gen-a)
             (ldb (byte 16 0) gen-b))
      :sum 1))

(defun solve/1 (limit)
  (loop
    :with initial-a := +a+
    :with initial-b := +b+
    :for times :from 0 :upto limit
    :for gen-a := (next initial-a 16807) :then (next gen-a 16807)
    :for gen-b := (next initial-b 48271) :then (next gen-b 48271)
    :when (= (ldb (byte 16 0) gen-a)
             (ldb (byte 16 0) gen-b))
      :sum 1)) 

(defun solve/2 (limit initial-a initial-b)
  (let ((gen-a initial-a)
        (gen-b initial-b)
        (match-count 0)
        (success-count 0))
    (tagbody
     next-a
       (setf gen-a (next gen-a 16807))
       (unless (zerop (mod gen-a 4))
         (go next-a))

     next-b
       (setf gen-b (next gen-b 48271))
       (unless (zerop (mod gen-b 8))
         (go next-b))

     compare
       (incf match-count)
       (when (= (ldb (byte 16 0) gen-a)
                (ldb (byte 16 0) gen-b))
         (incf success-count))

       (unless (= match-count limit)
         (go next-a)))
    success-count)))
