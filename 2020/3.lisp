(defpackage "DAY/03"
  (:use "CL"))

(in-package "DAY/03")

(defparameter +input+ #P"3.input")
(defparameter +example+ #P"3.example")

(defparameter +map+ (let ((ret (make-array '(31 323))))
                      (with-open-file (in +input+)
                        (loop :for line := (read-line in nil)
                              :for y :from 0
                              :while line
                              :do (loop :for char :across line
                                        :for x :from 0
                                        :do (setf (aref ret x y) (char= #\# char)))))
                      ret))

(defparameter +example-map+ (let ((ret (make-array '(11 11))))
                      (with-open-file (in +example+)
                        (loop :for line := (read-line in nil)
                              :for y :from 0
                              :while line
                              :do (loop :for char :across line
                                        :for x :from 0
                                        :do (setf (aref ret x y) (char= #\# char)))))
                      ret))

(with-open-file (in +example+)
  (loop :for line := (read-line in nil)
        :while line
        :count t))

;; 11 chars example
;; 11 lines example

(defun treep (x y map)
  (destructuring-bind (width height)
      (array-dimensions map)
    (aref map
          (mod x width)
          (mod y height))))

(loop :for x :from 0 :by 3
      :for y :from 0 :by 1
      :repeat 11
      :count (treep x y +example-map+))

(loop :for x :from 0 :by 3
      :for y :from 0 :by 1
      :repeat 323
      :count (treep x y +map+))
;; 31 chars
 ;; 323 lines


(defun count-trees (x-stride y-stride)
  (loop :for x :from 0 :by x-stride
        :for y :from 0 :by y-stride
        :repeat (/ (second (array-dimensions +map+))
                   y-stride)
        :count (treep x y +map+)))

(* (count-trees 1 1)
   (count-trees 3 1)
   (count-trees 5 1)
   (count-trees 7 1)
   (count-trees 1 2))
