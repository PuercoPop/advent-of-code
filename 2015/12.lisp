(defpackage #:day/12
  (:use #:cl))
(in-package #:day/12)

(defparameter +input+ #P"12.input")
(with-open-file (in +input+)
  (yason:parse in))

(defun flatten (ht)
  (loop
    :for k :being :the :hash-keys :of  ht
    :for v :being :the :hash-values :of ht
    :collect k
    :collect v))

(defun walk (x)
  (cond ((stringp x) 0)
        ((numberp x) x)
        ((hash-table-p x) (reduce #'+ (mapcar #'walk (flatten x))))
        ((consp x) (reduce #'+ (mapcar #'walk x)))
        (t (error "Unexpected type of object.~%~A~%" x))))

(defun filter (ht)
  (let ((xs (loop
               :for k :being :the :hash-keys :of  ht
               :for v :being :the :hash-values :of ht
               :collect k
               :collect v)))
    (if (member "red" xs :test #'equalp)
        ()
        xs)))

(defun walk/2 (x)
  (cond ((stringp x) 0)
        ((numberp x) x)
        ((hash-table-p x) (reduce #'+ (mapcar #'walk/2 (filter x))))
        ((consp x) (reduce #'+ (mapcar #'walk/2 x)))
        (t (error "Unexpected type of object.~%~A~%" x))))
