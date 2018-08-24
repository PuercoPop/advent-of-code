(defpackage #:aoc/11
  (:use #:cl #:split-sequence))
(in-package #:aoc/11)

(defclass point ()
  ((x :initarg :x :reader pos-x)
   (y :initarg :y :reader pos-y)
   (z :initarg :z :reader pos-z)))

(defmethod print-object ((obj point) stream)
  (print-unreadable-object (obj stream)
    (format stream "(@ ~A ~A ~A)"
            (pos-x obj)
            (pos-y obj)
            (pos-z obj))))

(defun @ (x y z)
  (make-instance 'point :x x :y y :z z))

(defun @+ (@1 @2)
  (@ (+ (pos-x @1)
        (pos-x @2))
     (+ (pos-y @1)
        (pos-y @2))
     (+ (pos-z @1)
        (pos-z @2))))

(defparameter +input+
  (with-open-file (in #P"11.input")
    (mapcar (lambda (x) (intern (string-upcase x)))
            (split-sequence #\, (read-line in)))))

(defun cube-distance (x y)
  (/ (+ (abs (+ (pos-x x)
                (pos-x y)))
        (abs (+ (pos-y x)
                (pos-y y)))
        (abs (+ (pos-z x)
                (pos-z y))))
     2))

(defparameter +directions+
  (let ((ret (make-hash-table)))
    (setf (gethash 'nw ret) (@ -1 0 1)
          (gethash 'n ret) (@ -1 1 0)
          (gethash 'ne ret) (@ 0 1 -1)
          (gethash 'se ret) (@ 1 0 -1)
          (gethash 's ret) (@ 1 -1 0)
          (gethash 'sw ret) (@ 0 -1 1))
    ret))

(defparameter +target+ (loop 
                         :for dir :in +input+
                         :for pos := (@ 0 0 0) :then (@+ pos (gethash dir +directions+))
                         ;; :maximizing (cube-distance (@ 0 0 0) pos)
                         :finally (return pos)))
