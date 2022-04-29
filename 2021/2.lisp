(defpackage #:day/2
  (:use #:cl))
(in-package #:day/2)

(defparameter +input+ #P"2.input")
(defparameter +example+ #P"2.example")

(defun parse-line (line)
  (let* ((pivot (position #\Space line))
         (direction (subseq line 0 pivot))
         (amount (subseq line (1+ pivot))))
    (values (intern (string-upcase direction)) (parse-integer amount))))

(defun part-1 (input)
  (let ((position #C(0 0)))
    (with-open-file (in input)
      (loop :for line := (read-line in nil nil)
            :while line
            :do (multiple-value-bind (direction amount)
                    (parse-line line)
                  (ecase direction
                    (:forward (incf position (complex amount 0)))
                    (:up (incf position (complex 0 amount)))
                    (:down (decf position (complex 0 amount)))))))
    (values position (* (abs (realpart position)) (abs (imagpart position))))))

(defclass submarine ()
  ((x :initform 0 :accessor sub-x)
   (y :initform 0 :accessor sub-y)
   (aim :initform 0 :accessor sub-aim)))

(defmethod print-object ((obj submarine) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "Horizontal: ~A. Depth: ~A. Aim: ~A"
            (sub-x obj)
            (sub-y obj)
            (sub-aim obj))))

(defmethod forward ((sub submarine) amount)
  (incf (sub-x sub) amount)
  (incf (sub-y sub) (* amount (sub-aim sub))))

(defmethod up ((sub submarine) amount)
  (decf (sub-aim sub) amount))

(defmethod down ((sub submarine) amount)
  (incf (sub-aim sub) amount))

(defun part-2 (input)
  (let ((sub (make-instance 'submarine)))
    (with-open-file (in input)
      (loop :for line := (read-line in nil nil)
            :while line
            :do (multiple-value-bind (instruction amount)
                    (parse-line line)
                  (funcall instruction sub amount))))
    (values sub (* (sub-x sub) (sub-y sub)))))
